{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2009.
--
-- Package management tool
--
-----------------------------------------------------------------------------

module Main (main) where

import Version ( version, targetOS, targetARCH )
import qualified GHC.PackageDb as GhcPkg
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)
import Distribution.InstalledPackageInfo as Cabal
import Distribution.Compat.ReadP hiding (get)
import Distribution.ParseUtils
import Distribution.Package hiding (depends, installedPackageId)
import Distribution.Text
import Distribution.Version
import Distribution.Simple.Utils (fromUTF8, toUTF8)
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing,
                          getModificationTime )


import System.Console.GetOpt
import qualified Control.Exception as Exception

import Data.Char ( isSpace, toLower )
import Control.Monad
import System.Directory ( doesDirectoryExist, getDirectoryContents,
                          doesFileExist, renameFile, removeFile,
                          getCurrentDirectory )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO
import System.IO.Error
import GHC.IO.Exception (IOErrorType(InappropriateType))
import Data.List
import Control.Concurrent

import qualified Data.ByteString.Char8 as BS

import System.Posix hiding (fdToHandle)

-- -----------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  [globalPackageDb] <- getArgs
  runit globalPackageDb

-- -----------------------------------------------------------------------------
-- Do the business

-- | Represents how a package may be specified by a user on the command line.
data PackageArg
    -- | A package identifier foo-0.1; the version might be a glob.
    = Id PackageIdentifier
    -- | An installed package ID foo-0.1-HASH.  This is guaranteed to uniquely
    -- match a single entry in the package database.
    | IPId InstalledPackageId
    -- | A package key foo_HASH.  This is also guaranteed to uniquely match
    -- a single entry in the package database
    | PkgKey PackageKey
    -- | A glob against the package name.  The first string is the literal
    -- glob, the second is a function which returns @True@ if the the argument
    -- matches.
    | Substring String (String->Bool)

runit :: FilePath -> IO ()
runit globalPackageDb = do
  installSignalHandlers -- catch ^C and clean up
  prog <- getProgramName
  let
        auto_ghci_libs = False
        multi_instance = False
        expand_env_vars= False
        mexpand_pkgroot= Nothing
                
        splitFields fields = unfoldr splitComma (',':fields)
          where splitComma "" = Nothing
                splitComma fs = Just $ break (==',') (tail fs)

        -- | Parses a glob into a predicate which tests if a string matches
        -- the glob.  Returns Nothing if the string in question is not a glob.
        -- At the moment, we only support globs at the beginning and/or end of
        -- strings.  This function respects case sensitivity.
        --
        -- >>> fromJust (substringCheck "*") "anything"
        -- True
        --
        -- >>> fromJust (substringCheck "string") "string"
        -- True
        --
        -- >>> fromJust (substringCheck "*bar") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "foo*") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "*ooba*") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "f*bar") "foobar"
        -- False
        substringCheck :: String -> Maybe (String -> Bool)
        substringCheck ""    = Nothing
        substringCheck "*"   = Just (const True)
        substringCheck [_]   = Nothing
        substringCheck (h:t) =
          case (h, init t, last t) of
            ('*',s,'*') -> Just (isInfixOf (f s) . f)
            ('*',_, _ ) -> Just (isSuffixOf (f t) . f)
            ( _ ,s,'*') -> Just (isPrefixOf (f (h:s)) . f)
            _           -> Nothing
          where f = id
  --
  -- first, parse the command
  recache globalPackageDb

parseCheck :: ReadP a a -> String -> String -> IO a
parseCheck parser str what =
  case [ x | (x,ys) <- readP_to_S parser str, all isSpace ys ] of
    [x] -> return x
    _ -> die ("cannot parse \'" ++ str ++ "\' as a " ++ what)

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide, trust, distrust
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

data PackageDB 
  = PackageDB {
      location, locationAbsolute :: !FilePath,
      -- We need both possibly-relative and definately-absolute package
      -- db locations. This is because the relative location is used as
      -- an identifier for the db, so it is important we do not modify it.
      -- On the other hand we need the absolute path in a few places
      -- particularly in relation to the ${pkgroot} stuff.
      
      packages :: [InstalledPackageInfo]
    }

type PackageDBStack = [PackageDB]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

getPkgDatabases :: Bool    -- we are modifying, not reading
                -> Bool    -- use the user db
                -> Bool    -- read caches, if available
                -> Bool    -- expand vars, like ${pkgroot} and $topdir
                -> FilePath
                -> IO (PackageDBStack, 
                          -- the real package DB stack: [global,user] ++ 
                          -- DBs specified on the command line with -f.
                       Maybe FilePath,
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases modify use_user use_cache expand_vars globalPackageDb = do
  -- first we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let err_msg = "missing --global-package-db option, location of global package database unknown\n"
  let global_conf = globalPackageDb

  -- The value of the $topdir variable used in some package descriptions
  -- Note that the way we calculate this is slightly different to how it
  -- is done in ghc itself. We rely on the convention that the global
  -- package db lives in ghc's libdir.
  top_dir <- absolutePath (takeDirectory global_conf)

  -- get the location of the user package database, and create it if necessary
  -- getAppUserDataDirectory can fail (e.g. if $HOME isn't set)
  e_appdir <- tryIO $ getAppUserDataDirectory "ghc"

  mb_user_conf <-
    case e_appdir of
        Left _    -> return Nothing
        Right appdir -> do
          let subdir = targetARCH ++ '-':targetOS ++ '-':Version.version
              dir = appdir </> subdir
          r <- lookForPackageDBIn dir
          case r of
            Nothing -> return (Just (dir </> "package.conf.d", False))
            Just f  -> return (Just (f, True))

  -- If the user database exists, and for "use_user" commands (which includes
  -- "ghc-pkg check" and all commands that modify the db) we will attempt to
  -- use the user db.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          use_user || user_exists = [user_conf, global_conf]
        | otherwise               = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | not (null path) && isSearchPathSeparator (last path)
                  -> splitSearchPath (init path) ++ sys_databases
                  | otherwise
                  -> splitSearchPath path

        -- The "global" database is always the one at the bottom of the stack.
        -- This is the database we modify by default.
      virt_global_conf = last env_stack

  let db_flags = [virt_global_conf]

  let flag_db_names = reverse (nub db_flags)

  -- For a "modify" command, treat all the databases as
  -- a stack, where we are modifying the top one, but it
  -- can refer to packages in databases further down the
  -- stack.

  -- -f flags on the command line add to the database
  -- stack, unless any of them are present in the stack
  -- already.
  let final_stack = env_stack

  -- the database we actually modify is the one mentioned
  -- rightmost on the command-line.
  let to_modify
        | not modify    = Nothing
        | null db_flags = Just virt_global_conf
        | otherwise     = Just (last db_flags)

  db_stack  <- sequence
    [ do db <- readParseDatabase mb_user_conf modify use_cache db_path
         if expand_vars then return (mungePackageDBPaths top_dir db)
                        else return db
    | db_path <- final_stack ]

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  return (db_stack, to_modify, flag_db_stack)


lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
    let path_file = dir </> "package.conf"
    exists_file <- doesFileExist path_file
    if exists_file then return (Just path_file) else return Nothing

readParseDatabase :: Maybe (FilePath,Bool)
                  -> Bool -- we will be modifying, not just reading
                  -> Bool -- use cache
                  -> FilePath
                  -> IO PackageDB

readParseDatabase mb_user_conf modify use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = mkPackageDB []
  | otherwise
  = do e <- tryIO $ getDirectoryContents path
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType ->
              die ("ghc no longer supports single-file style package databases "
                ++ "(" ++ path ++ ") use 'ghc-pkg init' to create the database "
                ++ "with the correct format.")
           | otherwise -> ioError err
         Right fs
           | not use_cache -> ignore_cache (const $ return ())
           | otherwise -> do
              let cache = path </> cachefilename
              tdir     <- getModificationTime path
              e_tcache <- tryIO $ getModificationTime cache
              case e_tcache of
                Left ex -> do
                  whenReportCacheErrors $
                    if isDoesNotExistError ex
                      then do
                        warn ("WARNING: cache does not exist: " ++ cache)
                        warn ("ghc will fail to read this package db. " ++
                              "Use 'ghc-pkg recache' to fix.")
                      else do
                        warn ("WARNING: cache cannot be read: " ++ show ex)
                        warn "ghc will fail to read this package db."
                  ignore_cache (const $ return ())
                Right tcache -> do
                  let compareTimestampToCache file =
                          return ()
                  if tcache >= tdir
                      then do
                          pkgs <- GhcPkg.readPackageDbForGhcPkg cache
                          mkPackageDB pkgs
                      else do
                          whenReportCacheErrors $ do
                              warn ("WARNING: cache is out of date: " ++ cache)
                              warn ("ghc will see an old view of this " ++
                                    "package db. Use 'ghc-pkg recache' to fix.")
                          ignore_cache compareTimestampToCache
            where
                 ignore_cache :: (FilePath -> IO ()) -> IO PackageDB
                 ignore_cache checkTime = do
                     let confs = filter (".conf" `isSuffixOf`) fs
                         doFile f = do checkTime f
                                       parseSingletonPackageConf f
                     pkgs <- mapM doFile $ map (path </>) confs
                     mkPackageDB pkgs

                 -- We normally report cache errors for read-only commands,
                 -- since modify commands because will usually fix the cache.
                 whenReportCacheErrors =
                     when (not modify)
  where
    mkPackageDB pkgs = do
      path_abs <- absolutePath path
      return PackageDB {
        location = path,
        locationAbsolute = path_abs,
        packages = pkgs
      }

parseSingletonPackageConf :: FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf file = do
  readUTF8File file >>= fmap fst . parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

mungePackageDBPaths :: FilePath -> PackageDB -> PackageDB
mungePackageDBPaths top_dir db@PackageDB { packages = pkgs } =
    db { packages = map (mungePackagePaths top_dir pkgroot) pkgs }
  where
    pkgroot = takeDirectory (locationAbsolute db)    
    -- It so happens that for both styles of package db ("package.conf"
    -- files and "package.conf.d" dirs) the pkgroot is the parent directory
    -- ${pkgroot}/package.conf  or  ${pkgroot}/package.conf.d/

-- TODO: This code is duplicated in compiler/main/Packages.lhs
mungePackagePaths :: FilePath -> FilePath
                  -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths top_dir pkgroot pkg =
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
                     -- haddock-html is allowed to be either a URL or a file
      haddockHTMLs = munge_paths (munge_urls (haddockHTMLs pkg))
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url

    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing


-- -----------------------------------------------------------------------------
-- Registering

parsePackageInfo
        :: String
        -> IO (InstalledPackageInfo, [ValidateWarning])
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    ParseOk warnings ok -> return (mungePackageInfo ok, ws)
      where
        ws = [ msg | PWarning msg <- warnings
                   , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
    ParseFailed err -> case locatedErrorMsg err of
                           (Nothing, s) -> die s
                           (Just l, s) -> die (show l ++ ": " ++ s)

mungePackageInfo :: InstalledPackageInfo -> InstalledPackageInfo
mungePackageInfo ipi = ipi { packageKey = packageKey' }
  where
    packageKey'
      | OldPackageKey (PackageIdentifier (PackageName "") _) <- packageKey ipi
          = OldPackageKey (sourcePackageId ipi)
      | otherwise = packageKey ipi

-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeDB :: [DBOp] -> PackageDB -> IO ()
changeDB cmds db = do
  let db' = updateInternalDB db cmds
  createDirectoryIfMissing True (location db)
  changeDBDir cmds db'

updateInternalDB :: PackageDB -> [DBOp] -> PackageDB
updateInternalDB db cmds = db{ packages = foldl do_cmd (packages db) cmds }
 where
  do_cmd pkgs (RemovePackage p) = 
    filter ((/= installedPackageId p) . installedPackageId) pkgs
  do_cmd pkgs (AddPackage p) = p : pkgs
  do_cmd pkgs (ModifyPackage p) = 
    do_cmd (do_cmd pkgs (RemovePackage p)) (AddPackage p)
    

changeDBDir :: [DBOp] -> PackageDB -> IO ()
changeDBDir cmds db = do
  mapM_ do_cmd cmds
  updateDBCache db
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    removeFileSafe file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    writeFileUtf8Atomic file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) = 
    do_cmd (AddPackage p)

updateDBCache :: PackageDB -> IO ()
updateDBCache db = do
  let filename = location db </> cachefilename

      pkgsCabalFormat :: [InstalledPackageInfo]
      pkgsCabalFormat = packages db

      pkgsGhcCacheFormat :: [PackageCacheFormat]
      pkgsGhcCacheFormat = map convertPackageInfoToCacheFormat pkgsCabalFormat

  GhcPkg.writePackageDb filename pkgsGhcCacheFormat pkgsCabalFormat
    `catchIO` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e
  -- See Note [writeAtomic leaky abstraction]
  -- Cross-platform "touch". This only works if filename is not empty, and not
  -- open for writing already.
  -- TODO. When the Win32 or directory packages have either a touchFile or a
  -- setModificationTime function, use one of those.
  withBinaryFile filename ReadWriteMode $ \handle -> do
      c <- hGetChar handle
      hSeek handle AbsoluteSeek 0
      hPutChar handle c

type PackageCacheFormat = GhcPkg.InstalledPackageInfo
                            String     -- installed package id
                            String     -- src package id
                            String     -- package name
                            String     -- package key
                            ModuleName -- module name

convertPackageInfoToCacheFormat :: InstalledPackageInfo -> PackageCacheFormat
convertPackageInfoToCacheFormat pkg =
    GhcPkg.InstalledPackageInfo {
       GhcPkg.installedPackageId = display (installedPackageId pkg),
       GhcPkg.sourcePackageId    = display (sourcePackageId pkg),
       GhcPkg.packageName        = display (packageName pkg),
       GhcPkg.packageVersion     = packageVersion pkg,
       GhcPkg.packageKey         = display (packageKey pkg),
       GhcPkg.depends            = map display (depends pkg),
       GhcPkg.importDirs         = importDirs pkg,
       GhcPkg.hsLibraries        = hsLibraries pkg,
       GhcPkg.extraLibraries     = extraLibraries pkg,
       GhcPkg.extraGHCiLibraries = extraGHCiLibraries pkg,
       GhcPkg.libraryDirs        = libraryDirs pkg,
       GhcPkg.frameworks         = frameworks pkg,
       GhcPkg.frameworkDirs      = frameworkDirs pkg,
       GhcPkg.ldOptions          = ldOptions pkg,
       GhcPkg.ccOptions          = ccOptions pkg,
       GhcPkg.includes           = includes pkg,
       GhcPkg.includeDirs        = includeDirs pkg,
       GhcPkg.haddockInterfaces  = haddockInterfaces pkg,
       GhcPkg.haddockHTMLs       = haddockHTMLs pkg,
       GhcPkg.exposedModules     = map convertExposed (exposedModules pkg),
       GhcPkg.hiddenModules      = hiddenModules pkg,
       GhcPkg.instantiatedWith   = map convertInst (instantiatedWith pkg),
       GhcPkg.exposed            = exposed pkg,
       GhcPkg.trusted            = trusted pkg
    }
  where convertExposed (ExposedModule n reexport sig) =
            GhcPkg.ExposedModule n (fmap convertOriginal reexport)
                                   (fmap convertOriginal sig)
        convertOriginal (OriginalModule ipid m) =
            GhcPkg.OriginalModule (display ipid) m
        convertInst (m, o) = (m, convertOriginal o)

instance GhcPkg.BinaryStringRep ModuleName where
  fromStringRep = ModuleName.fromString . fromUTF8 . BS.unpack
  toStringRep   = BS.pack . toUTF8 . display

instance GhcPkg.BinaryStringRep String where
  fromStringRep = fromUTF8 . BS.unpack
  toStringRep   = BS.pack . toUTF8


-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar

recache :: FilePath -> IO ()
recache globalPackageDb = do
  (db_stack, Just to_modify, _flag_dbs) <- 
     getPkgDatabases True{-modify-} True{-use user-} False{-no cache-}
                     False{-expand vars-} globalPackageDb
  let
        db_to_operate_on = my_head "recache" $
                           filter ((== to_modify).location) db_stack
  --
  changeDB [] db_to_operate_on

-- -----------------------------------------------------------------------------
-- Listing packages

simplePackageList :: FilePath -> [InstalledPackageInfo] -> IO ()
simplePackageList globalPackageDb pkgs = do
   let showPkg = display
       -- Sort using instance Ord PackageId
       strs = map showPkg $ sort $ map sourcePackageId pkgs
   when (not (null pkgs)) $
      hPutStrLn stdout $ concat $ intersperse " " strs

-- -----------------------------------------------------------------------------
-- Describe

-- PackageId is can have globVersion for the version
findPackages :: PackageDBStack -> PackageArg -> IO [InstalledPackageInfo]
findPackages db_stack pkgarg
  = fmap (concatMap snd) $ findPackagesByDB db_stack pkgarg

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(PackageDB, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db <- db_stack,
           let matched = filter (pkgarg `matchesPkg`) (packages db),
           not (null matched) ] of
        [] -> die ("cannot find package " ++ pkg_msg pkgarg)
        ps -> return ps
  where
        pkg_msg (Id pkgid)           = display pkgid
        pkg_msg (PkgKey pk)          = display pk
        pkg_msg (IPId ipid)          = display ipid
        pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: PackageIdentifier -> PackageIdentifier -> Bool
pid `matches` pid'
  = (pkgName pid == pkgName pid')
    && (pkgVersion pid == pkgVersion pid' || not (realVersion pid))

realVersion :: PackageIdentifier -> Bool
realVersion pkgid = versionBranch (pkgVersion pkgid) /= []
  -- when versionBranch == [], this is a glob

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` sourcePackageId pkg
(PkgKey pk)     `matchesPkg` pkg = pk == packageKey pkg
(IPId ipid)     `matchesPkg` pkg = ipid == installedPackageId pkg
(Substring _ m) `matchesPkg` pkg = m (display (sourcePackageId pkg))

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateWarning = String

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

die :: String -> IO a
die = dieWith 1

dieWith :: Int -> String -> IO a
dieWith ec s = do
  prog <- getProgramName
  reportError (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

warn :: String -> IO ()
warn = reportError

-- send info messages to stdout
infoLn :: String -> IO ()
infoLn = putStrLn

reportError :: String -> IO ()
reportError s = do hFlush stdout; hPutStrLn stderr s

my_head :: String -> [a] -> a
my_head s []      = error s
my_head _ (x : _) = x

-----------------------------------------
-- Cut and pasted from ghc/compiler/main/SysTools

getLibDir :: IO (Maybe String)
getLibDir = return Nothing

-----------------------------------------
-- Adapted from ghc/compiler/utils/Panic

installSignalHandlers :: IO ()
installSignalHandlers = do
  threadid <- myThreadId
  let
      interrupt = Exception.throwTo threadid
                                    (Exception.ErrorCall "interrupted")
  --
  _ <- installHandler sigQUIT (Catch interrupt) Nothing
  _ <- installHandler sigINT  (Catch interrupt) Nothing
  return ()

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

writeFileUtf8Atomic :: FilePath -> String -> IO ()
writeFileUtf8Atomic targetFile content =
  withFileAtomic targetFile $ \h -> do
     hSetEncoding h utf8
     hPutStr h content

-- copied from Cabal's Distribution.Simple.Utils, except that we want
-- to use text files here, rather than binary files.
withFileAtomic :: FilePath -> (Handle -> IO ()) -> IO ()
withFileAtomic targetFile write_content = do
  (newFile, newHandle) <- openNewFile targetDir template
  do  write_content newHandle
      hClose newHandle
      renameFile newFile targetFile
   `Exception.onException` do hClose newHandle
                              removeFileSafe newFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = "."
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

openNewFile :: FilePath -> String -> IO (FilePath, Handle)
openNewFile dir template = do
  -- this was added to System.IO in 6.12.1
  -- we must use this version because the version below opens the file
  -- in binary mode.
  openTempFileWithDefaultPermissions dir template

readUTF8File :: FilePath -> IO String
readUTF8File file = do
  h <- openFile file ReadMode
  -- fix the encoding to UTF-8
  hSetEncoding h utf8
  hGetContents h

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: FilePath -> IO ()
removeFileSafe fn =
  removeFile fn `catchIO` \ e ->
    when (not $ isDoesNotExistError e) $ ioError e

absolutePath :: FilePath -> IO FilePath
absolutePath path = return . normalise . (</> path) =<< getCurrentDirectory


{- Note [writeAtomic leaky abstraction]
GhcPkg.writePackageDb calls writeAtomic, which first writes to a temp file,
and then moves the tempfile to its final destination. This all happens in the
same directory (package.conf.d).
Moving a file doesn't change its modification time, but it *does* change the
modification time of the directory it is placed in. Since we compare the
modification time of the cache file to that of the directory it is in to
decide whether the cache is out-of-date, it will be instantly out-of-date
after creation, if the renaming takes longer than the smallest time difference
that the getModificationTime can measure.

The solution we opt for is a "touch" of the cache file right after it is
created. This resets the modification time of the cache file and the directory
to the current time.

Other possible solutions:
  * backdate the modification time of the directory to the modification time
    of the cachefile. This is what we used to do on posix platforms. An
    observer of the directory would see the modification time of the directory
    jump back in time. Not nice, although in practice probably not a problem.
    Also note that a cross-platform implementation of setModificationTime is
    currently not available.
  * set the modification time of the cache file to the modification time of
    the directory (instead of the curent time). This could also work,
    given that we are the only ones writing to this directory. It would also
    require a high-precision getModificationTime (lower precision times get
    rounded down it seems), or the cache would still be out-of-date.
  * change writeAtomic to create the tempfile outside of the target file's
    directory.
  * create the cachefile outside of the package.conf.d directory in the first
    place. But there are tests and there might be tools that currently rely on
    the package.conf.d/package.cache format.
-}
