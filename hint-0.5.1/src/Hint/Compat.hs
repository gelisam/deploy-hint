{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Hint.Compat where

import Control.Monad (foldM, liftM)

import qualified Hint.GHC as GHC

-- Kinds became a synonym for Type in GHC 6.8. We define this wrapper
-- to be able to define a FromGhcRep instance for both versions
newtype Kind = Kind GHC.Kind

-- supportedLanguages :: [String]
supportedExtensions = map f GHC.xFlags
    where
#if (__GLASGOW_HASKELL__ >= 710)
      f = GHC.flagSpecName
#else
      f (e,_,_) = e
#endif

setContext :: GHC.GhcMonad m => [GHC.Module] -> [GHC.ImportDecl GHC.RdrName] -> m ()
setContext ms ds =
  let ms' = map modToIIMod ms
      ds' = map GHC.IIDecl ds
      is = ms' ++ ds'
  in GHC.setContext is

getContext :: GHC.GhcMonad m => m ([GHC.Module], [GHC.ImportDecl GHC.RdrName])
getContext = GHC.getContext >>= foldM f ([], [])
  where
    f :: (GHC.GhcMonad m) =>
         ([GHC.Module], [GHC.ImportDecl GHC.RdrName]) ->
         GHC.InteractiveImport ->
         m ([GHC.Module], [GHC.ImportDecl GHC.RdrName])
    f (ns, ds) i = case i of
      (GHC.IIDecl d)     -> return (ns, d : ds)
      m@(GHC.IIModule _) -> do n <- iiModToMod m; return (n : ns, ds)

modToIIMod :: GHC.Module -> GHC.InteractiveImport
iiModToMod :: GHC.GhcMonad m => GHC.InteractiveImport -> m GHC.Module
modToIIMod = GHC.IIModule . GHC.moduleName
iiModToMod (GHC.IIModule m) = GHC.findModule m Nothing
iiModToMod _ = error "iiModToMod!"

-- Explicitly-typed variants of getContext/setContext, for use where we modify
-- or override the context.
setContextModules :: GHC.GhcMonad m => [GHC.Module] -> [GHC.Module] -> m ()
setContextModules as = setContext as . map (GHC.simpleImportDecl . GHC.moduleName)

getContextNames :: GHC.GhcMonad m => m([String], [String])
getContextNames = fmap (\(as,bs) -> (map name as, map decl bs)) getContext
    where name = GHC.moduleNameString . GHC.moduleName
          decl = GHC.moduleNameString . GHC.unLoc . GHC.ideclName

stringToStringBuffer = return . GHC.stringToStringBuffer

configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags = dflags{GHC.ghcMode    = GHC.CompManager,
                                  GHC.hscTarget  = GHC.HscInterpreted,
                                  GHC.ghcLink    = GHC.LinkInMemory,
                                  GHC.verbosity  = 0}

parseDynamicFlags :: GHC.GhcMonad m
                   => GHC.DynFlags -> [String] -> m (GHC.DynFlags, [String])
parseDynamicFlags d = fmap firstTwo . GHC.parseDynamicFlags d . map GHC.noLoc
    where firstTwo (a,b,_) = (a, map GHC.unLoc b)

fileTarget :: FilePath -> GHC.Target
fileTarget f = GHC.Target (GHC.TargetFile f $ Just next_phase) True Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile

-- add a bogus Maybe, in order to use it with mayFail
compileExpr :: GHC.GhcMonad m => String -> m (Maybe GHC.HValue)
compileExpr = fmap Just . GHC.compileExpr

-- add a bogus Maybe, in order to use it with mayFail
exprType :: GHC.GhcMonad m => String -> m (Maybe GHC.Type)
exprType = fmap Just . GHC.exprType

-- add a bogus Maybe, in order to use it with mayFail
typeKind :: GHC.GhcMonad m => String -> m (Maybe GHC.Kind)
typeKind = fmap Just . (liftM snd) . (GHC.typeKind True)

pprType :: GHC.Type -> GHC.SDoc
#if __GLASGOW_HASKELL__ < 708
pprType = GHC.pprTypeForUser False -- False means drop explicit foralls
#else
pprType = GHC.pprTypeForUser
#endif

mkLocMessage = GHC.mkLocMessage GHC.SevError
