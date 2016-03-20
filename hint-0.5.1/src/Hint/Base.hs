module Hint.Base (
    MonadInterpreter(..), RunGhc,

    GhcError(..), InterpreterError(..), mayFail, catchIE,

    InterpreterSession, SessionData(..), GhcErrLogger,
    InterpreterState(..), fromState, onState,
    InterpreterConfiguration(..),

    runGhc1, runGhc2, runGhc3,

    ModuleName, PhantomModule(..),
    findModule, moduleIsLoaded,
    withDynFlags,

    ghcVersion,

    debug, showGHC
) where

import Control.Monad.Trans
import Control.Monad.Catch as MC

import Data.IORef
import Data.Dynamic

import qualified Hint.GHC as GHC

import Hint.Extension

-- | Version of the underlying ghc api. Values are:
--
-- * @708@ for GHC 7.8.x
--
-- * @710@ for GHC 7.10.x
--
-- * etc...
ghcVersion :: Int
ghcVersion = __GLASGOW_HASKELL__

class (MonadIO m, MonadMask m) => MonadInterpreter m where
    fromSession      :: FromSession m a
    modifySessionRef :: ModifySessionRef m a
    runGhc           :: RunGhc m a

-- this is for hiding the actual types in haddock
type FromSession      m a = (InterpreterSession -> a) -> m a
type ModifySessionRef m a = (InterpreterSession -> IORef a) -> (a -> a) -> m a

data InterpreterError = UnknownError String
                      | WontCompile [GhcError]
                      | NotAllowed  String
                      -- | GhcExceptions from the underlying GHC API are caught
                      -- and rethrown as this.
                      | GhcException String
                      deriving (Show, Typeable)

data InterpreterState = St {
                           activePhantoms    :: [PhantomModule],
                           zombiePhantoms    :: [PhantomModule],
                           hintSupportModule :: PhantomModule,
                           importQualHackMod :: Maybe PhantomModule,
                           qualImports       :: [(ModuleName, String)],
                           defaultExts       :: [(Extension,Bool)], -- R/O
                           configuration     :: InterpreterConfiguration
                        }

data InterpreterConfiguration = Conf {
                                  searchFilePath :: [FilePath],
                                  languageExts   :: [Extension],
                                  allModsInScope :: Bool
                                }

type InterpreterSession = SessionData ()

instance Exception InterpreterError

type RunGhc  m a =
    (forall n.(MonadIO n, MonadMask n,Functor n) => GHC.GhcT n a)
 -> m a

type RunGhc1 m a b =
    (forall n.(MonadIO n, MonadMask n, Functor n) => a -> GHC.GhcT n b)
 -> (a -> m b)

type RunGhc2 m a b c =
    (forall n.(MonadIO n, MonadMask n, Functor n) => a -> b -> GHC.GhcT n c)
 -> (a -> b -> m c)

type RunGhc3 m a b c d =
    (forall n.(MonadIO n, MonadMask n, Functor n) => a -> b -> c -> GHC.GhcT n d)
 -> (a -> b -> c -> m d)

data SessionData a = SessionData {
                       internalState   :: IORef InterpreterState,
                       versionSpecific :: a,
                       ghcErrListRef   :: IORef [GhcError],
                       ghcErrLogger    :: GhcErrLogger
                     }

-- When intercepting errors reported by GHC, we only get a ErrUtils.Message
-- and a SrcLoc.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
newtype GhcError = GhcError{errMsg :: String} deriving Show

mapGhcExceptions :: MonadInterpreter m
                 => (String -> InterpreterError)
                 -> m a
                 -> m a
mapGhcExceptions buildEx action =
    action
      `MC.catch` (\err -> case err of
                            GhcException s -> throwM (buildEx s)
                            _              -> throwM err)

catchIE :: MonadInterpreter m => m a -> (InterpreterError -> m a) -> m a
catchIE = MC.catch

type GhcErrLogger = GHC.LogAction

-- | Module names are _not_ filepaths.
type ModuleName = String

runGhc1 :: MonadInterpreter m => RunGhc1 m a b
runGhc1 f a = runGhc (f a)

runGhc2 :: MonadInterpreter m => RunGhc2 m a b c
runGhc2 f a = runGhc1 (f a)

runGhc3 :: MonadInterpreter m => RunGhc3 m a b c d
runGhc3 f a = runGhc2 (f a)

-- ================ Handling the interpreter state =================

fromState :: MonadInterpreter m => (InterpreterState -> a) -> m a
fromState f = do ref_st <- fromSession internalState
                 liftIO $ f `fmap` readIORef ref_st

onState :: MonadInterpreter m => (InterpreterState -> InterpreterState) -> m ()
onState f = modifySessionRef internalState f >> return ()

-- =============== Error handling ==============================

mayFail :: MonadInterpreter m => m (Maybe a) -> m a
mayFail action =
    do
        maybe_res <- action
        --
        es <- modifySessionRef ghcErrListRef (const [])
        --
        case (maybe_res, null es) of
            (Nothing,True)  -> throwM $ UnknownError "Got no error message"
            (Nothing,False) -> throwM $ WontCompile (reverse es)
            (Just a, True)  -> return a
            (Just _, False) -> fail $ "GHC returned a result but said: " ++
                                      show es

-- ================= Debugging stuff ===============

debug :: MonadInterpreter m => String -> m ()
debug = liftIO . putStrLn . ("!! " ++)

showGHC :: (MonadInterpreter m, GHC.Outputable a) => a -> m String
showGHC a
 = do unqual <- runGhc GHC.getPrintUnqual
      withDynFlags $ \df ->
        return $ GHC.showSDocForUser df unqual (GHC.ppr a)

-- ================ Misc ===================================

-- this type ought to go in Hint.Context, but ghc dislikes cyclic imports...
data PhantomModule = PhantomModule{pmName :: ModuleName, pmFile :: FilePath}
                   deriving (Eq, Show)

findModule :: MonadInterpreter m => ModuleName -> m GHC.Module
findModule mn = mapGhcExceptions NotAllowed $
                    runGhc2 GHC.findModule mod_name Nothing
    where mod_name = GHC.mkModuleName mn

moduleIsLoaded :: MonadInterpreter m => ModuleName -> m Bool
moduleIsLoaded mn = (findModule mn >> return True)
                   `catchIE` (\e -> case e of
                                      NotAllowed{} -> return False
                                      _            -> throwM e)

withDynFlags :: MonadInterpreter m => (GHC.DynFlags -> m a) -> m a
withDynFlags action
 = do df <- runGhc GHC.getSessionDynFlags
      action df
