module Language.Haskell.Interpreter.Unsafe (
    unsafeSetGhcOption, unsafeRunInterpreterWithArgs,
    unsafeInterpret
) where

import Control.Monad.Trans
import Control.Monad.Catch

import Hint.Base
import Hint.Eval
import Hint.Configuration
import Hint.InterpreterT

-- | Set a GHC option for the current session,
--   eg. @unsafeSetGhcOption \"-XNoMonomorphismRestriction\"@.
--
--   Warning: Some options may interact badly with the Interpreter.
unsafeSetGhcOption :: MonadInterpreter m => String -> m ()
unsafeSetGhcOption = setGhcOption

-- | Executes the interpreter, setting the args as though they were
--   command-line args.  In particular, this means args that have no
--   effect with :set in ghci might function properly from this
--   context.
--
--   Warning: Some options may interact badly with the Interpreter.
unsafeRunInterpreterWithArgs :: (MonadMask m, MonadIO m, Functor m)
                                => [String]
                                -> InterpreterT m a
                                -> m (Either InterpreterError a)
unsafeRunInterpreterWithArgs = runInterpreterWithArgs
