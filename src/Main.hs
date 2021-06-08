import Control.Monad
import Control.Monad.Trans.Reader
import Text.Printf
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe


interpretDiag :: () -> Interpreter ((),())
interpretDiag u = do
    f <- interpret "\\x -> (x,x)" (as :: () -> ((),()))
    return (f u)

interpretId :: () -> Interpreter ()
interpretId u = do
    setImports ["Prelude"]
    f <- interpret "id" (as :: () -> ())
    return (f u)

interpretAsk :: () -> Interpreter ()
interpretAsk u = do
    setImports
      [ "Prelude"
      , "Data.Functor.Identity"
      , "Control.Monad.Trans.Reader"
      ]
    body <- interpret "ask" (as :: Reader () ())
    return (runReader body u)


libdir :: String
libdir = "/root/my-program/haskell-libs"


main :: IO ()
main = do
    putStrLn "please type '()':"
    u <- readLn
    
    r <- unsafeRunInterpreterWithArgsLibdir [] libdir (interpretDiag u)
    printf "(\\x -> (x,x)) %s is:\n" (show u)
    print r
    
    putStrLn "and now, let's try the Prelude..."
    r <- unsafeRunInterpreterWithArgsLibdir [] libdir (interpretId u)
    printf "id %s is:\n" (show u)
    print r
    
    putStrLn "and finally, a library from hackage."
    r <- unsafeRunInterpreterWithArgsLibdir [] libdir (interpretAsk u)
    printf "runReader ask %s is:\n" (show u)
    case r of
      Left err -> print err
      Right r -> do
        print r
