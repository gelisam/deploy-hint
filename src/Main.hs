import Control.Monad
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

interpretDon'tReturn :: () -> Interpreter (IO ())
interpretDon'tReturn u = do
    setImports ["Prelude","Acme.Dont"]
    don't <- interpret "don't" (as :: IO () -> IO ())
    return (don't (return u))


runInterpreterWithPackageDb :: Interpreter a -> IO (Either InterpreterError a)
runInterpreterWithPackageDb = unsafeRunInterpreterWithArgs "/root/my-program/haskell-core-libs" args
  where
    args = [ "-package-db /root/my-program/haskell-core-libs/package.conf.d"
           , "-package-db /root/my-program/haskell-libs/x86_64-linux-ghc-7.10.3-packages.conf.d"
           ]


main :: IO ()
main = do
    putStrLn "please type '()':"
    u <- readLn
    
    r <- runInterpreter "/root/my-program/haskell-core-libs" (interpretDiag u)
    printf "(\\x -> (x,x)) %s is:\n" (show u)
    print r
    
    putStrLn "and now, let's try the Prelude..."
    r <- runInterpreter "/root/my-program/haskell-core-libs" (interpretId u)
    printf "id %s is:\n" (show u)
    print r
    
    putStrLn "and finally, a library from hackage."
    r <- runInterpreterWithPackageDb (interpretDon'tReturn u)
    printf "don't (return %s) is:\n" (show u)
    case r of
      Left err -> print err
      Right body -> do
        r <- body
        print r
