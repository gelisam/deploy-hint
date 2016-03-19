import Text.Printf
import Language.Haskell.Interpreter


interpretDiag :: () -> Interpreter ((),())
interpretDiag u = do
    f <- interpret "\\x -> (x,x)" (as :: () -> ((),()))
    return (f u)

interpretId :: () -> Interpreter ()
interpretId u = do
    setImports ["Prelude"]
    f <- interpret "id" (as :: () -> ())
    return (f u)

main :: IO ()
main = do
    putStrLn "please type '()':"
    u <- readLn
    
    r <- runInterpreter (interpretDiag u)
    printf "(\\x -> (x,x)) %s is:\n" (show u)
    print r
    
    putStrLn "and now, let's try the Prelude..."
    r <- runInterpreter (interpretId u)
    printf "id %s is:\n" (show u)
    print r
