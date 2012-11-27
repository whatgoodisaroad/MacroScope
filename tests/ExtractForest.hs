import System.Environment
import MacroScope.Syntax
import MacroScope.Translator
import MacroScope.Properties

main = do 
    ps <- getArgs
    f <- parseFiles ps
    putStrLn $ showForest f
