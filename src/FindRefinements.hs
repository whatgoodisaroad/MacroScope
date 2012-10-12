import System.Environment
import MacroScope.Translator
import MacroScope.Properties

printRefinement :: (String, String) -> IO ()
printRefinement (c, p) = putStrLn $ c ++ " refines " ++ p

main = do 
    ps <- getArgs
    f <- parseFiles ps
    mapM_ printRefinement $ refinements_naive f
