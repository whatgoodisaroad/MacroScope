import System.Environment

import MacroScope.Syntax
import MacroScope.Properties
import MacroScope.Translator

classes :: Forest -> (Float, Float, Float)
classes f = (scale mutex, scale refinements, scale weakRefinements)
    where
        macroSpace = (length $ macrosOfForest f) ^ 2
        mutex = findMutexes f
        refinements = findRefinements f
        weakRefinements = findWeakRefinements f
        scale = \pairs -> 
            100.0 * (fromIntegral $ length pairs) / (fromIntegral macroSpace)

showClasses :: (Float, Float, Float) -> String
showClasses (m, r, w) = 
    "Mutually exclusive: " ++ show m ++ "%\n" ++
    "Refinements: " ++ show r ++ "%\n" ++
    "Weak refinements: " ++ show r ++ "%\n"

main = do 
    ps <- getArgs
    f <- parseFiles ps
    putStrLn $ showClasses $ classes f
