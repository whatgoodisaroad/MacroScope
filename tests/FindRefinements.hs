import System.IO.Unsafe
import System.Environment
import System.FilePath.Find

import MacroScope.Syntax
import MacroScope.Translator
import MacroScope.Properties


cFiles :: FilePath -> IO [FilePath]
cFiles = find always (extension ==? ".c" ||? extension ==? ".h")

printRefinement :: (String, String) -> IO ()
printRefinement (c, p) = putStrLn $ c ++ " refines " ++ p

parsePair :: FilePath -> IO (FilePath, Forest)
parsePair p = parseFiles [p] >>= \f -> return (p, f)

printPair :: (FilePath, Forest) -> IO ()
printPair (p, f) = do
    putStr p
    putStr "\t\t"
    putStr $ show $ length f
    putStr "\n"

--  From http://www.haskell.org/pipermail/haskell-cafe/2006-October/019033.html
unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
    y <- f x
    ys <- unsafeInterleaveMapIO f xs
    return (y : ys)
unsafeInterleaveMapIO _ [] = return []

main = do
    --path <- fmap (!! 0) getArgs
    --ps <- cFiles path
    --f <- fmap concat $ unsafeInterleaveMapIO (parseFiles . return) ps

    let path = "/Users/allenwy/macrosurvey/linux/kernel"

    f <- cFiles path >>= parseFiles
    mapM_ printRefinement $ findRefinements $ f
    --pairs <- unsafeInterleaveMapIO parsePair ps
    --mapM_ printPair pairs
