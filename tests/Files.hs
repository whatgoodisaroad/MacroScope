module Files where

import System.FilePath.Find

cFiles :: FilePath -> IO [FilePath]
cFiles = find always (extension ==? ".c" ||? extension ==? ".h")
