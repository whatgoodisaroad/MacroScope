-- Project Euler 51
-- Wyatt Allen
---------------------------------------------------------------

import Data.List
import Data.Char
import Control.Monad


-- From: http://stackoverflow.com/q/4541415
-- Modified so that 1 is not prime
isPrime 1 = False
isPrime x = null $ filter divisible $ takeWhile notTooBig [2..] 
    where
        divisible y = x `mod` y == 0
        notTooBig y = y * y <= x

overwrite :: Int -> a -> [a] -> [a]
overwrite 0 a (_:as) = a:as
overwrite n a (_:as) = overwrite (pred n) a as

replace :: Int -> [Int] -> Int
replace = foldl overwrite

replacements :: Int -> [Int] -> [Int]
replacements n digits = do
    rep <- [0..9]

