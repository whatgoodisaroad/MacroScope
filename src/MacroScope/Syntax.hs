-- MacroScope Syntax
-------------------------------------------------------------------------------

module MacroScope.Syntax where

import Data.List

-- Datatypes
-------------------------------------------------------------------------------

type Macro = String

data Expr = Macro Macro
          | Expr :& Expr
          | Expr :| Expr
          | Not Expr

type Forest = [Tree]
data Tree = Tree Expr Forest Forest

instance Show Expr where
    show (Macro m)  = m
    show (e :& e')  = "(" ++ show e ++ " & " ++ show e' ++ ")"
    show (e :| e')  = "(" ++ show e ++ " | " ++ show e' ++ ")"
    show (Not e)    = "~(" ++ show e ++ ")"

showForest :: Forest -> String
showForest = concat . intersperse "\n" . map showTree

showTree :: Tree -> String
showTree = showTD 0
    where
        showTD :: Int -> Tree -> String
        showTD d (Tree e f f') = foldl1 (++) [
                pre,
                show e,
                "<[",
                sub f,
                "],[",
                sub f',
                "]>"
            ]
            where
                sub :: Forest -> String
                sub for = if null for
                    then ""
                    else foldl1 (++) [ 
                            "\n", 
                            showFD (succ d) for, 
                            "\n", 
                            pre 
                        ]

                pre = take (4 * d) $ repeat ' '
                showFD d = concat . intersperse "\n" . map (showTD d)




