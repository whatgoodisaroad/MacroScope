-- MacroScope Translator
-------------------------------------------------------------------------------

module MacroScope.Path where

import MacroScope.Syntax
import MacroScope.Properties

data PathStep = PL | PR | Elem Int deriving Show
type Path = [PathStep]

pathsOfUse :: Macro -> Forest -> [Path]
m `pathsOfUse` f = topLevel 0 f ++ deeperL 0 f ++ deeperR 0 f
    where
        topLevel :: Int -> Forest -> [Path]
        topLevel _ [] = []
        topLevel e (t:ts) = let k = topLevel (succ e) ts in
            if (occursInExpr m $ exprOfTree t) then [(Elem e)] : k else k

        deeperL, deeperR :: Int -> Forest -> [Path]
        deeperL _ [] = []
        deeperL e (t:ts) = 
                (map ([Elem e, PL] ++) $ m `pathsOfUse` (leftOfTree t)) 
            ++  deeperL (succ e) ts
        deeperR _ [] = []
        deeperR e (t:ts) = 
                map ([Elem e, PR] ++) $ m `pathsOfUse` (rightOfTree t) 
            ++  deeperR (succ e) ts
        


