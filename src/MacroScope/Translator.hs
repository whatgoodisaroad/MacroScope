-- MacroScope Translator
-------------------------------------------------------------------------------

module MacroScope.Tranlsator where

import Language.CPP.Syntax
import Language.CPP.Parser

import qualified Analysis.Conditional as AC
import Analysis.Translator

import qualified MacroScope.Syntax as MS
import qualified MacroScope.Properties as MP

translate' :: [File ()] -> ([AC.Cond AC.BExpr (StoreAs ())], [CExpr])
translate' = convert . extract

translate :: File () -> [MS.Forest]
translate f = let (cs, _) = translate' [f] in [trans cs]

condToTree :: AC.Cond AC.BExpr (StoreAs ()) -> MS.Tree
condToTree (AC.IfThen be cs) = MS.Tree (bexprToExpr be) (trans cs) []
condToTree (AC.IfThenElse be cs cs') = MS.Tree (bexprToExpr be) (trans cs) (trans cs')

isLeaf :: AC.Cond c a -> Bool
isLeaf (AC.Leaf _) = True
isLeaf _ = False

trans = map condToTree . filter (not . isLeaf)

bexprToExpr :: AC.BExpr -> MS.Expr
bexprToExpr (AC.Con c) = MS.Macro (" *" ++ show c ++ "* ")
bexprToExpr (AC.Var m) = MS.Macro m
bexprToExpr (AC.Not e) = MS.Not $ bexprToExpr e
bexprToExpr (AC.And e e') = (MS.:&) (bexprToExpr e) (bexprToExpr e')
bexprToExpr (AC.Or e e') = (MS.:|) (bexprToExpr e) (bexprToExpr e')
