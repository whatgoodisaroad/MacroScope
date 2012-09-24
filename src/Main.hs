
type Macro = String

data Expr = Macro Macro
		  | Expr :& Expr
		  | Expr :| Expr
		  | Not Expr

type Forest = [Tree]
data Tree = Tree Expr Forest Forest

-- Macro Occurrence
occursInForest :: Macro -> Forest -> Bool
m `occursIn` f = any occursInForest f

occursInTree :: Macro -> Tree -> Bool
m `occursInTree` (Tree e f1 f2) = or [ 
		occursInExpr e, 
		occursInForest f1, 
		occursInForest f2 
	]

occursInExpr :: Macro -> Expr -> Bool
m `occursInExpr` (Macro m') = m == m'
m `occursInExpr` (e :& e') = or $ map occursInExpr [e, e']
m `occursInExpr` (e :| e') = or $ map occursInExpr [e, e']
m `occursInExpr` (Not e) = occursInExpr e

occursIn :: Macro -> Forest -> Bool
occursIn = occursInForest


