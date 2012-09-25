-- MacroScope
-------------------------------------------------------------------------------

module MacroScope where

import Data.List (nub, intersect)


-- Datatypes
-------------------------------------------------------------------------------

type Macro = String

data Expr = Macro Macro
		  | Expr :& Expr
		  | Expr :| Expr
		  | Not Expr

type Forest = [Tree]
data Tree = Tree Expr Forest Forest


-- Expression Semantics
-------------------------------------------------------------------------------

evalExpr :: [Macro] -> Expr -> Bool
evalExpr ms (Macro m) = m `elem` ms
evalExpr ms (e :& e') = evalExpr ms e && evalExpr ms e'
evalExpr ms (e :| e') = evalExpr ms e || evalExpr ms e'
evalExpr ms (Not e) = not $ evalExpr ms e


-- Macro Occurrence
-------------------------------------------------------------------------------

occursInForest :: Macro -> Forest -> Bool
m `occursInForest` fs = any (occursInTree m) fs

occursInTree :: Macro -> Tree -> Bool
m `occursInTree` (Tree e f1 f2) = or [ 
		(occursInExpr m) e, 
		(occursInForest m) f1, 
		(occursInForest m) f2 
	]

occursInExpr :: Macro -> Expr -> Bool
m `occursInExpr` (Macro m') = m == m'
m `occursInExpr` (e :& e') = or $ map (occursInExpr m) [e, e']
m `occursInExpr` (e :| e') = or $ map (occursInExpr m) [e, e']
m `occursInExpr` (Not e) = (occursInExpr m) e


-- Macro Descendance
-------------------------------------------------------------------------------

descendantOf, leftDescendantOf, rightDescendentOf 
	:: Macro -> Macro -> Forest -> Bool

descendantOf child parent forest = 
	   leftDescendantOf child parent forest 
	|| rightDescendentOf child parent forest

leftDescendantOf child parent forest =
	child `occursInForest` (parent `usesOf_sub_l` forest)

rightDescendentOf child parent forest =
	child `occursInForest` (parent `usesOf_sub_r` forest)

-- Trees with expressions that use the given macro.
usesOf_1, usesOf_sub_l, usesOf_sub_r, usesOf_sub, usesOf 
	:: Macro -> Forest -> Forest

m `usesOf_1` ts 	= filter (\(Tree e _ _) -> m `occursInExpr` e) ts
m `usesOf_sub_l` ts = concat $ map (\(Tree _ l _) -> m `usesOf_sub` l) ts
m `usesOf_sub_r` ts = concat $ map (\(Tree _ _ r) -> m `usesOf_sub` r) ts
m `usesOf_sub` ts 	= m `usesOf_sub_l` ts ++ m `usesOf_sub_r` ts
m `usesOf` ts 		= m `usesOf_1` ts ++ m `usesOf_sub` ts


-- Macro Dominance
-------------------------------------------------------------------------------

-- Transform into literal normal form (i.e. Not is only applied to names)
toLnf :: Expr -> Expr
toLnf (e :& e')         = toLnf e :& toLnf e'
toLnf (e :| e')         = toLnf e :| toLnf e'
toLnf (Not (e :& e'))   = toLnf (Not e) :| toLnf (Not e')
toLnf (Not (e :| e'))   = toLnf (Not e) :& toLnf (Not e')
toLnf (Not (Not e))     = toLnf e
toLnf e@(Not (Macro _)) = e
toLnf e@(Macro _)       = e

-- Calculate dominating literals
type Lit = (Bool, Macro)

dominatingLiterals :: Expr -> [Lit]
dominatingLiterals = noConflict . findDoms . toLnf

-- Find the potential dominating literals
findDoms :: Expr -> [Lit]
findDoms (Macro m)          = [(True, m)]
findDoms (Not (Macro m))    = [(False, m)]
findDoms (e :& e')          = findDoms e ++ findDoms e'
findDoms (e :| e')          = findDoms e `intersect` findDoms e'

-- Remove contradictory dominating literals.
noConflict :: [Lit] -> [Lit]
noConflict ls = nub [ l | l@(b, n) <- ls, not $ elem (not b, n) ls ]


-- Macro Selection and Blocking
-------------------------------------------------------------------------------

controls, weakIndep, sel_l, sel_r, blk_l, blk_r :: Macro -> Tree -> Bool
m `controls` (Tree e _ _) 	= any (\(_, m') -> m == m') $ dominatingLiterals e
m `sel_l` (Tree e _ _) 		= (True, m) `elem` dominatingLiterals e
m `sel_r` (Tree e _ _) 		= (False, m) `elem` dominatingLiterals e

m `weakIndep` t = not $ m `controls` t
blk_l = sel_r
blk_r = sel_l


-- Mutual exclusion
-------------------------------------------------------------------------------

mutex :: Macro -> Macro -> Forest -> Bool
mutex m m' ts = not $ m `desc` m' || m' `desc` m
	where desc m m' = descendantOf m m' ts






