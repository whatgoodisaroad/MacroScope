-- MacroScope Properties
-------------------------------------------------------------------------------

module MacroScope.Properties where

import Data.List (nub, intersect)
import Control.Monad (guard)

import  MacroScope.Syntax

-- Expression Semantics
-------------------------------------------------------------------------------

evalExpr :: [Macro] -> Expr -> Bool
evalExpr ms (Macro m) = m `elem` ms
evalExpr ms (e :& e') = evalExpr ms e && evalExpr ms e'
evalExpr ms (e :| e') = evalExpr ms e || evalExpr ms e'
evalExpr ms (Not e) = not $ evalExpr ms e


-- Macro Occurrence
-------------------------------------------------------------------------------

exprOfTree :: Tree -> Expr
exprOfTree (Tree e _ _) = e

leftOfTree, rightOfTree :: Tree -> Forest
leftOfTree (Tree _ l _) = l
rightOfTree (Tree _ _ r) = r

-- Checks whether the macro occurs in any expression in the tree, at any level.
-- Defined as the disjunction of whether the macro appears in any of the trees.
occursInForest :: Macro -> Forest -> Bool
m `occursInForest` fs = any (occursInTree m) fs

-- Checks whether the macro occurs in the tree's expression, or in its left or 
-- right subforests.
occursInTree :: Macro -> Tree -> Bool
m `occursInTree` (Tree e f1 f2) = or [ 
		(occursInExpr m) e, 
		(occursInForest m) f1, 
		(occursInForest m) f2 
	]

-- Checks whether the macro is used anywhere in the expression.
occursInExpr :: Macro -> Expr -> Bool
m `occursInExpr` (Macro m') = m == m'
m `occursInExpr` (e :& e') = or $ map (occursInExpr m) [e, e']
m `occursInExpr` (e :| e') = or $ map (occursInExpr m) [e, e']
m `occursInExpr` (Not e) = (occursInExpr m) e


-- Macro Listing
-------------------------------------------------------------------------------

-- Lists all the macros in the expression.
macrosOfExpr :: Expr -> [Macro]
macrosOfExpr (Macro m) = [m]
macrosOfExpr (e :& e') = macrosOfExpr e ++ macrosOfExpr e'
macrosOfExpr (e :| e') = macrosOfExpr e ++ macrosOfExpr e'
macrosOfExpr (Not e) = macrosOfExpr e

-- Lists all the macros from the tree's expression, and left and right 
-- subforests.
macrosOfTree :: Tree -> [Macro]
macrosOfTree (Tree e f f') = nub $
	   macrosOfExpr e 
	++ macrosOfForest f 
	++ macrosOfForest f'

-- List all the macros in the forest.
macrosOfForest :: Forest -> [Macro]
macrosOfForest ts = nub $ concat $ map macrosOfTree ts


-- Macro Descendance
-------------------------------------------------------------------------------

-- Checks whether there is ever a use of the child macro under a use of the 
-- parent macro.
descendantOf :: Macro -> Macro -> Forest -> Bool
descendantOf child parent forest = any 
	(child `leftDescendantOf` parent) 
	(parent `useOfInForest` forest)

leftDescendantOf, rightDescendentOf 
	:: Macro -> Macro -> Tree -> Bool 

leftDescendantOf child parent tree =
	child `occursInForest` (parent `useOfInLeft` tree)

rightDescendentOf child parent tree =
	child `occursInForest` (parent `useOfInRight` tree)



-- Checks whether the child macro is ever in a tree selected by the parent 
-- macro. This is tricky because the two different kinds of selection require
-- two different kinds of recursion.
follows :: Macro -> Macro -> Forest -> Bool
follows child parent forest = inL || inR
	where
		inL = child `occursInForest` 
			(concat $ map leftOfTree $ parent `sel_lUseIn` forest)
		inR = child `occursInForest` 
			(concat $ map rightOfTree $ parent `sel_rUseIn` forest)




-- Trees with expressions that use the given macro, not necessarily in the 
-- trees of their subforests.
useOfInForest, strongUseIn :: Macro -> Forest -> Forest

m `useOfInForest` ts = concat $ map (useOfInTree m) ts

-- Strong use is use of a macro where it controls.
m `strongUseIn` ts = filter (m `controls`) $ m `useOfInForest` ts
m `sel_lUseIn` ts = filter (m `sel_l`) $ m `useOfInForest` ts
m `sel_rUseIn` ts = filter (m `sel_r`) $ m `useOfInForest` ts


useOfInTree, useOfInLeft, useOfInRight :: Macro -> Tree -> Forest

-- Trees which use the macro in the left subforest of the given tree.
m `useOfInLeft` (Tree _ l _) = m `useOfInForest` l

-- Trees which use the macro in the right subforest of the given tree.
m `useOfInRight` (Tree _ _ r) = m `useOfInForest` r

-- Trees which use the macro in either the left or right subforest, in addition 
-- to the tree itself, if it uses the macro.
m `useOfInTree` t@(Tree e left right) = self ++ l_sub ++ r_sub
	where
		inExpr = m `elem` macrosOfExpr e
		self = if inExpr then [t] else []
		l_sub = m `useOfInLeft` t
		r_sub = m `useOfInRight` t




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

type Lit = (Bool, Macro)

-- Calculate dominating literals
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

-- A macro is said to control an expression if it dominates in either the 
-- positive or the negative.
m `controls` (Tree e _ _) 	= any (\(_, m') -> m == m') $ dominatingLiterals e

-- A macro is said to select-left the expression if it is a positive dominating
-- literal, and select-right if it is negative one.
m `sel_l` (Tree e _ _) 		= (True, m) `elem` dominatingLiterals e
m `sel_r` (Tree e _ _) 		= (False, m) `elem` dominatingLiterals e

-- A macro is said to be weakly independent from a tree if it does not dominate
-- at all.
m `weakIndep` t = not $ m `controls` t

-- Blocking is selection on the other hand.
blk_l = sel_r
blk_r = sel_l


-- Mutual exclusion
-------------------------------------------------------------------------------

-- Macros m and m' appear to be mutually exclusive iff neither m descends from
-- m' nor m' descends from m.
mutex :: Macro -> Macro -> Forest -> Bool
mutex m m' ts = not $ m `desc` m' || m' `desc` m
	where desc m m' = follows m m' ts

-- Get pairwise distinct pairs from a list
pairs :: [a] -> [(a, a)]
pairs as = do
	idx <- [0 .. (pred $ length as)]
	let a = as !! idx
	a' <- drop (succ idx) as
	return (a, a')

-- Naive implementation.
mutexMacros_naive :: Forest -> [(Macro, Macro)]
mutexMacros_naive f = do
	(m, m') <- pairs $ macrosOfForest f
	guard $ mutex m m' f
	return (m, m')



-- For test purposes, delete eventually...
-- ts <- fmap (head . translate) $ parseFile discard "tests/testfile.c"


