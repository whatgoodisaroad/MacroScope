module Macroscope where


import Data.List (nub,intersect)


type Name = String

data BExpr = Macro Name
           | And BExpr BExpr
           | Or BExpr BExpr
           | Not BExpr
           deriving Show

type Lit = (Bool,Name)


-- Transform into literal normal form (i.e. Not is only applied to names)
--
lnf :: BExpr -> BExpr
lnf (And e e')        = And (lnf e) (lnf e')
lnf (Or  e e')        = Or  (lnf e) (lnf e')
lnf (Not (And e e'))  = Or  (lnf (Not e)) (lnf (Not e'))
lnf (Not (Or  e e'))  = And (lnf (Not e)) (lnf (Not e'))
lnf (Not (Not e))     = lnf e
lnf e@(Not (Macro _)) = e
lnf e@(Macro _)       = e


-- Compute dominating literals
--
domlit :: BExpr -> [Lit]
domlit = noConflict . dominate . lnf


dominate :: BExpr -> [Lit]
dominate (Macro m)       = [(True,m)]
dominate (Not (Macro m)) = [(False,m)]
dominate (And e e')      = dominate e ++ dominate e'
dominate (Or  e e')      = dominate e `intersect` dominate e'

noConflict :: [Lit] -> [Lit]
noConflict ls = nub [l | l@(b,n) <- ls, not (elem (not b,n) ls)]


-- Examples
--
[m1,m2,m3] = [ Macro ('m':show i) | i <- [1..3] ]

e1 = And m1 (Not m1)
e2 = And m2 (Not m1)
e = Or m1 e2
