-- MacroScope Syntax
-------------------------------------------------------------------------------

module MacroScope.Syntax where


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

instance Show Tree where
    show (Tree e f f') = show e ++ "<" ++ show f ++ "," ++ show f' ++ ">"
