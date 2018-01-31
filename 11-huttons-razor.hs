data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)          = x
eval (Add left right) = eval left + eval right

printExpr :: Expr -> String
printExpr (Lit x)          = show x
printExpr (Add left right) = printExpr left ++ " + " ++ printExpr right


