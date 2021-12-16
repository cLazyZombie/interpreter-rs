[statements]
Stmt = LetStmt | ReturnStmt | BlockStmt | ExprStmt
LetStmt = "let" IdentToken "=" Expr ";"
ReturnStmt = "return" Expr ";"
BlockStmt = "{" Stmt "}"

[expr]
Expr =  IdentExpr | NumExpr | BoolExpr | PrefixExpr | InfixExpr | IfExpr | FnExpr | FnCallExpr
IdentExpr = IdentToken
