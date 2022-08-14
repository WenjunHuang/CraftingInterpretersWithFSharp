namespace com.github.wenjunhuang.lox

type Statement =
    | Expr of expr:Expression
    | Var of name:Token * initializer: Expression option
    | Print of expr:Expression