namespace com.github.wenjunhuang.lox

type Expression =
    | Literal of value: Value
    | Unary of operator: Token * right: Expression
    | Assign of name: Token * value: Expression
    | Binary of left: Expression * operator: Token * right: Expression
    | Grouping of expression: Expression
    | Variable of name: Token
