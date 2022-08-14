namespace com.github.wenjunhuang.lox

type IExprVisitor<'a> =
    abstract Visit: expr: Expression -> 'a