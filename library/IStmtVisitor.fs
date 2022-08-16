namespace com.github.wenjunhuang.lox

type IStmtVisitor<'T> =
    abstract Visit: stmt: Statement -> 'T
