namespace com.github.wenjunhuang.lox

type Value =
    | NoValue
    | DoubleValue of double
    | StringValue of string
    | BoolValue of bool

