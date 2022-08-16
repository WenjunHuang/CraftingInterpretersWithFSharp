namespace com.github.wenjunhuang.lox

open System
open com.github.wenjunhuang.lox

type RuntimeError(token: Token, message: String) =
    inherit Exception(message)

    member this.Token = token
