module com.github.wenjunhuang.lox.Lox

let mutable hadError = false

let report line where msg =
    printfn $"[line {line} Error {where}: {msg}"

let errorAt line msg = report line "" msg

let error token message =
    if token.tokenType = TokenType.EOF then
        report token.line "" message
    else
        report token.line $" at '{token.lexeme}'" message

let runtimeError (error: RuntimeError) =
    printfn $"{error.Message}\n[line ${error.Token.line}"
    hadError <- true
