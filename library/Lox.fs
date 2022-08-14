module com.github.wenjunhuang.lox.Lox

let mutable hadError = false

let report line where msg =
    printfn $"[line {line} Error {where}: {msg}"
    
let error line msg =
    report line "" msg