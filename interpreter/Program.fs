module com.github.wenjunhuang.lox.interpreter.Program

open com.github.wenjunhuang.lox

[<EntryPoint>]
let main _ =
    let cont =
        seq {
            let mutable endLoop = false

            while not endLoop do
                stdout.Write("> ")

                match stdin.ReadLine() |> Option.ofObj with
                | Some line -> yield line
                | None -> endLoop <- true
        }

    let interpret = Interpreter()

    for line in cont do
        match (Scanner(line).scanTokens () |> Parser).Parse() with
        | Ok resultValue -> interpret.Interpret(resultValue)
        | Error error -> stdout.WriteLine(error.Message)

    0
