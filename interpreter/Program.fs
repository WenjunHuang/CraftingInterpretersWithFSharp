// For more information see https://aka.ms/fsharp-console-apps


[<EntryPoint>]
let main args =
    let cont =
        seq {
            let mutable endLoop = false

            while not endLoop do
                stdout.Write("> ")

                match stdin.ReadLine() |> Option.ofObj with
                | Some line -> yield line
                | None -> endLoop <- true
        }

    for line in cont do
        stdout.WriteLine line

    0
