namespace com.github.wenjunhuang.lox

open System.Collections.Generic

type VarEnvironment =

    val private outer: option<VarEnvironment>
    val mutable values: Map<string, Value>

    private new(o: option<VarEnvironment>) = { outer = o; values = Map.empty }

    static member Global = VarEnvironment(None)
    static member Enclosing(outer: VarEnvironment) = VarEnvironment(Some outer)

    member this.Define (name: Token) value =
        this.values <- Map.add name.lexeme value this.values

    member this.Get(name: Token) =
        match Map.tryFind name.lexeme this.values with
        | Some value -> value
        | None ->
            match this.outer with
            | Some outer -> outer.Get(name)
            | None -> raise (RuntimeError(name, "Undefined variable."))

    member this.Assign (name: Token) (value: Value) =
        this.values <-
            this.values
            |> Map.change name.lexeme (function
                | Some _ -> Some value
                | None -> raise (RuntimeError(name, "Undefined variable.")))
