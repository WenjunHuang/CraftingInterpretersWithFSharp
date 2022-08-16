namespace com.github.wenjunhuang.lox.interpreter

open Microsoft.FSharp.Core
open com.github.wenjunhuang.lox

type Interpreter() =
    let mutable environment =
        VarEnvironment.Global

    let checkNumberOperand operator operand func =
        // let ok = operand :? double
        match operand with
        | DoubleValue v -> func v
        | _ -> raise (RuntimeError(operator, "Operand must be a number."))

    let checkNumberOperands operator left right func =
        match (left, right) with
        | (DoubleValue lv, DoubleValue rv) -> func lv rv
        | _ -> raise (RuntimeError(operator, "Operands must be number."))

    let isTruthy operand =
        match operand with
        | BoolValue b -> b
        | NoValue -> false
        | _ -> true

    let rec visit expr =
        match expr with
        | Literal literal -> literal
        | Unary (operator, right) ->
            let right = evaluate right

            match operator.tokenType with
            | TokenType.MINUS -> DoubleValue(checkNumberOperand operator right (fun x -> -x))
            | TokenType.BANG -> BoolValue(not (isTruthy right))
            | _ -> NoValue

        | Binary (left, operator, right) ->
            let lv = evaluate left
            let rv = evaluate right

            match operator.tokenType with
            | TokenType.PLUS ->
                match (lv, rv) with
                | DoubleValue l, DoubleValue r -> DoubleValue(l + r)
                | StringValue l, StringValue r -> StringValue(l + r)
                | _ -> NoValue
            | TokenType.MINUS -> DoubleValue(checkNumberOperands operator lv rv (fun l r -> l - r))
            | TokenType.STAR -> DoubleValue(checkNumberOperands operator lv rv (fun l r -> l * r))
            | TokenType.SLASH -> DoubleValue(checkNumberOperands operator lv rv (fun l r -> l / r))
            | TokenType.GREATER -> BoolValue(checkNumberOperands operator lv rv (fun l r -> l > r))
            | TokenType.GREATER_EQUAL -> BoolValue(checkNumberOperands operator lv rv (fun l r -> l >= r))
            | TokenType.LESS -> BoolValue(checkNumberOperands operator lv rv (fun l r -> l < r))
            | TokenType.LESS_EQUAL -> BoolValue(checkNumberOperands operator lv rv (fun l r -> l <= r))
            | TokenType.BANG_EQUAL -> BoolValue(not (lv = rv))
            | TokenType.EQUAL_EQUAL -> BoolValue(lv = rv)
            | _ -> NoValue

        | Grouping expression -> evaluate expression
        | Variable (name) -> environment.Get name
        | Assign (name, value) ->
            let value = evaluate value
            environment.Assign name value
            value

    and evaluate expr = visit expr

    let rec visitStatement stmt =
        match stmt with
        | Statement.Expr expr -> evaluate expr |> ignore
        | Statement.Print expr ->
            let value = evaluate expr
            printfn $"{value}"
        | Statement.Var (token, expressionOption) ->
            let value =
                match expressionOption with
                | Some expr -> evaluate expr
                | None -> NoValue

            environment.Define token value
        | Statement.Block stmts -> executeBlock stmts

    and executeBlock statements =
        let oldEnvironment = environment

        try
            let newEnvironment =
                VarEnvironment.Enclosing oldEnvironment

            environment <- newEnvironment

            for stmt in statements do
                visitStatement stmt
        finally
            environment <- oldEnvironment

    member this.Interpret(statements: seq<Statement>) =
        try
            for stmt in statements do
                (this :> IStmtVisitor<obj>).Visit(stmt) |> ignore
        with
        | :? RuntimeError as re -> Lox.runtimeError re

    interface IExprVisitor<Value> with
        member this.Visit(expr) = visit expr

    interface IStmtVisitor<obj> with
        member this.Visit(stmt) = visitStatement stmt
