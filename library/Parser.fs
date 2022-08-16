namespace com.github.wenjunhuang.lox

open System
open System.Collections.Generic
open System.Text.RegularExpressions

type Parser(tokens: Token array) =
    let mutable current = 0

    let peek () = tokens[current]

    let previous () = tokens[current - 1]

    let isAtEnd () = peek().tokenType = TokenType.EOF

    let advance () =
        if not (isAtEnd ()) then
            current <- current + 1

        previous ()

    let check tokenType =
        if isAtEnd () then
            false
        else
            peek().tokenType = tokenType

    let synchronize () =
        advance () |> ignore
        let mutable endLoop = false

        while not (isAtEnd ()) && not endLoop do
            if previous().tokenType = TokenType.SEMICOLON then
                endLoop <- true
            else
                match peek().tokenType with
                | CLASS
                | FOR
                | FUN
                | IF
                | PRINT
                | RETURN
                | WHILE
                | VAR -> endLoop <- true
                | _ -> advance () |> ignore

    let error token message =
        Lox.error token message
        ParseError()

    let consume tokenType message =
        if not (check tokenType) then
            error (peek ()) message |> raise
        else
            advance ()

    let matching types =
        if types |> Seq.exists check then
            advance () |> ignore
            true
        else
            false

    let rec declaration () =
        try
            if matching [ TokenType.VAR ] then
                Some(varDeclaration ())
            else
                Some(statement ())
        with
        | :? ParseError ->
            synchronize ()
            None

    and varDeclaration () =
        let name =
            consume TokenType.IDENTIFIER "Expect variable name."

        if matching [ TokenType.EQUAL ] then
            let initializer = expression ()

            consume TokenType.SEMICOLON "Expect ';' after variable declaration."
            |> ignore

            Statement.Var(name = name, initializer = Some(initializer))
        else
            consume TokenType.SEMICOLON "Expect '=' after variable name."
            |> ignore

            Statement.Var(name = name, initializer = None)

    and statement () =
        if matching [ TokenType.PRINT ] then
            printStatement ()
        elif matching [ TokenType.LEFT_BRACE ] then
            Statement.Block(blockStatement ())
        else
            expressionStatement ()

    and blockStatement () =
        let statements = List<Statement>()

        while not (check TokenType.RIGHT_BRACE)
              && not (isAtEnd ()) do
            match declaration () with
            | Some stm -> statements.Add(stm)
            | None -> ()

        consume TokenType.RIGHT_BRACE "Expect '}' after block."
        |> ignore

        statements.ToArray()

    and expressionStatement () =
        let expr = expression ()

        consume TokenType.SEMICOLON "Expect ';' after expression."
        |> ignore

        Statement.Expr(expr)

    and printStatement () =
        let expr = expression ()

        consume TokenType.SEMICOLON "Expect ';' after value."
        |> ignore

        Statement.Print(expr)

    and expression () = assignment ()

    and assignment () =
        let expr = equality ()

        if matching [ TokenType.EQUAL ] then
            let equals = previous ()

            match expr with
            | Expression.Variable token ->
                let value = assignment ()
                Expression.Assign(name = token, value = value)
            | c ->
                error equals "Invalid assignment target."
                |> ignore

                c
        else
            expr

    and equality () =
        let mutable expr = comparison ()

        while matching [ TokenType.BANG_EQUAL
                         TokenType.EQUAL_EQUAL ] do
            let op = previous ()
            let right = comparison ()
            expr <- Expression.Binary(left = expr, operator = op, right = right)

        expr

    and comparison () =
        let mutable expr = term ()

        while matching [ TokenType.GREATER
                         TokenType.GREATER_EQUAL
                         TokenType.LESS
                         TokenType.LESS_EQUAL ] do
            let op = previous ()
            let right = term ()
            expr <- Expression.Binary(left = expr, operator = op, right = right)

        expr

    and term () =
        let mutable expr = factor ()

        while matching [ TokenType.MINUS
                         TokenType.PLUS ] do
            let op = previous ()
            let right = factor ()
            expr <- Expression.Binary(left = expr, operator = op, right = right)

        expr

    and factor () =
        let mutable expr = unary ()

        while matching [ TokenType.SLASH
                         TokenType.STAR ] do
            let op = previous ()
            let right = unary ()
            expr <- Expression.Binary(left = expr, operator = op, right = right)

        expr

    and unary () =
        if matching [ BANG; MINUS ] then
            let op = previous ()
            let right = unary ()
            Unary(operator = op, right = right)
        else
            primary ()

    and primary () =
        if matching [ NUMBER; STRING ] then
            Literal(value = previous().literal)
        elif matching [ TRUE ] then
            Literal(value = BoolValue(true))
        elif matching [ FALSE ] then
            Literal(value = BoolValue(false))
        elif matching [ IDENTIFIER ] then
            Variable(name = previous ())
        elif matching [ NIL ] then
            Literal(value = NoValue)
        elif matching [ LEFT_PAREN ] then
            let expr = expression ()

            consume RIGHT_PAREN "Expect ')' after expression."
            |> ignore

            Grouping(expr)
        else
            error (peek ()) "Expect expression." |> raise


    member this.Parse() : Result<array<Statement>, Exception> =
        try
            let mutable statements = []

            while not (isAtEnd ()) do
                let statement = declaration ()

                match statement with
                | Some statement -> statements <- statement :: statements
                | None -> synchronize ()

            Ok(List.toArray statements)
        with
        | e -> Error(e)
