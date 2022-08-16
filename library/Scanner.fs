namespace com.github.wenjunhuang.lox

open System
open System.Collections.Generic

type Scanner(source: string) as this =
    static let keywords =
        Map [ ("and", AND)
              ("class", CLASS)
              ("else", ELSE)
              ("false", FALSE)
              ("for", FOR)
              ("fun", FUN)
              ("if", IF)
              ("nil", NIL)
              ("or", OR)
              ("print", PRINT)
              ("return", RETURN)
              ("super", SUPER)
              ("this", THIS)
              ("true", TRUE)
              ("var", VAR)
              ("while", WHILE) ]

    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let tokens = List<Token>()

    let advance () =
        current <- current + 1
        source.[current - 1]

    let isAtEnd () = current >= source.Length

    let matchChar c =
        if isAtEnd () then
            false
        else if source[current] <> c then
            false
        else
            current <- current + 1
            true

    let peek () =
        if isAtEnd () then
            '\u0000'
        else
            source[current]

    let isDigit c = c >= '0' && c <= '9'

    let isAlpha c =
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c = '_')

    let isAlphaNumeric c = (isAlpha c) || (isDigit c)

    let peekNext () =
        if current + 1 >= source.Length then
            '\u0000'
        else
            source[current + 1]

    let number () =
        while peek () |> isDigit do
            advance () |> ignore

        // Look for a fractional part.
        if peek () = '.' && peekNext () |> isDigit then
            // Consume the "."
            advance () |> ignore

            while peek () |> isDigit do
                advance () |> ignore

        let literal =
            match Double.TryParse(source.Substring(start, current - start)) with
            | (true, value) -> DoubleValue(value)
            | _ -> NoValue

        this.addToken (TokenType.NUMBER, literal)

    let stringToken () =
        while peek () <> '"' && not (isAtEnd ()) do
            if peek () = '\n' then line <- line + 1
            advance () |> ignore

        if isAtEnd () then
            Lox.errorAt line "Unterminated string."
        else
            advance () |> ignore // The closing "
            // Trim the surrounding quotes
            let value = StringValue(source.Substring(start + 1, current - start - 2))

            this.addToken (TokenType.STRING, value)

    let identifier () =
        while peek () |> isAlphaNumeric do
            advance () |> ignore

        let text = source.Substring(start, current - start)

        match keywords.TryGetValue text with
        | true, token -> this.addToken token
        | _ -> this.addToken IDENTIFIER


    let scanToken () =
        let c = advance ()

        match c with
        | '(' -> this.addToken TokenType.LEFT_PAREN
        | ')' -> this.addToken TokenType.RIGHT_PAREN
        | '{' -> this.addToken TokenType.LEFT_BRACE
        | '}' -> this.addToken TokenType.RIGHT_BRACE
        | ',' -> this.addToken TokenType.COMMA
        | '.' -> this.addToken TokenType.DOT
        | '-' -> this.addToken TokenType.MINUS
        | '+' -> this.addToken TokenType.PLUS
        | ';' -> this.addToken TokenType.SEMICOLON
        | '*' -> this.addToken TokenType.STAR
        | '!' ->
            (if matchChar '=' then
                 TokenType.BANG_EQUAL
             else
                 TokenType.BANG)
            |> this.addToken
        | '=' ->
            (if matchChar '=' then
                 TokenType.EQUAL_EQUAL
             else
                 TokenType.EQUAL)
            |> this.addToken
        | '<' ->
            (if matchChar '=' then
                 TokenType.LESS_EQUAL
             else
                 TokenType.LESS)
            |> this.addToken
        | '>' ->
            (if matchChar '=' then
                 TokenType.GREATER_EQUAL
             else
                 TokenType.GREATER)
            |> this.addToken
        | '/' ->
            if matchChar '/' then
                let notEndOfLine () = peek () <> '\n' && not (isAtEnd ())

                while notEndOfLine () do
                    advance () |> ignore
            elif matchChar '*' then
                let matchEndOfComment () = matchChar '*' && matchChar '/'

                while not (isAtEnd ()) && not (matchEndOfComment ()) do
                    advance () |> ignore
            else
                this.addToken TokenType.SLASH
        | ' '
        | '\r'
        | '\t' -> ()
        | '\n' -> line <- line + 1
        | '"' -> stringToken ()
        | c when isDigit (c) -> number ()
        | c when isAlpha (c) -> identifier ()
        | _ -> Lox.errorAt line "Unexpected character."

    member private this.addToken(tokenType: TokenType, ?literal: Value) =
        tokens.Add
            { tokenType = tokenType
              lexeme = source.Substring(start, current - start)
              literal = literal |> Option.defaultValue NoValue
              line = line }


    member this.scanTokens() : Token array =
        while not (isAtEnd ()) do
            start <- current
            scanToken ()

        this.addToken TokenType.EOF
        tokens.ToArray()
