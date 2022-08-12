namespace com.github.wenjunhuang.lox.lexer

type Scanner(source: string) =
    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let tokens: Token list = []
    
    let advance () =
        current <- current + 1
        source.[current - 1]

    let isAtEnd () =
        current >= source.Length
        
    let matchChar c =
        if isAtEnd() then false
        else if source.[current] == c then false
        else
            advance() |> ignore
            true
        
    member this.scanTokens() =
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
        | '!' -> if (matchChar '=') then this.addToken TokenType.BANG_EQUAL else this.addToken TokenType.BANG
        | _ -> failwith "todo"

        
    member this.addToken (tokenType:TokenType) (?literal: obj) =
        ()
        
