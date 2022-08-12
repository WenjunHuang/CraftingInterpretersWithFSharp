namespace com.github.wenjunhuang.lox.lexer

type Token =
    { tokenType: TokenType
      lexeme: string
      literal: option<obj>
      line: int }
