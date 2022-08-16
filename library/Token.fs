namespace com.github.wenjunhuang.lox

type Token =
    { tokenType: TokenType
      lexeme: string
      literal: Value
      line: int }
