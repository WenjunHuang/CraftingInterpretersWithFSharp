namespace com.github.wenjunhuang.lox

type Token =
    { tokenType: TokenType
      lexeme: string
      literal: obj option
      line: int }
