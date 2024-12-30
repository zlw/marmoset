module [Token, TokenType, new]

Token : {
    type : TokenType,
    literal : Str,
}

TokenType : [
    Illegal,
    EOF,

    # Identifiers + literals
    Ident,
    Int,

    # Operators
    Assign,
    Plus,

    # Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    # Keywords
    Function,
    Let,
]

new : TokenType, Str -> Token
new = \t, l -> { type: t, literal: l }
