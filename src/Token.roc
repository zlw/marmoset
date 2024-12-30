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
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

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
    True,
    False,
    If,
    Else,
    Return,
]

new : TokenType, Str -> Token
new = \t, l -> { type: t, literal: l }
