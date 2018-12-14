type token =
    | OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | Semicolon
    | IntKeyword
    | ReturnKeyword
    | Negation
    | BitwiseComp
    | LogicNegation
    | Int of int
    | Id of string