module Symbols exposing (..)

import Parser exposing (..)


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


isLetter : Char -> Bool
isLetter c =
    List.member c letters


isDigit : Char -> Bool
isDigit c =
    List.member c digits


isSymbol : Char -> Bool
isSymbol c =
    List.member c symbols


isCharacter : Char -> Bool
isCharacter c =
    isLetter c || isDigit c || isSymbol c || c == '_'


isLetterOrDigitOrUnderscore : Char -> Bool
isLetterOrDigitOrUnderscore c =
    List.member c letters
        || List.member c digits
        || (c == '_')


letters : List Char
letters =
    [ 'A'
    , 'B'
    , 'C'
    , 'D'
    , 'E'
    , 'F'
    , 'G'
    , 'H'
    , 'I'
    , 'J'
    , 'K'
    , 'L'
    , 'M'
    , 'N'
    , 'O'
    , 'P'
    , 'Q'
    , 'R'
    , 'S'
    , 'T'
    , 'U'
    , 'V'
    , 'W'
    , 'X'
    , 'Y'
    , 'Z'
    , 'a'
    , 'b'
    , 'c'
    , 'd'
    , 'e'
    , 'f'
    , 'g'
    , 'h'
    , 'i'
    , 'j'
    , 'k'
    , 'l'
    , 'm'
    , 'n'
    , 'o'
    , 'p'
    , 'q'
    , 'r'
    , 's'
    , 't'
    , 'u'
    , 'v'
    , 'w'
    , 'x'
    , 'y'
    , 'z'
    ]


digits : List Char
digits =
    [ '0'
    , '1'
    , '2'
    , '3'
    , '4'
    , '5'
    , '6'
    , '7'
    , '8'
    , '9'
    ]


symbols : List Char
symbols =
    [ '<'
    , '>'
    , '.'
    , ','
    , ';'
    , '='
    ]



-- letter : Parser ()
-- letter =
--     succeed identity
--         |= (oneOf <|
--                 List.map symbol <|
--                     List.map toString letters
--            )
--
--
-- digit : Parser ()
-- digit =
--     succeed identity
--         |= (oneOf <|
--                 List.map symbol <|
--                     List.map toString digits
--            )
--
--
-- sym : Parser ()
-- sym =
--     succeed identity
--         |= (oneOf <|
--                 List.map symbol <|
--                     List.map toString symbols
--            )
--
--
-- char : Parser ()
-- char =
--     oneOf [ letter, digit, sym, symbol "_" ]
