module Symbols exposing (digits, isCharacter, isDigit, isLetter, isLetterOrDigitOrUnderscore, isSymbol, letters, symbols)

import Parser exposing (..)


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
    [ '['
    , ']'
    , '{'
    , '}'
    , '('
    , ')'
    , '<'
    , '>'
    , '='
    , ','
    , '.'
    , '|'
    , ';'
    , ' '
    , '!'
    , '#'
    , '$'
    , '&'
    , '%'
    , '*'
    , '+'
    , '-'
    , '/'
    , ':'
    , '?'
    , '@'
    , '\\'
    , '^'
    , '_'
    , '`'
    , '~'
    , '\''
    ]
