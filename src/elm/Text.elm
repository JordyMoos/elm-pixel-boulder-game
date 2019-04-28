module Text exposing (Letter, Letters, letterHeight, stringToLetters)

import Data.Position as Position exposing (Position)
import Dict exposing (Dict)
import Maybe.Extra


letterHeight : Int
letterHeight =
    5


type alias Letter =
    { width : Int
    , positions : List Position
    }


type alias Design =
    List String


type alias Letters =
    List Letter


dictionary : Dict Char Letter
dictionary =
    [ ( 'a'
      , [ " o "
        , "o o"
        , "ooo"
        , "o o"
        , "o o"
        ]
      )
    , ( 'b'
      , [ "oo "
        , "o o"
        , "oo "
        , "o o"
        , "oo "
        ]
      )
    , ( 'c'
      , [ " oo"
        , "o  "
        , "o  "
        , "o  "
        , " oo"
        ]
      )
    , ( 'd'
      , [ "oo "
        , "o o"
        , "o o"
        , "o o"
        , "oo "
        ]
      )
    , ( 'e'
      , [ "oo"
        , "o "
        , "oo"
        , "o "
        , "oo"
        ]
      )
    , ( 'f'
      , [ "oo"
        , "o "
        , "oo"
        , "o "
        , "o "
        ]
      )
    , ( 'g'
      , [ " ooo"
        , "o  "
        , "o oo"
        , "o  o"
        , " oo "
        ]
      )
    , ( 'h'
      , [ "o o"
        , "o o"
        , "ooo"
        , "o o"
        , "o o"
        ]
      )
    , ( 'i'
      , [ "o"
        , "o"
        , "o"
        , "o"
        , "o"
        ]
      )
    , ( 'j'
      , [ "  o"
        , "  o"
        , "  o"
        , "o o"
        , " o"
        ]
      )
    , ( 'k'
      , [ "o  o"
        , "o o "
        , "oo  "
        , "o o "
        , "o  o"
        ]
      )
    , ( 'l'
      , [ "o "
        , "o "
        , "o "
        , "o "
        , "oo"
        ]
      )
    , ( 'm'
      , [ "o   o"
        , "oo oo"
        , "o o o"
        , "o   o"
        , "o   o"
        ]
      )
    , ( 'n'
      , [ "o   o"
        , "oo  o"
        , "o o o"
        , "o  oo"
        , "o   o"
        ]
      )
    , ( 'o'
      , [ " oo "
        , "o  o"
        , "o  o"
        , "o  o"
        , " oo "
        ]
      )
    , ( 'p'
      , [ "oo "
        , "o o"
        , "oo "
        , "o  "
        , "o  "
        ]
      )
    , ( 'q'
      , [ " oo"
        , "o o"
        , " oo"
        , "  o"
        , "  o"
        ]
      )
    , ( 'r'
      , [ "oo "
        , "o o"
        , "oo "
        , "o o"
        , "o o"
        ]
      )
    , ( 's'
      , [ " oo"
        , "o  "
        , " oo"
        , "  o"
        , "oo "
        ]
      )
    , ( 't'
      , [ "ooo"
        , " o "
        , " o "
        , " o "
        , " o "
        ]
      )
    , ( 'u'
      , [ "o o"
        , "o o"
        , "o o"
        , "o o"
        , "ooo"
        ]
      )
    , ( 'v'
      , [ "o o"
        , "o o"
        , "o o"
        , "o o"
        , " o "
        ]
      )
    , ( 'w'
      , [ "o   o"
        , "o o o"
        , "o o o"
        , "oo oo"
        , "o   o"
        ]
      )
    , ( 'x'
      , [ "o o"
        , "o o"
        , " o "
        , "o o"
        , "o o"
        ]
      )
    , ( 'y'
      , [ "o o"
        , "o o"
        , " o "
        , " o "
        , " o "
        ]
      )
    , ( 'z'
      , [ "ooo"
        , "  o"
        , " o "
        , "o  "
        , "ooo"
        ]
      )
    , ( '0'
      , [ " o "
        , "o o"
        , "o o"
        , "o o"
        , " o "
        ]
      )
    , ( '1'
      , [ "o"
        , "o"
        , "o"
        , "o"
        , "o"
        ]
      )
    , ( '2'
      , [ " o "
        , "o o"
        , "  o"
        , " o "
        , "ooo"
        ]
      )
    , ( '3'
      , [ "ooo"
        , "  o"
        , " oo"
        , "  o"
        , "oo "
        ]
      )
    , ( '4'
      , [ "o o"
        , "o o"
        , "ooo"
        , "  o"
        , "  o"
        ]
      )
    , ( '5'
      , [ "ooo"
        , "o  "
        , "oo "
        , "  o"
        , "oo "
        ]
      )
    , ( '6'
      , [ "o  "
        , "o  "
        , "ooo"
        , "o o"
        , "ooo"
        ]
      )
    , ( '7'
      , [ "ooo"
        , "  o"
        , "  o"
        , "  o"
        , "  o"
        ]
      )
    , ( '8'
      , [ "ooo"
        , "o o"
        , "ooo"
        , "o o"
        , "ooo"
        ]
      )
    , ( '9'
      , [ "ooo"
        , "o o"
        , "ooo"
        , "  o"
        , "ooo"
        ]
      )
    , ( ' '
      , [ " "
        , " "
        , " "
        , " "
        , " "
        ]
      )
    , ( '.'
      , [ " "
        , " "
        , " "
        , " "
        , "o"
        ]
      )
    , ( ':'
      , [ " "
        , "o"
        , " "
        , " "
        , "o"
        ]
      )
    , ( '='
      , [ "   "
        , "ooo"
        , "   "
        , "ooo"
        , "   "
        ]
      )
    , ( ','
      , [ "  "
        , "  "
        , "  "
        , " o"
        , "o "
        ]
      )
    , ( '-'
      , [ "  "
        , "  "
        , "oo"
        , "  "
        , "  "
        ]
      )
    , ( '['
      , [ "oo"
        , "o "
        , "o "
        , "o "
        , "oo"
        ]
      )
    , ( ']'
      , [ "oo"
        , " o"
        , " o"
        , " o"
        , "oo"
        ]
      )
    ]
        |> List.map designToLetter
        |> Dict.fromList


designToLetter : ( Char, Design ) -> ( Char, Letter )
designToLetter ( char, design ) =
    ( char
    , { width = getWidth design
      , positions = getPositions design
      }
    )


getWidth : Design -> Int
getWidth design =
    design
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0


getPositions : Design -> List Position
getPositions design =
    design
        |> List.indexedMap
            (\y line ->
                String.toList line
                    |> List.indexedMap
                        (\x symbol ->
                            [ symbol ]
                                |> List.filter ((==) ' ' >> not)
                                |> List.map (always { x = x, y = y })
                        )
                    |> List.concat
            )
        |> List.concat


stringToLetters : String -> Letters
stringToLetters string =
    string
        |> String.toLower
        |> String.toList
        |> List.map (\a -> Dict.get a dictionary)
        |> Maybe.Extra.values
        |> List.reverse
