module Text exposing (Letter, Letters, stringToLetters)

import Color exposing (Color)
import Dict exposing (Dict)


type alias Letter =
    { width : Int
    , positions : List ( Int, Int )
    }


type alias Letters =
    List Letter


dictionary : Dict Char Letter
dictionary =
    Dict.fromList <|
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
        ]
            |> List.map designToLetter


designToLetter : ( Char, List String ) -> Letter
designToLetter ( char, design ) =
    Letter char []


stringToLetters : String -> Letters
stringToLetters string =
    []
