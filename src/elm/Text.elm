module Text exposing (Letter, Letters, stringToLetters)

import Color exposing (Color)
import Dict exposing (Dict)
import Data.Common exposing (Position)
import Maybe.Extra


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
                (String.toList line)
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
    List.map
        (flip Dict.get dictionary)
        (String.toList string)
        |> Maybe.Extra.values
