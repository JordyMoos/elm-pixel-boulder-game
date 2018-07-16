module Text exposing (..)

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
    Dict.fromList
        [ ( 'a', Letter 3 [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ), ( 2, 4 ) ] )
        , ( 'b', Letter 3 [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 1, 0 ), ( 1, 2 ), ( 1, 4 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ) ] )
        , ( 'c', Letter 3 [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 0 ), ( 1, 4 ), ( 2, 0 ), ( 2, 4 ) ] )
        , ( 'w', Letter 5 [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 1, 4 ), ( 2, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 0 ), ( 4, 1 ), ( 4, 2 ), ( 4, 3 ) ] )
        ]


stringToPixels : String -> Letters
stringToPixels string =
    []
