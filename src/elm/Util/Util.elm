module Util.Util exposing (lazyAll, lazyAny, colorToHex, hexToColor)

import Color exposing (Color, toRgb, rgba, rgb)
import Char
import Regex
import ParseInt exposing (parseIntHex)


lazyAll : List (() -> Bool) -> Bool
lazyAll =
    List.all
        (\p -> p ())


lazyAny : List (() -> Bool) -> Bool
lazyAny =
    List.any
        (\p -> p ())


colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            toRgb cl
    in
        List.map toHex [ red, green, blue ]
            |> (::) "#"
            |> String.join ""


toHex : Int -> String
toHex =
    toRadix >> String.padLeft 2 '0'


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
        if n < 16 then
            getChr n
        else
            toRadix (n // 16) ++ getChr (n % 16)


hexToColor : String -> Result String Color
hexToColor =
    let
        {- Converts "f" to "ff" and "ff" to "ff" -}
        extend : String -> String
        extend token =
            case String.toList token of
                [ token ] ->
                    String.fromList [ token, token ]

                _ ->
                    token

        pattern =
            ""
                ++ "^"
                ++ "#?"
                ++ "(?:"
                -- RRGGBB
                ++
                    "(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))"
                -- RGB
                ++
                    "|"
                ++ "(?:([a-f\\d])([a-f\\d])([a-f\\d]))"
                -- RRGGBBAA
                ++
                    "|"
                ++ "(?:([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2}))"
                -- RGBA
                ++
                    "|"
                ++ "(?:([a-f\\d])([a-f\\d])([a-f\\d])([a-f\\d]))"
                ++ ")"
                ++ "$"
    in
        String.toLower
            >> Regex.find (Regex.AtMost 1) (Regex.regex pattern)
            >> List.head
            >> Maybe.map .submatches
            >> Maybe.map (List.filterMap identity)
            >> Result.fromMaybe "Parsing hex regex failed"
            >> Result.andThen
                (\colors ->
                    case List.map (extend >> parseIntHex) colors of
                        [ Ok r, Ok g, Ok b, Ok a ] ->
                            Ok <| rgba r g b (roundToPlaces 2 (toFloat a / 255))

                        [ Ok r, Ok g, Ok b ] ->
                            Ok <| rgb r g b

                        _ ->
                            -- there could be more descriptive error cases per channel
                            Err "Parsing ints from hex failed"
                )


roundToPlaces : Int -> Float -> Float
roundToPlaces places number =
    let
        multiplier =
            toFloat (10 ^ places)
    in
        toFloat (round (number * multiplier)) / multiplier

