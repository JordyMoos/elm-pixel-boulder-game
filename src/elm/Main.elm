module Main exposing (main)

import Html exposing (..)


type alias Model =
    {}


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


view : Model -> Html Msg
view model =
    div
        []
        (List.concat <|
            List.repeat 12 <|
                List.append
                    (List.repeat 12 dirt)
                    [ br [] [] ]
        )


empty : Html Msg
empty =
    text "[ ]"


wall : Html Msg
wall =
    text "[@]"


dirt : Html Msg
dirt =
    text "[.]"


player : Html Msg
player =
    text "[P]"


dead : Html Msg
dead =
    text "[X]"


rock : Html Msg
rock =
    text "[O]"


diamond : Html Msg
diamond =
    text "[*]"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
