module GameState.MainMenu exposing (..)

import Text
import Data.Menu as Menu
import InputController


type alias Model =
    { menu : Menu.Menu }


init : Model
init =
    { menu =
        { items =
            { before = []
            , selected =
                { key = "start"
                , text = Text.stringToLetters "start"
                }
            , after =
                [ { key = "about"
                  , text = Text.stringToLetters "about"
                  }
                ]
            }
        }
    }


updateTick : Model -> InputController.Model -> Model
updateTick model inputModel =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.DownKey ->
            moveMenuDown model

        _ ->
            model



moveMenuDown : Model -> Model
moveMenuDown model =
    case model.menu.items.after of
        [] ->
            model

        next :: others ->
