module GameState.MainMenu
    exposing
        ( Model
        , Action(..)
        , init
        , updateTick
        , view
        )

import Text
import Data.Menu as Menu
import InputController
import Html exposing (Html, div)
import Renderer.Canvas.TextRenderer as TextRenderer


type alias Model =
    { menu : Menu.Menu
    , width : Int
    , height : Int
    }


type Action
    = Stay Model
    | LoadLevel String


init : Int -> Int -> Model
init width height =
    { menu =
        { items =
            { before = []
            , selected =
                { key = "level-001"
                , text = Text.stringToLetters "Pixel"
                }
            , after =
                [ { key = "level-images"
                  , text = Text.stringToLetters "Images"
                  }
                , { key = "level-pacman"
                  , text = Text.stringToLetters "Pac-Man"
                  }
                , { key = "level-tank"
                  , text = Text.stringToLetters "Tank"
                  }
                ]
            }
        }
    , width = width
    , height = height
    }


updateTick : InputController.Model -> Model -> Action
updateTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.UpKey ->
            Menu.moveMenuUp model.menu
                |> setMenu model
                |> Stay

        Just InputController.DownKey ->
            Menu.moveMenuDown model.menu
                |> setMenu model
                |> Stay

        Just InputController.SubmitKey ->
            LoadLevel model.menu.selected.key

        _ ->
            Stay model


view : Model -> Html msg
view model =
    TextRenderer.renderText
        model.width
        model.height
        [ model.menu.items.selected
        ]


setMenu : Model -> Menu.Menu -> Model
setMenu model menu =
    { model | menu = menu }
