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
import Data.Config exposing (Config)
import InputController
import Html exposing (Html, div)
import Renderer.Canvas.TextRenderer as TextRenderer


type alias Model =
    { config : Config
    , menu : Menu.Menu
    }


type Action
    = Stay Model
    | LoadLevel String


init : Config -> Model
init config =
    { config = config
    , menu =
        { items =
            { before = []
            , selected =
                { key = "001"
                , text = Text.stringToLetters "Pixel"
                }
            , after =
                [ { key = "images"
                  , text = Text.stringToLetters "Images"
                  }
                , { key = "pacman"
                  , text = Text.stringToLetters "Pac-Man"
                  }
                , { key = "tank"
                  , text = Text.stringToLetters "Tank"
                  }
                ]
            }
        }
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
        model.config.width
        model.config.height
        [ model.menu.items.selected
        ]


setMenu : Model -> Menu.Menu -> Model
setMenu model menu =
    { model | menu = menu }
