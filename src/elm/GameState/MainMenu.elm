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
import Renderer.Canvas.MenuRenderer as MenuRenderer
import List.Extra
import Maybe.Extra
import Color


type alias Model =
    { config : Config
    , menu : Menu.Menu Item
    , tick : Int
    , delay : Int
    }


type alias Item =
    { text : Text.Letters
    , action : Action
    }


type Action
    = Stay Model
    | LoadLevel String
    | LoadFlags


init : Config -> Model
init config =
    { config = config
    , menu =
        { items =
            { before = []
            , selected =
                { text = Text.stringToLetters "Json"
                , action = LoadFlags
                }
            , after =
                [ { text = Text.stringToLetters "Pixel"
                  , action = LoadLevel "pixel"
                  }
                , { text = Text.stringToLetters "Images"
                  , action = LoadLevel "images"
                  }
                , { text = Text.stringToLetters "Pac-Man"
                  , action = LoadLevel "pacman"
                  }
                , { text = Text.stringToLetters "Tank"
                  , action = LoadLevel "tank"
                  }
                ]
            }
        }
    , tick = 0
    , delay = 0
    }


updateTick : InputController.Model -> Model -> Action
updateTick inputModel model =
    if model.delay > 0 then
        model
            |> decreaseDelay
            |> Stay
    else
        case InputController.getOrderedPressedKeys inputModel |> List.head of
            Just InputController.UpKey ->
                Menu.moveMenuUp model.menu
                    |> setMenu model
                    |> resetTick
                    |> setDelay
                    |> Stay

            Just InputController.DownKey ->
                Menu.moveMenuDown model.menu
                    |> setMenu model
                    |> resetTick
                    |> setDelay
                    |> Stay

            Just InputController.SubmitKey ->
                model.menu.items.selected.action

            _ ->
                increaseTick model
                    |> Stay


increaseTick : Model -> Model
increaseTick model =
    { model | tick = model.tick + 1 }


resetTick : Model -> Model
resetTick model =
    { model | tick = 0 }


decreaseDelay : Model -> Model
decreaseDelay model =
    { model | delay = model.delay - 1 }


setDelay : Model -> Model
setDelay model =
    { model | delay = 4 }


view : Model -> Html msg
view model =
    MenuRenderer.render model.config model.tick model.menu


setMenu : Model -> Menu.Menu Item -> Model
setMenu model menu =
    { model | menu = menu }
