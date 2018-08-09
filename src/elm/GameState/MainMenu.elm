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
    , tick : Int
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
    , tick = 0
    }


updateTick : InputController.Model -> Model -> Action
updateTick inputModel model =
    case InputController.getOrderedPressedKeys inputModel |> List.head of
        Just InputController.UpKey ->
            Menu.moveMenuUp model.menu
                |> setMenu model
                |> resetTick
                |> Stay

        Just InputController.DownKey ->
            Menu.moveMenuDown model.menu
                |> setMenu model
                |> resetTick
                |> Stay

        Just InputController.SubmitKey ->
            LoadLevel model.menu.items.selected.key

        _ ->
            increaseTick model
                |> Stay


increaseTick : Model -> Model
increaseTick model =
    { model | tick = model.tick + 1 }


resetTick : Model -> Model
resetTick model =
    { model | tick = 0 }


view : Model -> Html msg
view model =
    TextRenderer.renderText
        model.config.width
        model.config.height
        [ ( getXOffset model model.menu.items.selected.text, model.menu.items.selected.text )
        ]


getXOffset : Model -> Text.Letters -> Int
getXOffset model letters =
    let
        lineLength =
            getLineLength letters

        beforeLength =
            3

        afterLength =
            1

        totalLength =
            beforeLength + lineLength + afterLength

        minOffset =
            0

        maxOffset =
            max 0 (lineLength - model.config.width)

        tickSpeedCorrection =
            model.tick // 4

        offset =
            (tickSpeedCorrection % totalLength) - beforeLength
    in
        clamp minOffset maxOffset offset
            |> negate


getLineLength : Text.Letters -> Int
getLineLength letters =
    letters
        |> List.map .width
        |> List.sum
        |> (+) (List.length letters)


setMenu : Model -> Menu.Menu -> Model
setMenu model menu =
    { model | menu = menu }
