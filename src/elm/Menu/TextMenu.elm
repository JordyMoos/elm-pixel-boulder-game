module Menu.TextMenu exposing
    ( Action(..)
    , Item
    , Model
    , TextMenu
    , init
    , updateTick
    , view
    )

import Data.Config exposing (Config)
import Data.Menu as Menu
import Html exposing (Html)
import InputController
import Renderer.Svg.MenuRenderer as MenuRenderer
import Text


type alias Model a =
    { config : Config
    , menu : TextMenu a
    , tick : Int
    , delay : Int
    }


type alias Item a =
    { text : Text.Letters
    , action : a
    }


type alias TextMenu a =
    Menu.Menu (Item a)


type Action a
    = Stay (Model a)
    | Invoke a


defaultDelay : Int
defaultDelay = 12

init : Config -> TextMenu a -> Model a
init config menu =
    { config = config
    , menu = menu
    , tick = 0
    , delay = defaultDelay
    }


updateTick : InputController.Model -> Model a -> Action a
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
                Invoke model.menu.items.selected.action

            _ ->
                increaseTick model
                    |> Stay


view : Model a -> Html msg
view model =
    MenuRenderer.render model.config model.tick model.menu


increaseTick : Model a -> Model a
increaseTick model =
    { model | tick = model.tick + 1 }


resetTick : Model a -> Model a
resetTick model =
    { model | tick = 0 }


decreaseDelay : Model a -> Model a
decreaseDelay model =
    { model | delay = model.delay - 1 }


setDelay : Model a -> Model a
setDelay model =
    { model | delay = defaultDelay }


setMenu : Model a -> TextMenu a -> Model a
setMenu model menu =
    { model | menu = menu }
