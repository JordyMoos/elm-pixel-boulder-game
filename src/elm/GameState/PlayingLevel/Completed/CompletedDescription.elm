module GameState.PlayingLevel.Completed.CompletedDescription
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
import Actor.Actor as Actor
import Menu.TextMenu as TextMenu


type alias Model =
    { textMenu : TextMenu.Model Action
    }


type Action
    = Stay Model
    | Finished String


init : Config -> String -> String -> Model
init config description nextLevel =
    { textMenu =
        TextMenu.init config
            { items =
                { before = []
                , selected =
                    { text = Text.stringToLetters description
                    , action = Finished nextLevel
                    }
                , after = []
                }
            }
    }


updateTick : InputController.Model -> Model -> Action
updateTick inputModel model =
    case TextMenu.updateTick inputModel model.textMenu of
        TextMenu.Stay textMenu ->
            Stay { model | textMenu = textMenu }

        TextMenu.Invoke action ->
            action


view : Model -> Html msg
view model =
    TextMenu.view model.textMenu
