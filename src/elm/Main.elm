module Main exposing (main)

import Html exposing (Html, text, br, div, button)
import Html.Events exposing (onClick)
import Keyboard
import Time
import Char
import List.Extra
import Dict exposing (Dict)
import Maybe.Extra
import Color exposing (Color)
import Canvas
import Canvas.Point
import Data.Common exposing (Tick, Position)
import InputController
import Actor exposing (Level)
import UpdateLoop


pixelSize : Int
pixelSize =
    30


defaultCameraBorderSize : Int
defaultCameraBorderSize =
    3


type alias Model =
    { level : Level
    , width : Int
    , height : Int
    , debug : Bool
    , gameSpeed : Maybe Time.Time
    , currentTick : Tick
    , inputController : InputController.Model
    }


type alias Flags =
    { debug : Bool
    , scene : List String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = InputControllerMsg InputController.Msg
    | GameTick Time.Time
    | GameSpeed (Maybe Time.Time)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        width =
            12

        height =
            12

        level =
            List.indexedMap
                (,)
                flags.scene
                |> List.foldr
                    (\( y, line ) level ->
                        List.indexedMap
                            (,)
                            (String.toList line)
                            |> List.foldr
                                (\( x, char ) level ->
                                    case Char.toUpper char of
                                        '#' ->
                                            Actor.addStrongWall x y level

                                        '|' ->
                                            Actor.addWall x y level

                                        '.' ->
                                            Actor.addDirt x y level

                                        'P' ->
                                            Actor.addPlayer x y defaultCameraBorderSize level

                                        'O' ->
                                            Actor.addRock x y level

                                        '0' ->
                                            Actor.addRock x y level

                                        '*' ->
                                            Actor.addDiamond x y level

                                        'E' ->
                                            Actor.addEnemy x y level

                                        '=' ->
                                            Actor.addExplosive x y level

                                        _ ->
                                            level
                                )
                                level
                    )
                    { actors = Dict.fromList []
                    , positionIndex = Dict.fromList []
                    , nextActorId = 1
                    , diamonds =
                        { total = 0
                        , collected = 0
                        }
                    , view =
                        { position = { x = 0, y = 0 }
                        , width = width
                        , height = height
                        }
                    }
    in
        { level = level
        , width = width
        , height = height
        , inputController = InputController.init
        , debug = flags.debug
        , gameSpeed = Nothing -- Just <| 40 * Time.millisecond
        , currentTick = 0
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputControllerMsg subMsg ->
            { model
                | inputController =
                    InputController.update subMsg model.inputController
            }
                ! []

        GameSpeed gameSpeed ->
            { model | gameSpeed = gameSpeed } ! []

        GameTick _ ->
            { model
                | inputController = InputController.resetWasPressed model.inputController
                , level = UpdateLoop.update (InputController.getCurrentDirection model.inputController) model.level
                , currentTick = model.currentTick + 1
            }
                ! []


view : Model -> Html Msg
view model =
    div [] []



{- }
   view : Model -> Html Msg
   view model =
       let
           view =
               model.level.view
       in
           div
               []
               [ Canvas.initialize (Canvas.Size (model.width * pixelSize) (model.height * pixelSize))
                   |> Canvas.batch
                       (List.range view.position.y (view.position.y + view.height - 1)
                           |> List.map
                               (\y ->
                                   List.range view.position.x (view.position.x + view.height - 1)
                                       |> List.map
                                           (\x ->
                                               getPixel model.currentTick view.position { x = x, y = y } model.level
                                                   |> Maybe.withDefault []
                                           )
                                       |> List.concat
                               )
                           |> List.concat
                       )
                   |> Canvas.toHtml []
               , debugView model
               ]


   debugView : Model -> Html Msg
   debugView model =
       if model.debug then
           div
               []
               [ text "Hint: Use the Arrow Keys"
               , br [] []
               , text "GameTick speed:"
               , br [] []
               , div
                   []
                   [ button [ onClick <| GameSpeed Nothing ] [ text "Off" ]
                   , button [ onClick <| GameSpeed (Just <| 10 * Time.second) ] [ text "0.1 fps" ]
                   , button [ onClick <| GameSpeed (Just <| 5 * Time.second) ] [ text "0.5 fps" ]
                   , button [ onClick <| GameSpeed (Just <| 1 * Time.second) ] [ text "1 fps" ]
                   , button [ onClick <| GameSpeed (Just <| 80 * Time.millisecond) ] [ text "12 fps" ]
                   , button [ onClick <| GameSpeed (Just <| 40 * Time.millisecond) ] [ text "24 fps" ]
                   , button [ onClick <| GameSpeed (Just <| 16 * Time.millisecond) ] [ text "60 fps" ]
                   ]
               ]
       else
           text ""


   getPixel : Tick -> Position -> Position -> Level -> Maybe (List Canvas.DrawOp)
   getPixel tick viewPosition position level =
       getActorsThatAffect position level
           |> List.foldr
               (\actor acc ->
                   (getTransformRenderComponent actor.components
                       |> Maybe.andThen
                           (\renderData ->
                               getTransformComponent actor.components
                                   |> Maybe.andThen
                                       (\transformData ->
                                           if transformData.position == position then
                                               case transformData.movingState of
                                                   MovingTowards towardsData ->
                                                       Just <| calculateColor (getColor tick renderData) (100.0 - towardsData.completionPercentage)

                                                   _ ->
                                                       Just (getColor tick renderData)
                                           else
                                               case transformData.movingState of
                                                   MovingTowards towardsData ->
                                                       if towardsData.position == position then
                                                           Just <| calculateColor (getColor tick renderData) towardsData.completionPercentage
                                                       else
                                                           Nothing

                                                   _ ->
                                                       Nothing
                                       )
                           )
                   )
                       :: acc
               )
               []
           |> Maybe.Extra.values
           |> List.foldr
               (\color acc ->
                   case acc of
                       Nothing ->
                           Just color

                       Just accColor ->
                           Just <| combineColors color accColor
               )
               Nothing
           |> Maybe.andThen
               (\color ->
                   Just <| asPixel viewPosition position color
               )


   getColor : Tick -> TransformRenderComponentData -> Color
   getColor tick renderData =
       round ((toFloat tick) / (toFloat (max renderData.ticksPerColor 1)))
           % (max 1 <| List.length renderData.colors)
           |> (flip List.Extra.getAt) renderData.colors
           |> Maybe.withDefault noColor


   noColor : Color
   noColor =
       Color.white


   combineColors : Color -> Color -> Color
   combineColors color1 color2 =
       let
           rgba1 =
               Color.toRgb color1

           rgba2 =
               Color.toRgb color2

           intAlpha1 =
               round (rgba1.alpha * 100.0)

           intAlpha2 =
               round (rgba2.alpha * 100.0)

           combinedRgba =
               { red = round <| (toFloat ((rgba1.red * intAlpha1) + (rgba2.red * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
               , green = round <| (toFloat ((rgba1.green * intAlpha1) + (rgba2.green * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
               , blue = round <| (toFloat ((rgba1.blue * intAlpha1) + (rgba2.blue * intAlpha2))) / (toFloat (max 1 (intAlpha1 + intAlpha2)))
               , alpha = (max rgba1.alpha rgba2.alpha)
               }
       in
           Color.rgba combinedRgba.red combinedRgba.green combinedRgba.blue combinedRgba.alpha


   calculateColor : Color -> Float -> Color
   calculateColor color percentage =
       let
           rgba =
               Color.toRgb color

           newRgba =
               { rgba | alpha = (percentage / 100) }
       in
           Color.rgba newRgba.red newRgba.green newRgba.blue newRgba.alpha


   asPixel : Position -> Position -> Color -> List Canvas.DrawOp
   asPixel viewPosition position color =
       [ Canvas.FillStyle color
       , Canvas.FillRect
           (Canvas.Point.fromInts ( (position.x - viewPosition.x) * pixelSize, (position.y - viewPosition.y) * pixelSize ))
           (Canvas.Size pixelSize pixelSize)
       ]


   subscriptions : Model -> Sub Msg
   subscriptions model =
       let
           sub =
               [ Keyboard.presses KeyPressed
               , Keyboard.downs KeyDown
               , Keyboard.ups KeyUp
               ]

           newSub =
               case model.gameSpeed of
                   Just delay ->
                       Time.every delay GameTick :: sub

                   Nothing ->
                       sub
       in
           Sub.batch newSub
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
