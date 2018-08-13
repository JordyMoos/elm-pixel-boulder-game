module GameState.PlayingLevel.Animation.Animation
    exposing
        ( Animation(..)
        , Action(..)
        , ReadingDirectionData(..)
        , AddingActorsData
        , WaitingData
        , readingDirectionInit
        , updateTick
        )

import Actor.Actor as Actor exposing (Level)
import Data.Position as Position exposing (Position)
import Data.Config as Config exposing (Config)
import Actor.Common as Common
import Dict
import Color


type Animation
    = ReadingDirection ReadingDirectionData


type ReadingDirectionData
    = AddingActors AddingActorsData
    | Waiting WaitingData


type alias AddingActorsData =
    { positions : List Position
    }


type alias WaitingData =
    { ticksLeft : Int }


type Action
    = Stay Animation Level
    | Finished


readingDirectionInit : Config -> Level -> Animation
readingDirectionInit config level =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    { x = x, y = y }
                )
                (List.range 0 <| config.width - 1)
        )
        (List.range 0 <| config.height - 1)
        |> AddingActorsData
        |> AddingActors
        |> ReadingDirection


updateTick : Animation -> Level -> Action
updateTick animation level =
    case animation of
        ReadingDirection animationData ->
            case animationData of
                AddingActors data ->
                    case data.positions of
                        [] ->
                            Stay (ReadingDirection <| Waiting <| { ticksLeft = 10 }) level

                        position :: otherPositions ->
                            Stay
                                (ReadingDirection <| AddingActors <| { positions = otherPositions })
                                (addBlock
                                    (Position.addPosition position level.view.position)
                                    level
                                )

                Waiting data ->
                    Finished


addBlock : Position -> Level -> Level
addBlock position level =
    Common.addActor
        (Dict.fromList
            [ ( "transform", Actor.TransformComponent { position = position, movingState = Actor.NotMoving } )
            , ( "render", Actor.RenderComponent <| Actor.PixelRenderComponent { colors = [ Color.black ], ticksPerColor = 1 } )
            , ( "damage", Actor.DamageComponent { damageStrength = 1000 } )
            , ( "rigid", Actor.RigidComponent )
            , ( "physics", Actor.PhysicsComponent { strength = 1000, shape = Actor.Square } )
            ]
        )
        level
