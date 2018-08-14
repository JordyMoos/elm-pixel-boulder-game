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
import List.Extra


type Animation
    = ReadingDirection ReadingDirectionData


type ReadingDirectionData
    = AddingActors AddingActorsData
    | Waiting WaitingData


type alias AddingActorsData =
    { positions : List Position
    , entities : Actor.Entities
    , entityNames : List String
    }


type alias WaitingData =
    { ticksLeft : Int }


type Action
    = Stay Animation Level
    | Finished


readingDirectionInit : Config -> Actor.Entities -> Actor.LevelFailedData -> Level -> Animation
readingDirectionInit config entities data level =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    { x = x, y = y }
                )
                (List.range 0 <| config.width - 1)
        )
        (List.range 0 <| config.height - 1)
        |> (\positions ->
                { positions = positions
                , entities = entities
                , entityNames = data.entityNames
                }
           )
        |> AddingActors
        |> ReadingDirection


updateTick : Int -> Animation -> Level -> Action
updateTick currentTick animation level =
    case animation of
        ReadingDirection animationData ->
            case animationData of
                AddingActors addingActorsData ->
                    case addingActorsData.positions of
                        [] ->
                            Stay (ReadingDirection <| Waiting <| { ticksLeft = 10 }) level

                        position :: otherPositions ->
                            Stay
                                (ReadingDirection <| AddingActors <| { addingActorsData | positions = otherPositions })
                                (addActor
                                    currentTick
                                    addingActorsData
                                    (Position.addPosition position level.view.position)
                                    level
                                )

                Waiting data ->
                    Finished


addActor : Int -> AddingActorsData -> Position -> Level -> Level
addActor currentTick addingActorsData position level =
    getEntityName currentTick addingActorsData.entityNames
        |> Maybe.andThen (flip Dict.get addingActorsData.entities)
        |> Maybe.map
            (\entity ->
                Common.addActor
                    (Dict.insert
                        "transform"
                        (Actor.TransformComponent { position = position, movingState = Actor.NotMoving })
                        entity
                    )
                    level
            )
        |> Maybe.withDefault level


getEntityName : Int -> List String -> Maybe String
getEntityName currentTick entityNames =
    List.Extra.getAt
        (currentTick % (List.length entityNames))
        entityNames
