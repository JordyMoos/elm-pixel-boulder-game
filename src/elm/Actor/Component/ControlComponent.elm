module Actor.Component.ControlComponent exposing (updateControlComponent, getControlComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Level
        , Component(ControlComponent)
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , WalkAroundAiControlData
        )
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Actor.Common as Common
import Actor.Component.PhysicsComponent as Physics
import Actor.Component.RigidComponent as Rigid
import Util
import Dict
import List.Extra
import Maybe.Extra


updateControlComponent : Maybe Direction -> ControlComponentData -> Actor -> Level -> Level
updateControlComponent inputControllerDirection controlData actor level =
    if Common.isActorMoving actor |> not then
        getControlDirection inputControllerDirection controlData actor level
            |> Maybe.map
                (\( direction, actor ) ->
                    handleDirection direction actor level
                )
            |> Maybe.withDefault level
    else
        level


getControlComponent : Actor -> Maybe ControlComponentData
getControlComponent actor =
    Dict.get "control" actor.components
        |> Maybe.andThen
            (\component ->
                case component of
                    ControlComponent data ->
                        Just data

                    _ ->
                        Nothing
            )


getPushStrength : Actor -> Int
getPushStrength actor =
    getControlComponent actor
        |> Maybe.map (\data -> data.settings.pushStrength)
        |> Maybe.withDefault 0


getWalkOverStrength : Actor -> Int
getWalkOverStrength actor =
    getControlComponent actor
        |> Maybe.map (\data -> data.settings.walkOverStrength)
        |> Maybe.withDefault 0


getControlDirection : Maybe Direction -> ControlComponentData -> Actor -> Level -> Maybe ( Direction, Actor )
getControlDirection inputControllerDirection controlData actor level =
    case ( controlData.control, inputControllerDirection ) of
        ( InputControl, Just direction ) ->
            Just ( direction, actor )

        ( InputControl, Nothing ) ->
            Nothing

        ( WalkAroundAiControl aiData, _ ) ->
            getWalkAroundAiDirection controlData aiData actor level

        ( GravityAiControl, _ ) ->
            getGravityAiDirection controlData actor level


getWalkAroundAiDirection : ControlComponentData -> WalkAroundAiControlData -> Actor -> Level -> Maybe ( Direction, Actor )
getWalkAroundAiDirection controlData aiData actor level =
    aiData.nextDirectionOffsets
        |> List.map
            (\directionOffset ->
                Direction.getDirectionFromID <| (Direction.getIDFromDirection aiData.previousDirection) - directionOffset
            )
        |> List.Extra.find
            (\direction ->
                canGoInDirection actor direction level
            )
        |> Maybe.map
            (\direction ->
                ( direction
                , Dict.insert
                    "control"
                    (ControlComponent
                        { controlData
                            | control = WalkAroundAiControl <| { aiData | previousDirection = direction }
                        }
                    )
                    actor.components
                    |> Common.updateComponents actor
                )
            )


getGravityAiDirection : ControlComponentData -> Actor -> Level -> Maybe ( Direction, Actor )
getGravityAiDirection controlData actor level =
    Common.getPosition actor
        |> Maybe.map
            (\position ->
                [ ( Direction.Down
                  , [ \() -> canGoInDirection actor Direction.Down level ]
                  )
                , ( Direction.Left
                  , [ \() ->
                        Common.isEmpty
                            (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Left, Position.getOffsetFromDirection Direction.Down ])
                            level
                    , \() -> Physics.isCircleAt (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]) level
                    , \() -> canGoInDirection actor Direction.Left level
                    ]
                  )
                , ( Direction.Right
                  , [ \() ->
                        Common.isEmpty
                            (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Right, Position.getOffsetFromDirection Direction.Down ])
                            level
                    , \() -> Physics.isCircleAt (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]) level
                    , \() -> canGoInDirection actor Direction.Right level
                    ]
                  )
                ]
            )
        |> Maybe.Extra.toList
        |> List.concat
        |> List.Extra.find
            (\( _, predicates ) ->
                Util.lazyAll predicates
            )
        |> Maybe.map
            (\( direction, _ ) ->
                ( direction, actor )
            )


isAllowedToBePushedByAi : Direction -> Actor -> Bool
isAllowedToBePushedByAi direction actor =
    getControlComponent actor
        |> Maybe.map
            (\controlData ->
                case controlData.control of
                    InputControl ->
                        False

                    WalkAroundAiControl data ->
                        False

                    GravityAiControl ->
                        direction /= Direction.Up
            )
        |> Maybe.withDefault True


handleDirection : Direction -> Actor -> Level -> Level
handleDirection direction actor level =
    Common.getTransformComponent actor
        |> Maybe.map
            (\transformData ->
                case Common.getActorsThatAffectNeighborPosition actor direction level of
                    -- No one there
                    [] ->
                        Common.startMovingTowards actor transformData (Position.addPositions [ transformData.position, Position.getOffsetFromDirection direction ]) level

                    -- Only one actor
                    [ otherActor ] ->
                        if canBeWalkedOver actor otherActor then
                            Common.startMovingTowards actor transformData (Position.addPositions [ transformData.position, Position.getOffsetFromDirection direction ]) level
                        else if canPush actor otherActor direction level then
                            Common.getTransformComponent otherActor
                                |> Maybe.map
                                    (\otherTransformData ->
                                        Common.startMovingTowards otherActor otherTransformData (Position.addPositions [ otherTransformData.position, Position.getOffsetFromDirection direction ]) level
                                            |> Common.startMovingTowards actor transformData (Position.addPositions [ transformData.position, Position.getOffsetFromDirection direction ])
                                    )
                                |> Maybe.withDefault level
                        else
                            level

                    -- Multiple actors. There is no implementation for that scenario
                    _ ->
                        level
            )
        |> Maybe.withDefault level


canGoInDirection : Actor -> Direction -> Level -> Bool
canGoInDirection actor direction level =
    case Common.getActorsThatAffectNeighborPosition actor direction level of
        -- No one there
        [] ->
            True

        -- Only one actor
        [ otherActor ] ->
            Util.lazyAny
                [ \() -> canBeWalkedOver actor otherActor
                , \() -> canPush actor otherActor direction level
                ]

        -- Multiple actors. There is no implementation for that scenario
        _ ->
            False


canPush : Actor -> Actor -> Direction -> Level -> Bool
canPush pushingActor toBePushedActor direction level =
    Util.lazyAll
        [ \() -> Rigid.hasRigidComponent pushingActor
        , \() -> Rigid.hasRigidComponent toBePushedActor
        , \() -> Common.isActorMoving toBePushedActor |> not
        , \() -> isAllowedToBePushedByAi direction toBePushedActor
        , \() -> Common.isDestinationEmpty toBePushedActor direction level
        , \() -> hasEnoughPushStrength pushingActor toBePushedActor
        ]


canBeWalkedOver : Actor -> Actor -> Bool
canBeWalkedOver initiatingActor destinationActor =
    Util.lazyAll
        [ \() -> Rigid.hasRigidComponent destinationActor |> not
        , \() -> hasEnoughWalkOverStrength initiatingActor destinationActor
        ]


hasEnoughPushStrength : Actor -> Actor -> Bool
hasEnoughPushStrength initiatingActor destinationActor =
    (getPushStrength initiatingActor) > (Physics.getStrength destinationActor)


hasEnoughWalkOverStrength : Actor -> Actor -> Bool
hasEnoughWalkOverStrength initiatingActor destinationActor =
    (getWalkOverStrength initiatingActor) > (Physics.getStrength destinationActor)
