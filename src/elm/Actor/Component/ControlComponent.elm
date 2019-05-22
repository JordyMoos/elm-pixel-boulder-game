module Actor.Component.ControlComponent exposing (getControlComponent, updateControlComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , Level
        , WalkAroundAiControlData
        )
import Actor.Common as Common
import Actor.Component.CollectorComponent as CollectorComponent
import Actor.Component.MovementComponent as MovementComponent
import Actor.Component.PhysicsComponent as Physics
import Actor.Component.RigidComponent as Rigid
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict
import InputController
import List.Extra
import Maybe.Extra
import Util.Util as Util


type alias Action =
    { direction : Direction
    , peak : Bool
    }


updateControlComponent : Int -> InputController.Model -> ControlComponentData -> Actor -> Level -> Level
updateControlComponent currentTick inputController controlData actor level =
    if MovementComponent.isActorMoving actor |> not then
        getControlAction inputController controlData actor level
            |> Maybe.map (handleAction currentTick level)
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


getControlAction : InputController.Model -> ControlComponentData -> Actor -> Level -> Maybe ( Action, Actor )
getControlAction inputController controlData actor level =
    case controlData.control of
        InputControl ->
            getInputControlAction inputController actor

        WalkAroundAiControl aiData ->
            getWalkAroundAiAction controlData aiData actor level

        GravityAiControl ->
            getGravityAiAction actor level


withActor : Actor -> Action -> ( Action, Actor )
withActor actor action =
    ( action, actor )


getInputControlAction : InputController.Model -> Actor -> Maybe ( Action, Actor )
getInputControlAction inputController actor =
    InputController.getCurrentDirection inputController
        |> Maybe.map (\direction -> { direction = direction, peak = InputController.isKeyPressed inputController InputController.submitKey })
        |> Maybe.map (withActor actor)


getWalkAroundAiAction : ControlComponentData -> WalkAroundAiControlData -> Actor -> Level -> Maybe ( Action, Actor )
getWalkAroundAiAction controlData aiData actor level =
    let
        positionFromDirectionOffset : Int -> Direction
        positionFromDirectionOffset =
            \directionOffset ->
                Direction.getIDFromDirection aiData.previousDirection
                    - directionOffset
                    |> Direction.getDirectionFromID

        updateActor : Direction -> Actor
        updateActor =
            \direction ->
                actor.components
                    |> Dict.insert
                        "control"
                        (ControlComponent
                            { controlData
                                | control = WalkAroundAiControl <| { aiData | previousDirection = direction }
                            }
                        )
                    |> Common.updateComponents actor
    in
    aiData.nextDirectionOffsets
        |> List.map positionFromDirectionOffset
        |> List.Extra.find (\direction -> canGoInDirection actor direction level)
        |> Maybe.map (\direction -> ( { direction = direction, peak = False }, updateActor direction ))


getGravityAiAction : Actor -> Level -> Maybe ( Action, Actor )
getGravityAiAction actor level =
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
                    , \() -> MovementComponent.isNotMovingAt (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]) level
                    , \() -> canGoInDirection actor Direction.Left level
                    ]
                  )
                , ( Direction.Right
                  , [ \() ->
                        Common.isEmpty
                            (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Right, Position.getOffsetFromDirection Direction.Down ])
                            level
                    , \() -> Physics.isCircleAt (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]) level
                    , \() -> MovementComponent.isNotMovingAt (Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]) level
                    , \() -> canGoInDirection actor Direction.Right level
                    ]
                  )
                ]
            )
        |> Maybe.Extra.toList
        |> Util.fastConcat
        |> List.Extra.find (\( _, predicates ) -> Util.lazyAll predicates)
        |> Maybe.map (\( direction, _ ) -> ( { direction = direction, peak = False }, actor ))


isAllowedToBePushedByAi : Direction -> Actor -> Bool
isAllowedToBePushedByAi direction actor =
    getControlComponent actor
        |> Maybe.map
            (\controlData ->
                case controlData.control of
                    InputControl ->
                        False

                    WalkAroundAiControl _ ->
                        False

                    GravityAiControl ->
                        direction /= Direction.Up
            )
        |> Maybe.withDefault True


handleAction : Int -> Level -> ( Action, Actor ) -> Level
handleAction currentTick level ( action, actor ) =
    if action.peak then
        handleStationedDirection currentTick level actor action.direction

    else
        handleMovementDirection currentTick level actor action.direction


handleMovementDirection : Int -> Level -> Actor -> Direction -> Level
handleMovementDirection currentTick level actor direction =
    case Common.getActorsThatAffectNeighborPosition actor direction level of
        -- No one there
        [] ->
            MovementComponent.startMovingTowards currentTick actor direction level

        -- Only one actor
        [ otherActor ] ->
            if canBeWalkedOver actor otherActor then
                MovementComponent.startMovingTowards currentTick actor direction level

            else if canPush actor otherActor direction level then
                level
                    |> MovementComponent.startMovingTowards currentTick otherActor direction
                    |> MovementComponent.startMovingTowards currentTick actor direction

            else
                level

        -- Multiple actors. There is no implementation for that scenario
        _ ->
            level


handleStationedDirection : Int -> Level -> Actor -> Direction -> Level
handleStationedDirection currentTick level actor direction =
    case Common.getActorsThatAffectNeighborPosition actor direction level of
        -- No one there, nothing to peak for then
        [] ->
            level

        -- Only one actor
        [ otherActor ] ->
            if canPush actor otherActor direction level then
                MovementComponent.startMovingTowards currentTick otherActor direction level

            else
                Common.getPosition otherActor
                    |> Maybe.map (\otherActorPosition -> CollectorComponent.tryCollectPosition actor otherActorPosition level)
                    |> Maybe.withDefault level

        -- Multiple actors. There is no implementation for that scenario
        _ ->
            level


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
        , \() -> MovementComponent.isActorMoving toBePushedActor |> not
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
    getPushStrength initiatingActor > Physics.getStrength destinationActor


hasEnoughWalkOverStrength : Actor -> Actor -> Bool
hasEnoughWalkOverStrength initiatingActor destinationActor =
    getWalkOverStrength initiatingActor > Physics.getStrength destinationActor
