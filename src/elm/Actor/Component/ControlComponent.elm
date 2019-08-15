module Actor.Component.ControlComponent exposing (getControlComponent, updateControlComponent)

import Actor.Actor as Actor
    exposing
        ( Actor
        , Component(..)
        , ControlComponentData
        , ControlSettings
        , ControlType(..)
        , InputControlData
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


type alias Queue =
    List Direction


type Action
    = Peak Direction
    | Goto Direction Queue


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
    let
        fromQueue : Maybe ( Action, Actor )
        fromQueue =
            case controlData.queue of
                head :: remaining ->
                    Just
                        ( Goto head remaining
                        , actor
                        )

                [] ->
                    Nothing

        newControlAction : () -> Maybe ( Action, Actor )
        newControlAction =
            \() ->
                case controlData.control of
                    InputControl inputControlData ->
                        getInputControlAction controlData inputController inputControlData actor

                    WalkAroundAiControl aiData ->
                        getWalkAroundAiAction controlData aiData actor level

                    GravityAiControl ->
                        getGravityAiAction controlData actor level
    in
    Maybe.Extra.orLazy fromQueue newControlAction


setQueue : Actor -> Queue -> Actor
setQueue actor queue =
    let
        innerSetQueue : ControlComponentData -> Actor
        innerSetQueue controlData =
            actor.components
                |> Dict.insert
                    "control"
                    (ControlComponent
                        { controlData
                            | queue = queue
                        }
                    )
                |> Common.updateComponents actor
    in
    getControlComponent actor
        |> Maybe.map innerSetQueue
        |> Maybe.withDefault actor


withActor : Actor -> Action -> ( Action, Actor )
withActor actor action =
    ( action, actor )


toQueue : ControlComponentData -> Direction -> Queue
toQueue controlData direction =
    List.repeat (controlData.steps - 1) direction


getInputControlAction : ControlComponentData -> InputController.Model -> InputControlData -> Actor -> Maybe ( Action, Actor )
getInputControlAction controlData inputController inputControlData actor =
    InputController.getCurrentDirection inputController inputControlData.allowedDirections
        |> Maybe.map
            (\direction ->
                if InputController.isKeyPressed inputController InputController.submitKey then
                    Peak direction

                else
                    Goto direction (toQueue controlData direction)
            )
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
        |> Maybe.map (\direction -> ( Goto direction (toQueue controlData direction), updateActor direction ))


getGravityAiAction : ControlComponentData -> Actor -> Level -> Maybe ( Action, Actor )
getGravityAiAction controlData actor level =
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
        |> Maybe.map (\( direction, _ ) -> ( Goto direction (toQueue controlData direction), actor ))


isAllowedToBePushedByAi : Direction -> Actor -> Bool
isAllowedToBePushedByAi direction actor =
    getControlComponent actor
        |> Maybe.map
            (\controlData ->
                case controlData.control of
                    InputControl _ ->
                        False

                    WalkAroundAiControl _ ->
                        False

                    GravityAiControl ->
                        direction /= Direction.Up
            )
        |> Maybe.withDefault True


handleAction : Int -> Level -> ( Action, Actor ) -> Level
handleAction currentTick level ( action, actor ) =
    case action of
        Peak direction ->
            handleStationedDirection currentTick level actor direction

        Goto direction queue ->
            handleMovementDirection currentTick level actor direction queue


handleMovementDirection : Int -> Level -> Actor -> Direction -> Queue -> Level
handleMovementDirection currentTick level actor direction queue =
    let
        clearQueueFromActor : () -> Level
        clearQueueFromActor _ =
            setQueue actor []
                |> Common.updateActor level.actors
                |> Common.updateActors level
    in
    case Common.getActorsThatAffectNeighborPosition actor direction level of
        -- No one there
        [] ->
            MovementComponent.startMovingTowards currentTick (setQueue actor queue) direction level

        -- Only one actor
        [ otherActor ] ->
            if canBeWalkedOver actor otherActor then
                MovementComponent.startMovingTowards currentTick (setQueue actor queue) direction level

            else if canPush actor otherActor direction level then
                level
                    |> MovementComponent.startMovingTowards currentTick otherActor direction
                    |> MovementComponent.startMovingTowards currentTick (setQueue actor queue) direction

            else
                clearQueueFromActor ()

        -- Multiple actors. Can only walk over
        multipleActors ->
            let
                canWalkOverAll =
                    List.all (\otherActor -> canBeWalkedOver actor otherActor) multipleActors
            in
            if canWalkOverAll then
                MovementComponent.startMovingTowards currentTick (setQueue actor queue) direction level

            else
                clearQueueFromActor ()


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

        multipleActors ->
            List.all (\otherActor -> canBeWalkedOver actor otherActor) multipleActors


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
