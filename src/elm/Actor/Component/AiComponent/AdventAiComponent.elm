module Actor.Component.AiComponent.AdventAiComponent exposing (updateAdventAi)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AdventAiData
        , AiComponentData
        , AiType(..)
        , AttackComponentData
        , Components
        , Health
        , HealthComponentData
        , Level
        )
import Actor.Common as Common
import Actor.Component.AiComponent.Common as CommonAi
import Actor.Component.AttackComponent as AttackComponent
import Actor.Component.HealthComponent as HealthComponent
import Actor.Component.TagComponent as TagComponent
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position, X, Y)
import Dict exposing (Dict)
import Maybe.Extra
import Pilf exposing (flip)
import Set


type alias Path =
    { directions : List Direction
    , currentPosition : Position
    }


type alias TargetSearch =
    { foundPositions : Dict ( X, Y ) ()
    , paths : List Path
    }


type Action
    = Attack Actor
    | Move Direction


type SearchAcc
    = Stop (List Action)
    | Continue TargetSearch


type alias Enemy =
    { path : Path
    , healthData : HealthComponentData
    , actor : Actor
    }


orderedDirections : List Direction
orderedDirections =
    [ Direction.Up
    , Direction.Left
    , Direction.Right
    , Direction.Down
    ]


updateAdventAi : AdventAiData -> Actor -> Level -> Level
updateAdventAi adventData actor level =
    Common.getPosition actor
        |> Maybe.map setupSearch
        |> Maybe.map (search level adventData.target)
        |> Maybe.map (handleActions actor level)
        |> Maybe.withDefault level


handleActions : Actor -> Level -> List Action -> Level
handleActions actor level =
    List.foldl
        (\action accLevel ->
            handleAction actor accLevel action
        )
        level


handleAction : Actor -> Level -> Action -> Level
handleAction actor level searchResult =
    --    let
    --        _ =
    --            Debug.log "Actor" (Debug.toString actor)
    --
    --        _ =
    --            Debug.log "Action" (Debug.toString searchResult)
    --    in
    case searchResult of
        Attack enemyActor ->
            handleAttack actor level enemyActor

        Move direction ->
            handleMove actor level direction


type alias HandleAttackData =
    { myAttack : AttackComponentData
    , enemyHealth : HealthComponentData
    }


handleAttack : Actor -> Level -> Actor -> Level
handleAttack actor level enemyActor =
    Maybe.map2
        HandleAttackData
        (AttackComponent.getAttackComponent actor)
        (HealthComponent.getHealthComponent enemyActor)
        |> Maybe.map (doAttack level enemyActor)
        |> Maybe.withDefault level


doAttack : Level -> Actor -> HandleAttackData -> Level
doAttack level enemyActor attackData =
    let
        enemyHealth =
            attackData.enemyHealth

        remainingHealth =
            enemyHealth.health - attackData.myAttack.power
    in
    if remainingHealth < 1 then
        handleEnemyDied level enemyActor

    else
        handleEnemySurvived level enemyActor { enemyHealth | health = remainingHealth }


handleEnemyDied : Level -> Actor -> Level
handleEnemyDied level enemy =
    Common.removeActor enemy level


handleEnemySurvived : Level -> Actor -> HealthComponentData -> Level
handleEnemySurvived level enemy healthData =
    Dict.insert
        "health"
        (Actor.HealthComponent healthData)
        enemy.components
        |> Common.updateComponents enemy
        |> Common.updateActor level.actors
        |> Common.updateActors level


handleMove : Actor -> Level -> Direction -> Level
handleMove actor level direction =
    Common.getPosition actor
        |> Maybe.map
            (\oldPosition ->
                ( oldPosition, Position.addDirection oldPosition direction )
            )
        |> Maybe.map
            (\( oldPosition, newPosition ) ->
                setNewPosition actor level oldPosition newPosition
            )
        |> Maybe.withDefault level


setNewPosition : Actor -> Level -> Position -> Position -> Level
setNewPosition actor level oldPosition newPosition =
    Dict.insert
        "transform"
        (Actor.TransformComponent
            { position = newPosition
            , movingState = Actor.NotMoving
            }
        )
        actor.components
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level
        |> Common.removeActorFromIndexByPosition oldPosition actor.id
        |> Common.addActorToIndex newPosition actor.id


setupSearch : Position -> TargetSearch
setupSearch position =
    { foundPositions = Dict.fromList [ ( ( position.x, position.y ), () ) ]
    , paths =
        [ { directions = []
          , currentPosition = position
          }
        ]
    }


search : Level -> String -> TargetSearch -> List Action
search level enemyTag targetSearch =
    List.range 0 30
        |> List.foldl
            (\step acc -> move level enemyTag step acc)
            (Continue targetSearch)
        |> asActions


asActions : SearchAcc -> List Action
asActions acc =
    case acc of
        Stop actions ->
            actions

        Continue _ ->
            []


move : Level -> String -> Int -> SearchAcc -> SearchAcc
move level enemyTag step acc =
    case acc of
        Stop searchResult ->
            Stop searchResult

        Continue targetSearch ->
            let
                newTargetSearch =
                    walkOneStep targetSearch
            in
            case ( step, getEnemiesOnCurrentPositions level newTargetSearch enemyTag ) of
                ( _, [] ) ->
                    newTargetSearch.paths
                        |> List.filter
                            (\path ->
                                Common.isEmpty path.currentPosition level
                            )
                        |> (\newPaths ->
                                Continue { newTargetSearch | paths = newPaths }
                           )

                -- Enemy is near, we should fight!
                -- Sort on health
                ( 0, enemies ) ->
                    enemies
                        |> List.sortBy
                            (\enemy ->
                                ( enemy.healthData.health, enemy.path.currentPosition.y, enemy.path.currentPosition.x )
                            )
                        |> List.head
                        |> Maybe.map (\enemy -> Stop [ Attack enemy.actor ])
                        -- Should not happen
                        |> Maybe.withDefault (Continue newTargetSearch)

                -- Enemy is near, we should walk and then fight!
                -- Sort on health
                ( 1, enemies ) ->
                    enemies
                        |> List.sortBy
                            (\enemy ->
                                ( enemy.healthData.health, enemy.path.currentPosition.y, enemy.path.currentPosition.x )
                            )
                        |> List.head
                        |> Maybe.andThen
                            (\enemy ->
                                case enemy.path.directions of
                                    firstDirection :: _ ->
                                        Just ( firstDirection, enemy )

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.map (\( direction, enemy ) -> Stop [ Move direction, Attack enemy.actor ])
                        -- Should not happen
                        |> Maybe.withDefault (Continue newTargetSearch)

                -- Enemy is too far
                -- Sort on position y,x
                ( x, enemies ) ->
                    enemies
                        |> List.sortBy
                            (\enemy ->
                                ( enemy.path.currentPosition.y, enemy.path.currentPosition.x )
                            )
                        |> List.head
                        |> Maybe.andThen (\enemy -> List.head enemy.path.directions)
                        |> Maybe.map (\direction -> Stop [ Move direction ])
                        -- Should not happen
                        |> Maybe.withDefault (Continue newTargetSearch)


getEnemiesOnCurrentPositions : Level -> TargetSearch -> String -> List Enemy
getEnemiesOnCurrentPositions level targetSearch enemyTag =
    List.concatMap (getEnemiesOnPath level enemyTag) targetSearch.paths


getEnemiesOnPath : Level -> String -> Path -> List Enemy
getEnemiesOnPath level enemyTag path =
    Common.getActorsByPosition path.currentPosition level
        |> List.filter (TagComponent.isTag enemyTag)
        |> List.filterMap withHealth
        |> List.map (asEnemy path)


asEnemy : Path -> ( Actor, HealthComponentData ) -> Enemy
asEnemy path ( actor, healthData ) =
    { path = path
    , healthData = healthData
    , actor = actor
    }


withHealth : Actor -> Maybe ( Actor, HealthComponentData )
withHealth actor =
    HealthComponent.getHealthComponent actor
        |> Maybe.map (\healthData -> ( actor, healthData ))


walkOneStep : TargetSearch -> TargetSearch
walkOneStep targetSearch =
    List.foldl
        (\path acc ->
            let
                newPaths =
                    orderedDirections
                        |> List.map
                            (\direction ->
                                { path
                                    | directions = List.append path.directions [ direction ]
                                    , currentPosition = Position.addDirection path.currentPosition direction
                                }
                            )
                        |> List.filter
                            (\newPath ->
                                Dict.member ( newPath.currentPosition.x, newPath.currentPosition.y ) acc.foundPositions
                                    |> not
                            )

                newFoundPositions =
                    List.foldl
                        (\newPath foundPositions ->
                            Dict.insert ( newPath.currentPosition.x, newPath.currentPosition.y ) () foundPositions
                        )
                        acc.foundPositions
                        newPaths
            in
            { acc
                | paths = List.append acc.paths newPaths
                , foundPositions = newFoundPositions
            }
        )
        { targetSearch | paths = [] }
        targetSearch.paths
