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



{-

   #######
   #.G...#
   #...EG#
   #.#.#G#
   #..G#E#
   #.....#
   #######

-}


type alias Path =
    { directions : List Direction
    , currentPosition : Position
    }


type alias TargetSearch =
    { foundPositions : Dict ( X, Y ) ()
    , paths : List Path
    }


type SearchResult
    = Attack Actor
    | Move Direction
    | Idle


type SearchAcc
    = Stop SearchResult
    | Continue TargetSearch


type alias Enemy =
    { path : Path
    , health : Health
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
        |> Maybe.map (handleAction actor level)
        |> Maybe.withDefault level


handleAction : Actor -> Level -> SearchResult -> Level
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

        Idle ->
            level


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
        remainingHealth =
            attackData.enemyHealth.health - attackData.myAttack.power
    in
    if remainingHealth < 1 then
        handleEnemyDied level enemyActor

    else
        handleEnemySurvived level enemyActor remainingHealth


handleEnemyDied : Level -> Actor -> Level
handleEnemyDied level enemy =
    Common.removeActor enemy level


handleEnemySurvived : Level -> Actor -> Int -> Level
handleEnemySurvived level enemy health =
    Dict.insert
        "health"
        (Actor.HealthComponent
            { health = health
            }
        )
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


search : Level -> String -> TargetSearch -> SearchResult
search level enemyTag targetSearch =
    List.range 0 30
        |> List.foldl
            (\step acc -> move level enemyTag step acc)
            (Continue targetSearch)
        |> asSearchResult


asSearchResult : SearchAcc -> SearchResult
asSearchResult acc =
    case acc of
        Stop result ->
            result

        Continue _ ->
            Idle


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
                                ( enemy.health, enemy.path.currentPosition.y, enemy.path.currentPosition.x )
                            )
                        --                        |> (\sortedEnemies ->
                        --                                let
                        --                                    _ =
                        --                                        Debug.log "Sorted enemies" (Debug.toString sortedEnemies)
                        --                                in
                        --                                sortedEnemies
                        --                           )
                        |> List.head
                        |> Maybe.map (\enemy -> Stop (Attack enemy.actor))
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
                        |> Maybe.map (\direction -> Stop (Move direction))
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


asEnemy : Path -> ( Actor, Health ) -> Enemy
asEnemy path ( actor, health ) =
    { path = path
    , health = health
    , actor = actor
    }


withHealth : Actor -> Maybe ( Actor, Health )
withHealth actor =
    HealthComponent.getHealthComponent actor
        |> Maybe.map (\healthData -> ( actor, healthData.health ))



{-

   Check all positions to be either:
   - Empty -> Keep the path
   - Target Tag -> Will bring to the next stage
   - Not empty BUT not the target -> Remove the path


   Lets split:
   Go over all currentPositions to find list of enemies
   - If found -> Pick enemy
   - If not found -> Continue + remove non empty positions

-}


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



{-
   type alias Enemy =
       { position : Position
       , health : HealthComponentData
       , actor : Actor
       }


   type alias X =
       Int


   type alias Y =
       Int


   type alias FindEnemyAcc =
       { foundPositions : Dict ( X, Y ) ()
       , tracks : List
       }


   updateAdventAi : AiComponentData -> AdventAiData -> Actor -> Level -> Level
   updateAdventAi aiData adventData actor level =
       findNearestEnemy adventData.target actor level
           |> Maybe.map (moveOrAttack actor level)
           |> Maybe.withDefault level


   findNearestEnemy : String -> Actor -> Level -> Maybe Enemy
   findNearestEnemy enemyTag actor level =
       Common.getTransformComponent actor
           |> Maybe.map .position
           |> Maybe.andThen (findNearestEnemyNearPosition enemyTag level)


   findNearestEnemyNearPosition : String -> Level -> Position -> Maybe Enemy
   findNearestEnemyNearPosition enemyTag level position =
       List.range 0 30
           |> List.foldl
               (findEnemyWithingStep enemyTag level position)
               []
           |> pickEnemy


   findEnemyWithingStep : String -> Level -> Position -> Int -> FindEnemyAcc -> FindEnemyAcc
   findEnemyWithingStep enemyTag level position step acc =
       toPositionOffsetList step
           |> Position.fromTuple
           |> Position.addPosition position
           |> List.concatMap (flip Common.getActorsThatAffect level)
           |> List.filter (TagComponent.isTag enemyTag)
           |> List.filterMap withPosition
           |> List.filterMap withHealth
           |> List.map asEnemy


   asEnemy : ( Position, HealthComponentData, Actor ) -> Enemy
   asEnemy ( position, healthData, actor ) =
       { position = position
       , health = healthData
       , actor = actor
       }


   withPosition : Actor -> Maybe ( Position, Actor )
   withPosition actor =
       Common.getPosition actor
           |> Maybe.map (\position -> ( position, actor ))


   withHealth : ( Position, Actor ) -> Maybe ( Position, HealthComponentData, Actor )
   withHealth ( position, actor ) =
       HealthComponent.getHealthComponent actor
           |> Maybe.map (\healthData -> ( position, healthData, actor ))


   pickEnemy : FindEnemyAcc -> Maybe Enemy
   pickEnemy acc =
       Nothing


   moveOrAttack : Actor -> Level -> Enemy -> Level
   moveOrAttack actor level enemy =
       level


   toPositionOffsetList : Int -> List ( Int, Int )
   toPositionOffsetList step =
       List.range (step * -1) step
           |> List.foldl
               (\x acc ->
                   List.append
                       [ ( x, step - abs x )
                       , ( x, (step - abs x) * -1 )
                       ]
                       acc
               )
               []
           |> removeDuplicates
           |> List.sortBy sortByReadingOrder


   removeDuplicates : List comparable -> List comparable
   removeDuplicates =
       Set.fromList >> Set.toList


   sortByReadingOrder : ( Int, Int ) -> ( Int, Int )
   sortByReadingOrder ( x, y ) =
       ( y, x )
-}
