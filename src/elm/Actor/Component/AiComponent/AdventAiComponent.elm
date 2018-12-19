module Actor.Component.AiComponent.AdventAiComponent exposing (updateAdventAi)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AdventAiData
        , AiComponentData
        , AiType(..)
        , Components
        , HealthComponentData
        , Level
        )
import Actor.Common as Common
import Actor.Component.AiComponent.Common as CommonAi
import Actor.Component.HealthComponent as HealthComponent
import Actor.Component.TagComponent as TagComponent
import Data.Position as Position exposing (Position)
import Dict exposing (Dict)
import Maybe.Extra
import Pilf exposing (flip)
import Set


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
