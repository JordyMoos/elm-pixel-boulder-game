module Actor.Component.AiComponent.AdventAiComponent exposing (updateAdventAi)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AdventAiData
        , AiComponentData
        , AiType(..)
        , Components
        , Level
        )
import Actor.Common as Common
import Actor.Component.AiComponent.Common as CommonAi
import Actor.Component.CollectibleComponent as CollectibleComponent
import Data.Position as Position exposing (Position)
import Dict
import Maybe.Extra
import Pilf exposing (flip)
import Set


type alias Enemy =
    {}


type alias FindEnemyAcc =
    {}


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
            {}
        |> pickEnemy


findEnemyWithingStep : String -> Level -> Position -> Int -> FindEnemyAcc -> FindEnemyAcc
findEnemyWithingStep enemyTag level position step acc =
    List.range 0 step
        |> List.foldl


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
