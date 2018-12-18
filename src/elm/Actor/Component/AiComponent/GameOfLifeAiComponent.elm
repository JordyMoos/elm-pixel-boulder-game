module Actor.Component.AiComponent.GameOfLifeAiComponent exposing (updateGameOfLifeAi)

import Actor.Actor as Actor
    exposing
        ( Actor
        , AiComponentData
        , AiType(..)
        , Components
        , GameOfLifeAiAction
        , GameOfLifeAiData
        , Level
        )
import Actor.Common as Common
import Actor.Component.AiComponent.Common as CommonAi
import Actor.Component.CollectibleComponent as CollectibleComponent
import Data.Position as Position exposing (Position)
import Dict
import Maybe.Extra
import Pilf exposing (flip)


updateGameOfLifeAi : AiComponentData -> GameOfLifeAiData -> Actor -> Actor.Entities -> Level -> Level -> Level
updateGameOfLifeAi aiData gameOfLifeData actor entities levelBeforeUpdate level =
    if gameOfLifeData.delayTicks > 0 then
        updateDelay (gameOfLifeData.delayTicks - 1) aiData gameOfLifeData actor level

    else
        timeToBecome aiData gameOfLifeData actor entities levelBeforeUpdate level


updateDelay : Int -> AiComponentData -> GameOfLifeAiData -> Actor -> Level -> Level
updateDelay delayTicks aiData gameOfLifeData actor level =
    setDelay gameOfLifeData delayTicks
        |> GameOfLifeAi
        |> CommonAi.setAiComponent aiData
        |> Actor.AiComponent
        |> (\aiComponent -> Dict.insert "ai" aiComponent actor.components)
        |> Common.updateComponents actor
        |> Common.updateActor level.actors
        |> Common.updateActors level


timeToBecome : AiComponentData -> GameOfLifeAiData -> Actor -> Actor.Entities -> Level -> Level -> Level
timeToBecome aiData gameOfLifeData actor entities levelBeforeUpdate level =
    Common.getPosition actor
        |> Maybe.map (getSearchTagCount levelBeforeUpdate gameOfLifeData.tagToSearch)
        |> Maybe.andThen (findBecome gameOfLifeData.actions)
        |> Maybe.andThen (flip Dict.get entities)
        |> Maybe.map (doBecome actor)
        |> Maybe.map (Common.updateActor level.actors)
        |> Maybe.map (Common.updateActors level)
        |> Maybe.withDefault (updateDelay gameOfLifeData.delayTicksInitially aiData gameOfLifeData actor level)


getSearchTagCount : Level -> String -> Position -> Int
getSearchTagCount levelBeforeUpdate tagName myPosition =
    Position.aroundNeighborOffsets
        |> List.map (Position.addPosition myPosition)
        |> List.concatMap (flip Common.getActorsByPosition levelBeforeUpdate)
        |> List.map Common.getTagComponent
        |> Maybe.Extra.values
        |> List.map .name
        |> List.filter ((==) tagName)
        |> List.length


findBecome : List GameOfLifeAiAction -> Int -> Maybe String
findBecome actions count =
    actions
        |> List.filter (\action -> action.count == count)
        |> List.head
        |> Maybe.map .become


doBecome : Actor -> Actor.Components -> Actor
doBecome actor newComponents =
    Dict.union newComponents actor.components
        |> Common.updateComponents actor


setDelay : GameOfLifeAiData -> Int -> GameOfLifeAiData
setDelay gameOfLifeData delayTicks =
    { gameOfLifeData | delayTicks = delayTicks }
