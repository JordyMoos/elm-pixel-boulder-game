module Actor.Component.AiComponent exposing (updateAiComponent)

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
import Actor.Component.AiComponent.AdventAiComponent as AdventAiComponent
import Actor.Component.AiComponent.GameOfLifeAiComponent as GameOfLifeAiComponent
import Actor.Component.CollectibleComponent as CollectibleComponent
import Data.Position as Position exposing (Position)
import Dict
import Maybe.Extra
import Pilf exposing (flip)


updateAiComponent : AiComponentData -> Actor -> Actor.Entities -> Level -> Level -> Level
updateAiComponent aiData actor entities levelBeforeUpdate level =
    case aiData.ai of
        GameOfLifeAi gameOfLifeData ->
            GameOfLifeAiComponent.updateGameOfLifeAi aiData gameOfLifeData actor entities levelBeforeUpdate level

        AdventAi adventData ->
            AdventAiComponent.updateAdventAi adventData actor level
