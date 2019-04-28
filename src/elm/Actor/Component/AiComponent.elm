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
import Actor.Component.AiComponent.AdventAiComponent as AdventAiComponent
import Actor.Component.AiComponent.GameOfLifeAiComponent as GameOfLifeAiComponent


updateAiComponent : AiComponentData -> Actor -> Actor.Entities -> Level -> Level -> Level
updateAiComponent aiData actor entities levelBeforeUpdate level =
    case aiData.ai of
        GameOfLifeAi gameOfLifeData ->
            GameOfLifeAiComponent.updateGameOfLifeAi aiData gameOfLifeData actor entities levelBeforeUpdate level

        AdventAi adventData ->
            AdventAiComponent.updateAdventAi adventData actor level
