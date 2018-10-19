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
import Actor.Component.CollectibleComponent as CollectibleComponent
import Dict
import Maybe.Extra


updateAiComponent : AiComponentData -> Actor -> Level -> Level -> Level
updateAiComponent aiData actor levelBeforeUpdate level =
    case aiData.ai of
        GameOfLifeAi gameOfLifeData ->
            updateGameOfLifeAi aiData gameOfLifeData actor levelBeforeUpdate level


updateGameOfLifeAi : AiComponentData -> GameOfLifeAiData -> Actor -> Level -> Level -> Level
updateGameOfLifeAi aiData gameOfLifeData actor levelBeforeUpdate level =
    level
