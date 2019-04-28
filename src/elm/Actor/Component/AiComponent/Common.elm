module Actor.Component.AiComponent.Common exposing (setAiComponent)

import Actor.Actor exposing (AiComponentData, AiType)


setAiComponent : AiComponentData -> AiType -> AiComponentData
setAiComponent aiData aiType =
    { aiData | ai = aiType }
