module Actor.Component.DamageComponent exposing (DamageComponentData)

import Data.Common exposing (Tick)


type alias DamageComponentData =
    { remainingTicks : Tick
    }
