module Actor.Component.ExplodableComponent exposing (hasExplodableComponent)

import Actor.Actor as Actor exposing (Actor)
import Dict


hasExplodableComponent : Actor -> Bool
hasExplodableComponent actor =
    Dict.member "explodable" actor.components
