module Actor.RigidComponent exposing (hasRigidComponent)

import Actor.Actor as Actor exposing (Actor)
import Dict


hasRigidComponent : Actor -> Bool
hasRigidComponent actor =
    Dict.member "rigid" actor.components
