module Actor.Cheats exposing (addBigExplosion)

import Actor.Actor as Actor
    exposing
        ( Component(..)
        , Level
        , LifetimeAction(..)
        , RenderComponentData
        , RenderObject(..)
        )
import Actor.Common as Common
import Color
import Data.Direction as Direction exposing (Direction)
import Data.Position as Position exposing (Position)
import Dict


addExplosion : Int -> Int -> Level -> Level
addExplosion x y level =
    Common.addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y } } )
            , ( "render"
              , RenderComponent <|
                    { object =
                        PixelRenderObject
                            { colors = [ Color.red, Color.darkOrange, Color.yellow ]
                            , ticksPerColor = 4
                            }
                    , layer = 10
                    }
              )
            , ( "lifetime", LifetimeComponent { remainingTicks = 16, action = RemoveActorLifetimeAction } )
            , ( "damage", DamageComponent { damageStrength = 80 } )
            ]
        )
        level


addBigExplosion : Position -> Level -> Level
addBigExplosion position level =
    List.foldr
        (\pos lev ->
            lev |> addExplosion pos.x pos.y
        )
        level
        [ Position.addPositions [ position, Position.getOffsetFromDirection Direction.Left, Position.getOffsetFromDirection Direction.Up ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Up ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Right, Position.getOffsetFromDirection Direction.Up ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Left ]
        , position
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Right ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Left, Position.getOffsetFromDirection Direction.Down ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Down ]
        , Position.addPositions [ position, Position.getOffsetFromDirection Direction.Right, Position.getOffsetFromDirection Direction.Down ]
        ]
