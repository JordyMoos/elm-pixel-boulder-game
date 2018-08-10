module Actor.Cheats exposing (addBigExplosion)

import Actor.Actor as Actor
    exposing
        ( Level
        , Component(TransformComponent, RenderComponent, LifetimeComponent, DamageComponent)
        , RenderComponentData(PixelRenderComponent)
        )
import Dict
import Actor.Common as Common
import Data.Position as Position exposing (Position)
import Data.Direction as Direction exposing (Direction)
import Color


addExplosion : Int -> Int -> Level -> Level
addExplosion x y level =
    Common.addActor
        (Dict.fromList
            [ ( "transform", TransformComponent { position = { x = x, y = y }, movingState = Actor.NotMoving } )
            , ( "render", RenderComponent <| PixelRenderComponent { colors = [ Color.red, Color.darkOrange, Color.yellow ], ticksPerColor = 2 } )
            , ( "lifetime", LifetimeComponent { remainingTicks = 8 } )
            , ( "damage", DamageComponent { damageStrength = 80 } )
            ]
        )
        level


addBigExplosion : Position -> Level -> Level
addBigExplosion position level =
    List.foldr
        (\position level ->
            level |> addExplosion position.x position.y
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
