
# Handle actors as one - start of actor in actor

Maybe this should only allow movement. Then we can skip

## Airplane should be delayed before movement
lifetime component ->
    define action:
        - removeActor
        - becomeActor entity
        
which will then become some sort of delay



## Move all actors

move area component
- width
- height
- move logic

It will only test itself and if he can move. Then he forces that same movement to all actors in his area (fixing the position offset)
That way it will be impossible for some actors to not move
