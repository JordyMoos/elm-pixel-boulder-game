## Ledger

```
[ ] = Empty
[#] = Wall
[.] = Dirt
[P] = Player
[X] = Dead Player
[O] = Rock
[*] = Diamond
```

## Behaviour
- Because the view is really small, approximately 12x12, we can not really show an animation of an actor moving from one tile to another. It is in tile A or B. It can not be in between. To visually simulate that there is a movement, we will let the pixel blink for a short time until the movement to tile B is complete. A blinking pixel will thus suggest an movement between tiles and the player should be able to recognize itself which direction something is going. Like the direction of Rocks are predictable. And the direction of the Player is triggered by the player itself.

The player can walk through and thereby consume Dirt.

```
[P][.] -> [ ][P]
```

- The player can walk through and thereby collect Diamond.

```
[P][*] -> [ ][P]
```

- Rocks will fall down if it is not on top of something

```
[O] -> [ ]
[ ]    [O]
```

- Rocks will stay on its place if it is on top of Dirt or Wall

```
[ ][O][ ] -> [ ][O][ ]
[ ][.][ ]    [ ][.][ ]
```

- Rock on a Rock will be unstable and will fall of to the side if there is space

```
[.][O][ ] -> [.][ ][O] -> [.][ ][ ]
[.][O][ ]    [.][O][ ]    [.][O][O]
```

- If a Rock falls on top of the Player then the Player will be dead.

```
[O] -> [ ] -> [ ]
[ ]    [O]    [ ]
[P]    [P]    [X]
```

- Diamonds behave the same as Rocks. Except that Diamonds to not damage the Player if the Diamond falls on the Player.

- If an animation has started because of a movement, like a Rock falling or a Player walking. Then that actor will go to the designated tile. Thus a Player can not cancel an movement. This will also mean that the Player will not accidently not move “far enough” to actually go to a tile.

- A level is won when all Diamonds are collected

- A level is failed when the Player died or gives-up/restarts (because it can no longer reach all Diamonds)
