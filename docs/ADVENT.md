
HealthComponent width health property to keep track of the health
AttackComponent with attack power property to do damage


https://adventofcode.com/2018/day/15

How many steps it takes for x to attack an enemy.
We should scan for enemies this way.
- Scan for enemy
- Check how many steps it takes to reach (we know the minimum)
- if minimum then that is the enemy
- if more then minimum then scan next row as long as the row minimum is less then
an enemy shortest found path

```
|_|_|_|_|3|_|_|_|_|
|_|_|_|3|2|3|_|_|_|
|_|_|3|2|1|2|3|_|_|
|_|3|2|1|0|1|2|3|_|
|3|2|1|0|x|0|1|2|3|
|_|3|2|1|0|1|2|3|_|
|_|_|3|2|1|2|3|_|_|
|_|_|_|3|2|3|_|_|_|
|_|_|_|_|3|_|_|_|_|
```

```
x,y
x -> Left to right
y v Up to down

|_____|_____|_____|_____| 0,-4|_____|_____|_____|_____|
|_____|_____|_____|-1,-3| 0,-3| 1,-3|_____|_____|_____|
|_____|_____|-2,-2|-1,-2| 0,-2| 1,-2| 2,-2|_____|_____|
|_____|-3,-1|-2,-1|-1,-1| 0,-1| 1,-1| 2,-1| 3,-1|_____|
|-4, 0|-3, 0|-2, 0|-1, 0|  x  | 1, 0| 2, 0| 3, 0| 4, 0|
|_____|-3, 1|-2, 1|-1, 1| 0, 1| 1, 1| 2, 1| 3, 1|_____|
|_____|_____|-2, 2|-1, 2| 0, 2| 1, 2| 2, 2|_____|_____|
|_____|_____|_____|-1, 3| 0, 3| 1, 3|_____|_____|_____|
|_____|_____|_____|_____| 0, 4|_____|_____|_____|_____|
```

1
```
-1, 0
 0,-1
 1, 0
 0, 1
```

2
```
-2, 0
-1,-1
 0,-2
 1,-1
 2, 0
 1, 1
 0, 2
-1, 1
```

3
```
-3, 0
-2,-1
-1,-2
 0,-3
 1,-2
 2,-1
 3, 0
 2, 1
 1, 2
 0, 3
-1, 2
-2, 1
```



Sorted

1
```
-1, 0
 0,-1
 0, 1
 1, 0
```

2
```
-2, 0
-1,-1
-1, 1
 0,-2
 0, 2
 1,-1
 1, 1
 2, 0
```

3
```
-3, 0
-2,-1
-2, 1
-1,-2
-1, 2
 0,-3
 0, 3
 1,-2
 1, 2
 2,-1
 2, 1
 3, 0
```

```elm
toPositionOffsetList : Int -> Set (Int, Int)
toPositionOffsetList step =
    List.range (step * -1) step
        |> List.foldl
            (\x acc ->
                List.append
                    [ (x, step - (abs x))
                    , (x, (step - (abs x)) * -1)
                    ]
                    acc
            )
            []
        |> Set.fromList
```

