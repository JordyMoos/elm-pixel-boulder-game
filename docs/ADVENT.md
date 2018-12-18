
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

|_|_|_|_|3|_|_|_|_|
|_|_|_|3|2|3|_|_|_|
|_|_|3|2|1|2|3|_|_|
|_|3|2|1|0|1|2|3|_|
|3|2|1|0|x|0|1|2|3|
|_|3|2|1|0|1|2|3|_|
|_|_|3|2|1|2|3|_|_|
|_|_|_|3|2|3|_|_|_|
|_|_|_|_|3|_|_|_|_|
