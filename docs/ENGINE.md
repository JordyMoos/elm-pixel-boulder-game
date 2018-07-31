
Collect key state
When a tick happens depend action on key state
    - If a key is pressed and released then it must still count as a press in the game-update
    - game-update must invoke the real release after handing the update
    - Key status can thus be: IsPressed - WasPressed - NotPressed
        - And WasPressed must then be reset to NotPressed


Input needs to change
    If I hit top first and then left, all in game tick then i will walk left.
    While i should walk left because that was first.
    We need to store directions in een event list instead of this record.
    


Notes:

- Both has rigid -> collision
    - Push if you are strong enough

- Target has no rigid
    - Walk on top if strength is more then target strength