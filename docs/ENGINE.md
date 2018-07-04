
Collect key state
When a tick happens depend action on key state
    - If a key is pressed and released then it must still count as a press in the game-update
    - game-update must invoke the real release after handing the update
    - Key status can thus be: IsPressed - WasPressed - NotPressed
        - And WasPressed must then be reset to NotPressed
