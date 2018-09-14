module Messages exposing (Msg(..))

import Time exposing (Posix)


type Msg
    = Tick Time.Posix
    | Run
    | Pause
    | Finish
