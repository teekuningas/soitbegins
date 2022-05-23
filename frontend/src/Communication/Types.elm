module Communication.Types exposing (Connection, User)

import World.Types exposing (Earth)


type alias Connection =
    { earth :
        { msgEarth : Earth
        , previousMsgEarth : Earth
        }
    , elapsed :
        { msgElapsed : Float
        , previousMsgElapsed : Float
        }
    }


type alias User =
    { name : String
    }
