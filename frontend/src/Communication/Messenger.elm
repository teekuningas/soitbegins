port module Communication.Messenger exposing
    ( RecvServerValue
    , decodeJson
    , messageReceiver
    )

import Json.Decode


type alias RecvServerValue =
    { earth : RecvServerValueEarth }


type alias RecvServerValueEarth =
    { rotationAroundSun : Float
    , rotationAroundAxis : Float
    }


port messageReceiver : (String -> msg) -> Sub msg


decodeJson : String -> Result Json.Decode.Error RecvServerValue
decodeJson value =
    Json.Decode.decodeString msgDecoder value


msgEarthDecoder : Json.Decode.Decoder RecvServerValueEarth
msgEarthDecoder =
    Json.Decode.map2 RecvServerValueEarth
        (Json.Decode.field "rotationAroundSun" Json.Decode.float)
        (Json.Decode.field "rotationAroundAxis" Json.Decode.float)


msgDecoder : Json.Decode.Decoder RecvServerValue
msgDecoder =
    Json.Decode.map RecvServerValue
        (Json.Decode.field "earth" msgEarthDecoder)
