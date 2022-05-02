port module Receiver exposing (messageReceiver, decodeJson,
                               RecvServerValue)


import Json.Decode


-- Some types and functions for handling
-- port communication

type alias RecvServerValue = { earth : RecvServerValueEarth }

type alias RecvServerValueEarth = { locationX : Float
                                  , locationY : Float
                                  , locationZ : Float
                                  , rotationTheta : Float }


port messageReceiver : (String -> msg) -> Sub msg


decodeJson : String -> Result Json.Decode.Error RecvServerValue
decodeJson value = Json.Decode.decodeString msgDecoder value


msgEarthDecoder : Json.Decode.Decoder RecvServerValueEarth 
msgEarthDecoder =
  Json.Decode.map4 RecvServerValueEarth
    (Json.Decode.field "locationX" Json.Decode.float)
    (Json.Decode.field "locationY" Json.Decode.float)
    (Json.Decode.field "locationZ" Json.Decode.float)
    (Json.Decode.field "rotationTheta" Json.Decode.float)
    

msgDecoder : Json.Decode.Decoder RecvServerValue
msgDecoder = 
  Json.Decode.map RecvServerValue
    (Json.Decode.field "earth" msgEarthDecoder)

