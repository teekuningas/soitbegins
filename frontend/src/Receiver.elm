port module Receiver exposing (messageReceiver, decodeJson,
                               RecvValue, RecvValueEarth)


import Json.Decode


-- Some types and functions for handling
-- port communication

type alias RecvValue = { earth : RecvValueEarth }

type alias RecvValueEarth = { locationX : Float
                            , locationY : Float
                            , locationZ : Float
                            , rotationTheta : Float }


port messageReceiver : (String -> msg) -> Sub msg


decodeJson : String -> Result Json.Decode.Error RecvValue
decodeJson value = Json.Decode.decodeString msgDecoder value


msgEarthDecoder : Json.Decode.Decoder RecvValueEarth 
msgEarthDecoder =
  Json.Decode.map4 RecvValueEarth
    (Json.Decode.field "locationX" Json.Decode.float)
    (Json.Decode.field "locationY" Json.Decode.float)
    (Json.Decode.field "locationZ" Json.Decode.float)
    (Json.Decode.field "rotationTheta" Json.Decode.float)
    

msgDecoder : Json.Decode.Decoder RecvValue
msgDecoder = 
  Json.Decode.map RecvValue
    (Json.Decode.field "earth" msgEarthDecoder)

