port module Receiver exposing (messageReceiver, decodeJson,
                               RecvValue, RecvValueEarth)


import Json.Decode


type alias RecvValue = { earth : RecvValueEarth }


type alias RecvValueEarth = { locationX : Float
                            , locationY : Float
                            , locationZ : Float
                            , rotationTheta : Float
                            , rotationAxisX : Float
                            , rotationAxisY : Float
                            , rotationAxisZ : Float } 


port messageReceiver : (String -> msg) -> Sub msg


decodeJson : String -> Result Json.Decode.Error RecvValue
decodeJson value = Json.Decode.decodeString msgDecoder value


msgEarthDecoder : Json.Decode.Decoder RecvValueEarth 
msgEarthDecoder =
  Json.Decode.map7 RecvValueEarth
    (Json.Decode.field "locationX" Json.Decode.float)
    (Json.Decode.field "locationY" Json.Decode.float)
    (Json.Decode.field "locationZ" Json.Decode.float)
    (Json.Decode.field "rotationTheta" Json.Decode.float)
    (Json.Decode.field "rotationAxisX" Json.Decode.float)
    (Json.Decode.field "rotationAxisY" Json.Decode.float)
    (Json.Decode.field "rotationAxisZ" Json.Decode.float)
    

msgDecoder : Json.Decode.Decoder RecvValue
msgDecoder = 
  Json.Decode.map RecvValue
    (Json.Decode.field "earth" msgEarthDecoder)


