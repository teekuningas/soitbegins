module Flags exposing (flagsDecoder, FlagsValue)

import Json.Decode


-- Some types and functions to handle flags

type alias FlagsValue = { serverUpdateInterval : Int
                        , modelEarth : String }


flagsDecoder : Json.Decode.Decoder FlagsValue
flagsDecoder =
  Json.Decode.map2 FlagsValue
    (Json.Decode.field "serverUpdateInterval" Json.Decode.int)
    (Json.Decode.field "modelEarth" Json.Decode.string)
    
