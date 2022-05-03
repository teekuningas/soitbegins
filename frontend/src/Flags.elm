module Flags exposing (flagsDecoder, FlagsValue)

import Json.Decode


-- Some types and functions to handle flags

type alias FlagsValue = { modelEarth : String }


flagsDecoder : Json.Decode.Decoder FlagsValue
flagsDecoder =
  Json.Decode.map FlagsValue
    (Json.Decode.field "modelEarth" Json.Decode.string)
    
