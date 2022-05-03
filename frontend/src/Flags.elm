module Flags exposing (FlagsValue, flagsDecoder)

import Json.Decode


type alias FlagsValue =
    { modelEarth : String }


flagsDecoder : Json.Decode.Decoder FlagsValue
flagsDecoder =
    Json.Decode.map FlagsValue
        (Json.Decode.field "modelEarth" Json.Decode.string)
