module Communication.Flags exposing (FlagsValue, flagsDecoder)

import Json.Decode


type alias FlagsValue =
    { serverUpdateInterval : Int }


flagsDecoder : Json.Decode.Decoder FlagsValue
flagsDecoder =
    Json.Decode.map FlagsValue
        (Json.Decode.field "serverUpdateInterval" Json.Decode.int)
