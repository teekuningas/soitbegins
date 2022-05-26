port module Communication.ObjReceiver exposing (objReceiver)

import Json.Decode


port objReceiver : (Json.Decode.Value -> msg) -> Sub msg
