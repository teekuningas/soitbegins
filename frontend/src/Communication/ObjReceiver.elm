port module Communication.ObjReceiver exposing
    ( objReceiver
    )

port objReceiver : (String -> msg) -> Sub msg


