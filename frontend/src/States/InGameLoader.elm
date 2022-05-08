module States.InGameLoader exposing (Msg, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Common exposing (DragState(..), GameLoaderData, Model(..))
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Page exposing (embedInCanvas)
import Platform.Sub
import Receiver
import Task
import Time


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Receiver.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix


subscriptions : GameLoaderData -> Sub Msg
subscriptions gameLoaderData =
    Platform.Sub.batch
        [ onAnimationFrame (\x -> TimeElapsed x)
        , onResize (\width height -> ResizeMsg)
        , Receiver.messageReceiver recvServerJson
        ]


view : GameLoaderData -> Html Msg
view gameLoaderData =
    embedInCanvas
        [ div
            [ class "game-loader-container" ]
            [ p [] [ text "Loading game.." ] ]
        ]
        []
        []


update : Msg -> GameLoaderData -> ( Model, Cmd Msg )
update msg gameLoaderData =
    case msg of
        RecvServerMsgError message ->
            let
                newGameLoaderData =
                    { gameLoaderData | connectionData = Nothing }
            in
            ( InGameLoader gameLoaderData, Cmd.none )

        RecvServerMsg message ->
            let
                msgEarth =
                    { locationX = message.earth.locationX
                    , locationY = message.earth.locationY
                    , locationZ = message.earth.locationZ
                    , rotationTheta = message.earth.rotationTheta
                    }
            in
            case gameLoaderData.connectionData of
                Just preparingConnectionData ->
                    let
                        newEarth =
                            { msgEarth = msgEarth
                            , previousMsgEarth =
                                preparingConnectionData.earth
                                    |> Maybe.map .msgEarth
                                    |> Maybe.withDefault msgEarth
                                    |> Just
                            }

                        newConnectionData =
                            { preparingConnectionData | earth = Just newEarth }

                        newGameLoaderData =
                            { gameLoaderData | connectionData = Just newConnectionData }
                    in
                    ( InGameLoader newGameLoaderData
                    , Task.perform UpdateTimeMsg Time.now
                    )

                Nothing ->
                    let
                        newConnectionData =
                            { earth =
                                Just
                                    { msgEarth = msgEarth
                                    , previousMsgEarth = Nothing
                                    }
                            , elapsed = Nothing
                            }

                        newGameLoaderData =
                            { gameLoaderData | connectionData = Just newConnectionData }
                    in
                    ( InGameLoader newGameLoaderData
                    , Task.perform UpdateTimeMsg Time.now
                    )

        UpdateTimeMsg dt ->
            case gameLoaderData.connectionData of
                Just preparingConnectionData ->
                    let
                        msgElapsed =
                            toFloat (Time.posixToMillis dt)

                        newElapsedData =
                            { msgElapsed = msgElapsed
                            , previousMsgElapsed =
                                preparingConnectionData.elapsed
                                    |> Maybe.map .msgElapsed
                                    |> Maybe.withDefault msgElapsed
                                    |> Just
                            }

                        newConnectionData =
                            { preparingConnectionData | elapsed = Just newElapsedData }

                        newGameLoaderData =
                            { gameLoaderData | connectionData = Just newConnectionData }
                    in
                    ( InGameLoader newGameLoaderData
                    , Cmd.none
                    )

                Nothing ->
                    ( InGameLoader gameLoaderData, Cmd.none )

        TimeElapsed dt ->
            let
                initGameData renderData msgElapsed previousMsgElapsed msgEarth previousMsgEarth =
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        newConnectionData =
                            { earth =
                                { msgEarth = msgEarth
                                , previousMsgEarth = previousMsgEarth
                                }
                            , elapsed =
                                { msgElapsed = msgElapsed
                                , previousMsgElapsed = previousMsgElapsed
                                }
                            }
                    in
                    { earth = msgEarth
                    , camera =
                        { azimoth = 0
                        , elevation = 0
                        }
                    , controller =
                        { dragState = NoDrag
                        , pointerOffset = { x = 0, y = 0 }
                        , previousOffset = { x = 0, y = 0 }
                        , upButtonDown = False
                        , downButtonDown = False
                        }
                    , hero =
                        { altitude = 11
                        , latitude = -0.3
                        , longitude = 0.0
                        , rotationTheta = 0
                        , power = 1
                        }
                    , renderData =
                        { elapsed = elapsed
                        , previousElapsed = renderData.elapsed
                        }
                    , canvasDimensions = gameLoaderData.canvasDimensions
                    , connectionData = newConnectionData
                    , earthMesh = gameLoaderData.earthMesh
                    , refreshed = False
                    }
            in
            case ( gameLoaderData.renderData, gameLoaderData.connectionData ) of
                ( Just renderData, Just connectionData ) ->
                    case ( connectionData.elapsed, connectionData.earth ) of
                        ( Just elapsedData, Just earthData ) ->
                            case ( elapsedData.previousMsgElapsed, earthData.previousMsgEarth ) of
                                ( Just previousMsgElapsed, Just previousMsgEarth ) ->
                                    let
                                        msgElapsed =
                                            elapsedData.msgElapsed

                                        msgEarth =
                                            earthData.msgEarth

                                        newGameData =
                                            initGameData
                                                renderData
                                                msgElapsed
                                                previousMsgElapsed
                                                msgEarth
                                                previousMsgEarth
                                    in
                                    ( InGame newGameData
                                    , Cmd.none
                                    )

                                _ ->
                                    let
                                        elapsed =
                                            toFloat (Time.posixToMillis dt)

                                        newRenderData =
                                            { elapsed = elapsed
                                            , previousElapsed = Just renderData.elapsed
                                            }

                                        newGameLoaderData =
                                            { gameLoaderData | renderData = Just newRenderData }
                                    in
                                    ( InGameLoader newGameLoaderData
                                    , Cmd.none
                                    )

                        _ ->
                            let
                                elapsed =
                                    toFloat (Time.posixToMillis dt)

                                newRenderData =
                                    { elapsed = elapsed
                                    , previousElapsed = Just renderData.elapsed
                                    }

                                newGameLoaderData =
                                    { gameLoaderData | renderData = Just newRenderData }
                            in
                            ( InGameLoader newGameLoaderData
                            , Cmd.none
                            )

                ( Just renderData, _ ) ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        newRenderData =
                            { elapsed = elapsed
                            , previousElapsed = Just renderData.elapsed
                            }

                        newGameLoaderData =
                            { gameLoaderData | renderData = Just newRenderData }
                    in
                    ( InGameLoader newGameLoaderData
                    , Cmd.none
                    )

                _ ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        newRenderData =
                            { elapsed = elapsed
                            , previousElapsed = Nothing
                            }

                        newGameLoaderData =
                            { gameLoaderData | renderData = Just newRenderData }
                    in
                    ( InGameLoader newGameLoaderData
                    , Cmd.none
                    )

        ResizeMsg ->
            ( InGameLoader gameLoaderData, Task.attempt ViewportMsg (getViewportOf "webgl-canvas") )

        -- This Msg is often induced by code also, as it refreshes the screen..
        ViewportMsg returnValue ->
            let
                newCanvasDimensions =
                    returnValue
                        |> Result.map .viewport
                        |> Result.map
                            (\v ->
                                { width = round v.width
                                , height = round v.height
                                }
                            )
                        |> Result.withDefault gameLoaderData.canvasDimensions

                newGameLoaderData =
                    { gameLoaderData | canvasDimensions = newCanvasDimensions }
            in
            ( InGameLoader newGameLoaderData
            , Cmd.none
            )


recvServerJson : String -> Msg
recvServerJson value =
    case Receiver.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"
