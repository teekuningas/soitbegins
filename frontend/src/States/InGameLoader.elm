module States.InGameLoader exposing (Msg(..), subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Communication.Receiver as Receiver
import HUD.Page exposing (embedInCanvas)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Model.Model
    exposing
        ( DragState(..)
        , GameLoaderData
        , Model(..)
        )
import Platform.Sub
import Random
import Task
import Time


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Receiver.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix
    | RandomValueMsg RandomValues
    | InitMsg


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
        []
        [ div
            [ class "game-loader-container" ]
            [ p [] [ text "Loading game.." ] ]
        ]
        []
        []


update : Msg -> GameLoaderData -> ( Model, Cmd Msg )
update msg gameLoaderData =
    case msg of
        InitMsg ->
            ( InGameLoader gameLoaderData
            , Random.generate RandomValueMsg randomValues
            )

        RecvServerMsgError message ->
            let
                newGameLoaderData =
                    { gameLoaderData | connectionData = Nothing }
            in
            ( InGameLoader gameLoaderData, Cmd.none )

        RecvServerMsg message ->
            let
                msgEarth =
                    { rotationAroundSun = message.earth.rotationAroundSun
                    , rotationAroundAxis = message.earth.rotationAroundAxis
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
                mPreviousMsgElapsed =
                    gameLoaderData.connectionData
                        |> Maybe.map .elapsed
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .previousMsgElapsed
                        |> Maybe.withDefault Nothing

                mMsgElapsed =
                    gameLoaderData.connectionData
                        |> Maybe.map .elapsed
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .msgElapsed

                mPreviousMsgEarth =
                    gameLoaderData.connectionData
                        |> Maybe.map .earth
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .previousMsgEarth
                        |> Maybe.withDefault Nothing

                mMsgEarth =
                    gameLoaderData.connectionData
                        |> Maybe.map .earth
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .msgEarth

                mRenderData =
                    gameLoaderData.renderData

                mHero =
                    gameLoaderData.hero
            in
            case ( mPreviousMsgElapsed, ( mMsgElapsed, ( mPreviousMsgEarth, ( mMsgEarth, ( mRenderData, mHero ) ) ) ) ) of
                ( Just previousMsgElapsed, ( Just msgElapsed, ( Just previousMsgEarth, ( Just msgEarth, ( Just renderData, Just hero ) ) ) ) ) ->
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

                        newGameData =
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
                            , hero = hero
                            , renderData =
                                { elapsed = elapsed
                                , previousElapsed = renderData.elapsed
                                }
                            , canvasDimensions = gameLoaderData.canvasDimensions
                            , connectionData = newConnectionData
                            , earthMesh = gameLoaderData.earthMesh
                            , refreshed = False
                            , overviewToggle = False
                            , user = gameLoaderData.user
                            }
                    in
                    ( InGame newGameData
                    , Cmd.none
                    )

                _ ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        previousElapsed =
                            gameLoaderData.renderData
                                |> Maybe.map .elapsed

                        newRenderData =
                            { elapsed = elapsed
                            , previousElapsed = previousElapsed
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

        RandomValueMsg values ->
            let
                envColor =
                    vec3 values.envelope.envelopeR values.envelope.envelopeG values.envelope.envelopeB

                hero =
                    { altitude = 110
                    , latitude = values.location.latitude
                    , longitude = values.location.longitude
                    , latSpeed = values.location.latSpeed / 10000
                    , lonSpeed = values.location.lonSpeed / 10000
                    , rotationTheta = 0
                    , power = 1
                    , envColor = envColor
                    }

                newGameLoaderData =
                    { gameLoaderData | hero = Just hero }
            in
            ( InGameLoader newGameLoaderData
            , Cmd.none
            )



-- Random


type alias RandomLocation =
    { longitude : Float
    , latitude : Float
    , lonSpeed : Float
    , latSpeed : Float
    }


type alias RandomEnvelope =
    { envelopeR : Float
    , envelopeG : Float
    , envelopeB : Float
    }


type alias RandomValues =
    { location : RandomLocation
    , envelope : RandomEnvelope
    }


randomEnvelope : Random.Generator RandomEnvelope
randomEnvelope =
    Random.map3
        RandomEnvelope
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


randomLocation : Random.Generator RandomLocation
randomLocation =
    Random.map4
        RandomLocation
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)
        (Random.float 0 1)


randomValues : Random.Generator RandomValues
randomValues =
    Random.map2
        RandomValues
        randomLocation
        randomEnvelope


recvServerJson : String -> Msg
recvServerJson value =
    case Receiver.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"
