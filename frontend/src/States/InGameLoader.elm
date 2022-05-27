module States.InGameLoader exposing (Msg(..), Preparing, init, subscriptions, update, view)

import Browser.Dom exposing (getViewportOf)
import Browser.Events exposing (onAnimationFrame, onResize)
import Communication.Messenger as Messenger
import Communication.Types exposing (Connection)
import HUD.Page exposing (embedInCanvas)
import HUD.Types exposing (CanvasDimensions, RenderData)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Platform.Cmd
import Platform.Sub
import Random
import Task
import Time
import World.Types exposing (Earth, Hero)


type alias Preparing =
    { canvasDim : Maybe CanvasDimensions
    , renderData : Maybe PreparingRenderData
    , connection : Maybe PreparingConnection
    , hero : Maybe Hero
    }


type alias PreparingRenderData =
    { elapsed : Float
    , previousElapsed : Maybe Float
    }


type alias PreparingConnection =
    { earth :
        Maybe
            { msgEarth : Earth
            , previousMsgEarth : Maybe Earth
            }
    , elapsed :
        Maybe
            { msgElapsed : Float
            , previousMsgElapsed : Maybe Float
            }
    }


type Msg
    = TimeElapsed Time.Posix
    | ResizeMsg
    | ViewportMsg (Result Browser.Dom.Error Browser.Dom.Viewport)
    | RecvServerMsg Messenger.RecvServerValue
    | RecvServerMsgError String
    | UpdateTimeMsg Time.Posix
    | RandomValueMsg RandomValues
    | TransitionToInGameMsg
        { renderData : RenderData
        , connection : Connection
        , canvasDim : CanvasDimensions
        , hero : Hero
        }


type alias InitData =
    { preparing : Preparing
    }


init : ( InitData, Cmd Msg )
init =
    let
        preparing =
            { renderData = Nothing
            , connection = Nothing
            , canvasDim = Nothing
            , hero = Nothing
            }
    in
    ( { preparing = preparing }
    , Platform.Cmd.batch
        [ Random.generate RandomValueMsg randomValues
        , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
        ]
    )


subscriptions : Sub Msg
subscriptions =
    Platform.Sub.batch
        [ onAnimationFrame (\x -> TimeElapsed x)
        , onResize (\width height -> ResizeMsg)
        , Messenger.messageReceiver recvServerJson
        ]


view : Html msg
view =
    embedInCanvas
        []
        [ div
            [ class "game-loader-container" ]
            [ p [] [ text "Loading game.." ] ]
        ]
        []
        []


type alias UpdateData =
    { preparing : Preparing
    }


update : Msg -> UpdateData -> ( UpdateData, Cmd Msg )
update msg values =
    case msg of
        RecvServerMsgError message ->
            let
                preparing =
                    values.preparing

                newPreparing =
                    { preparing | connection = Nothing }
            in
            ( { values | preparing = newPreparing }
            , Cmd.none
            )

        RecvServerMsg message ->
            let
                preparing =
                    values.preparing

                msgEarth =
                    { rotationAroundSun = message.earth.rotationAroundSun
                    , rotationAroundAxis = message.earth.rotationAroundAxis
                    }
            in
            case preparing.connection of
                Just preparingConnection ->
                    let
                        newEarth =
                            { msgEarth = msgEarth
                            , previousMsgEarth =
                                preparingConnection.earth
                                    |> Maybe.map .msgEarth
                                    |> Maybe.withDefault msgEarth
                                    |> Just
                            }

                        newConnection =
                            { preparingConnection | earth = Just newEarth }

                        newPreparing =
                            { preparing | connection = Just newConnection }
                    in
                    ( { values | preparing = newPreparing }
                    , Task.perform UpdateTimeMsg Time.now
                    )

                Nothing ->
                    let
                        newConnection =
                            { earth =
                                Just
                                    { msgEarth = msgEarth
                                    , previousMsgEarth = Nothing
                                    }
                            , elapsed = Nothing
                            }

                        newPreparing =
                            { preparing | connection = Just newConnection }
                    in
                    ( { values | preparing = newPreparing }
                    , Task.perform UpdateTimeMsg Time.now
                    )

        UpdateTimeMsg dt ->
            let
                preparing =
                    values.preparing
            in
            case preparing.connection of
                Just preparingConnection ->
                    let
                        msgElapsed =
                            toFloat (Time.posixToMillis dt)

                        newElapsedData =
                            { msgElapsed = msgElapsed
                            , previousMsgElapsed =
                                preparingConnection.elapsed
                                    |> Maybe.map .msgElapsed
                                    |> Maybe.withDefault msgElapsed
                                    |> Just
                            }

                        newConnection =
                            { preparingConnection | elapsed = Just newElapsedData }

                        newPreparing =
                            { preparing | connection = Just newConnection }
                    in
                    ( { values | preparing = newPreparing }
                    , Cmd.none
                    )

                Nothing ->
                    ( values, Cmd.none )

        TimeElapsed dt ->
            let
                preparing =
                    values.preparing

                mPreviousMsgElapsed =
                    preparing.connection
                        |> Maybe.map .elapsed
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .previousMsgElapsed
                        |> Maybe.withDefault Nothing

                mMsgElapsed =
                    preparing.connection
                        |> Maybe.map .elapsed
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .msgElapsed

                mPreviousMsgEarth =
                    preparing.connection
                        |> Maybe.map .earth
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .previousMsgEarth
                        |> Maybe.withDefault Nothing

                mMsgEarth =
                    preparing.connection
                        |> Maybe.map .earth
                        |> Maybe.withDefault Nothing
                        |> Maybe.map .msgEarth

                mRenderData =
                    preparing.renderData

                mHero =
                    preparing.hero

                mCanvasDim =
                    preparing.canvasDim
            in
            case ( mPreviousMsgElapsed, ( mMsgElapsed, ( mPreviousMsgEarth, ( mMsgEarth, ( mRenderData, ( mHero, mCanvasDim ) ) ) ) ) ) of
                ( Just previousMsgElapsed, ( Just msgElapsed, ( Just previousMsgEarth, ( Just msgEarth, ( Just renderData, ( Just hero, Just canvasDim ) ) ) ) ) ) ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        newConnection =
                            { earth =
                                { msgEarth = msgEarth
                                , previousMsgEarth = previousMsgEarth
                                , previousEarthAtMsg = msgEarth
                                }
                            , elapsed =
                                { msgElapsed = msgElapsed
                                , previousMsgElapsed = previousMsgElapsed
                                }
                            }

                        transitionData =
                            { hero = hero
                            , renderData =
                                { elapsed = elapsed
                                , previousElapsed = renderData.elapsed
                                }
                            , canvasDim = canvasDim
                            , connection = newConnection
                            }
                    in
                    ( values
                    , Task.perform (always (TransitionToInGameMsg transitionData)) (Task.succeed ())
                    )

                _ ->
                    let
                        elapsed =
                            toFloat (Time.posixToMillis dt)

                        previousElapsed =
                            preparing.renderData
                                |> Maybe.map .elapsed

                        newRenderData =
                            { elapsed = elapsed
                            , previousElapsed = previousElapsed
                            }

                        newPreparing =
                            { preparing | renderData = Just newRenderData }
                    in
                    ( { values | preparing = newPreparing }
                    , Cmd.none
                    )

        -- Refresh canvas
        ResizeMsg ->
            ( values
            , Task.attempt ViewportMsg (getViewportOf "webgl-canvas")
            )

        ViewportMsg returnValue ->
            let
                preparing =
                    values.preparing

                newCanvasDim =
                    returnValue
                        |> Result.map .viewport
                        |> Result.map
                            (\v ->
                                { width = round v.width
                                , height = round v.height
                                }
                            )
                        |> Result.toMaybe
                        |> Maybe.map Just
                        |> Maybe.withDefault preparing.canvasDim

                newPreparing =
                    { preparing | canvasDim = newCanvasDim }
            in
            ( { values | preparing = newPreparing }
            , Cmd.none
            )

        RandomValueMsg results ->
            let
                preparing =
                    values.preparing

                envColor =
                    vec3 results.envelope.envelopeR results.envelope.envelopeG results.envelope.envelopeB

                hero =
                    { altitude = 110
                    , latitude = results.location.latitude
                    , longitude = results.location.longitude
                    , latSpeed = results.location.latSpeed / 10000
                    , lonSpeed = results.location.lonSpeed / 10000
                    , rotationTheta = 0
                    , power = 1
                    , envColor = envColor
                    }

                newPreparing =
                    { preparing | hero = Just hero }
            in
            ( { values | preparing = newPreparing }
            , Cmd.none
            )

        _ ->
            ( values
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



-- Others


recvServerJson : String -> Msg
recvServerJson value =
    case Messenger.decodeJson value of
        Ok result ->
            RecvServerMsg result

        Err errorMessage ->
            RecvServerMsgError "Error while communicating with the server"
