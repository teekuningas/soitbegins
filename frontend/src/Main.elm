module Main exposing (main)

import Browser
import Communication.Flags as Flags
import Html exposing (Html)
import Json.Decode
import Model.Model
    exposing
        ( Model(..)
        )
import Platform.Cmd
import Platform.Sub
import States.GatherInfo
import States.InGame
import States.InGameLoader
import States.Initialization
import States.MainMenu
import States.Termination


type Msg
    = InitializationMsg States.Initialization.Msg
    | MainMenuMsg States.MainMenu.Msg
    | GatherInfoMsg States.GatherInfo.Msg
    | InGameLoaderMsg States.InGameLoader.Msg
    | InGameMsg States.InGame.Msg
    | TerminationMsg States.Termination.Msg



-- The model initialization


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagsMsg =
    let
        flags =
            Json.Decode.decodeValue Flags.flagsDecoder flagsMsg
    in
    case flags of
        Err _ ->
            ( Termination "Could not read environment variables"
            , Cmd.none
            )

        Ok value ->
            States.Initialization.init value
                |> Tuple.mapBoth identity (Platform.Cmd.map InitializationMsg)



-- The view function


view : Model -> Html Msg
view model =
    case model of
        Termination message ->
            Html.map
                TerminationMsg
                (States.Termination.view message)

        Initialization initData ->
            Html.map
                InitializationMsg
                (States.Initialization.view initData)

        MainMenu menuData ->
            Html.map
                MainMenuMsg
                (States.MainMenu.view menuData)

        GatherInfo gatherInfoData ->
            Html.map
                GatherInfoMsg
                (States.GatherInfo.view gatherInfoData)

        InGameLoader gameLoaderData ->
            Html.map
                InGameLoaderMsg
                (States.InGameLoader.view gameLoaderData)

        InGame gameData ->
            Html.map
                InGameMsg
                (States.InGame.view gameData)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialization initData ->
            Platform.Sub.map
                InitializationMsg
                (States.Initialization.subscriptions initData)

        MainMenu menuData ->
            Platform.Sub.map
                MainMenuMsg
                (States.MainMenu.subscriptions menuData)

        GatherInfo gatherInfoData ->
            Platform.Sub.map
                GatherInfoMsg
                (States.GatherInfo.subscriptions gatherInfoData)

        InGameLoader gameLoaderData ->
            Platform.Sub.map
                InGameLoaderMsg
                (States.InGameLoader.subscriptions gameLoaderData)

        InGame gameData ->
            Platform.Sub.map
                InGameMsg
                (States.InGame.subscriptions gameData)

        Termination message ->
            Platform.Sub.map
                TerminationMsg
                (States.Termination.subscriptions message)



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InitializationMsg stateMsg, Initialization initData ) ->
            States.Initialization.update stateMsg initData
                |> Tuple.mapSecond (Platform.Cmd.map InitializationMsg)

        ( MainMenuMsg stateMsg, MainMenu menuData ) ->
            States.MainMenu.update stateMsg menuData
                |> Tuple.mapSecond (Platform.Cmd.map MainMenuMsg)

        ( GatherInfoMsg stateMsg, GatherInfo gatherInfoData ) ->
            States.GatherInfo.update stateMsg gatherInfoData
                |> Tuple.mapSecond (Platform.Cmd.map GatherInfoMsg)

        ( InGameLoaderMsg stateMsg, InGameLoader gameLoaderData ) ->
            States.InGameLoader.update stateMsg gameLoaderData
                |> Tuple.mapSecond (Platform.Cmd.map InGameLoaderMsg)

        ( InGameMsg stateMsg, InGame gameData ) ->
            States.InGame.update stateMsg gameData
                |> Tuple.mapSecond (Platform.Cmd.map InGameMsg)

        ( TerminationMsg stateMsg, Termination message ) ->
            States.Termination.update stateMsg message
                |> Tuple.mapSecond (Platform.Cmd.map TerminationMsg)

        _ ->
            ( model, Cmd.none )



-- Here it begins.


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
