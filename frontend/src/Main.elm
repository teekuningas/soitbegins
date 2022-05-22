module Main exposing (main)

import Browser
import Communication.Flags as Flags
import Html exposing (Html)
import Json.Decode
import Platform.Cmd
import Platform.Sub
import States.GatherInfo as GatherInfo
import States.InGame as InGame
import States.InGameLoader as InGameLoader
import States.Initialization as Initialization
import States.MainMenu as MainMenu
import States.Termination as Termination
import States.InitializationTypes exposing (InitData)
import States.GatherInfoTypes exposing (GatherInfoData)
import States.MainMenuTypes exposing (MenuData)
import States.InGameLoaderTypes exposing (GameLoaderData)
import States.InGameTypes exposing (GameData)



type Model
    = Initialization InitData
    | GatherInfo GatherInfoData
    | MainMenu MenuData
    | InGameLoader GameLoaderData
    | InGame GameData
    | Termination String


type Msg
    = InitializationMsg Initialization.Msg
    | MainMenuMsg MainMenu.Msg
    | GatherInfoMsg GatherInfo.Msg
    | InGameLoaderMsg InGameLoader.Msg
    | InGameMsg InGame.Msg
    | TerminationMsg Termination.Msg



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
            Initialization.init value
                |> Tuple.mapBoth Initialization (Platform.Cmd.map InitializationMsg)



-- The view function


view : Model -> Html Msg
view model =
    case model of
        Termination message ->
            Html.map
                TerminationMsg
                (Termination.view message)

        Initialization initData ->
            Html.map
                InitializationMsg
                (Initialization.view initData)

        MainMenu menuData ->
            Html.map
                MainMenuMsg
                (MainMenu.view menuData)

        GatherInfo gatherInfoData ->
            Html.map
                GatherInfoMsg
                (GatherInfo.view gatherInfoData)

        InGameLoader gameLoaderData ->
            Html.map
                InGameLoaderMsg
                (InGameLoader.view gameLoaderData)

        InGame gameData ->
            Html.map
                InGameMsg
                (InGame.view gameData)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialization initData ->
            Platform.Sub.map
                InitializationMsg
                (Initialization.subscriptions initData)

        MainMenu menuData ->
            Platform.Sub.map
                MainMenuMsg
                (MainMenu.subscriptions menuData)

        GatherInfo gatherInfoData ->
            Platform.Sub.map
                GatherInfoMsg
                (GatherInfo.subscriptions gatherInfoData)

        InGameLoader gameLoaderData ->
            Platform.Sub.map
                InGameLoaderMsg
                (InGameLoader.subscriptions gameLoaderData)

        InGame gameData ->
            Platform.Sub.map
                InGameMsg
                (InGame.subscriptions gameData)

        Termination message ->
            Platform.Sub.map
                TerminationMsg
                (Termination.subscriptions message)



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InitializationMsg stateMsg, Initialization initData ) ->
            case stateMsg of 

                Initialization.TransitionToGatherInfoMsg data ->
                    GatherInfo.init data
                        |> Tuple.mapBoth GatherInfo (Platform.Cmd.map GatherInfoMsg) 

                Initialization.TransitionToTerminationMsg data ->
                    Termination.init data
                        |> Tuple.mapBoth Termination (Platform.Cmd.map TerminationMsg) 

                _ ->
                    Initialization.update stateMsg initData
                        |> Tuple.mapBoth Initialization (Platform.Cmd.map InitializationMsg)

        ( GatherInfoMsg stateMsg, GatherInfo gatherInfoData ) ->
            case stateMsg of 
                GatherInfo.TransitionToMainMenuMsg data ->
                    MainMenu.init data
                        |> Tuple.mapBoth MainMenu (Platform.Cmd.map MainMenuMsg) 

                _ ->
                    GatherInfo.update stateMsg gatherInfoData
                        |> Tuple.mapBoth GatherInfo (Platform.Cmd.map GatherInfoMsg)

        ( MainMenuMsg stateMsg, MainMenu menuData ) ->

            case stateMsg of 
                MainMenu.TransitionToInGameLoaderMsg data ->
                    InGameLoader.init data
                        |> Tuple.mapBoth InGameLoader (Platform.Cmd.map InGameLoaderMsg) 

                _ ->
                    MainMenu.update stateMsg menuData
                        |> Tuple.mapBoth MainMenu (Platform.Cmd.map MainMenuMsg)

        ( InGameLoaderMsg stateMsg, InGameLoader gameLoaderData ) ->

            case stateMsg of 
                InGameLoader.TransitionToInGameMsg data ->
                    InGame.init data
                        |> Tuple.mapBoth InGame (Platform.Cmd.map InGameMsg) 

                _ ->
                    InGameLoader.update stateMsg gameLoaderData
                        |> Tuple.mapBoth InGameLoader (Platform.Cmd.map InGameLoaderMsg)


        ( InGameMsg stateMsg, InGame gameData ) ->

            case stateMsg of 

                InGame.TransitionToInGameLoaderMsg data ->
                    InGameLoader.init data
                        |> Tuple.mapBoth InGameLoader (Platform.Cmd.map InGameLoaderMsg) 

                _ ->
                    InGame.update stateMsg gameData
                        |> Tuple.mapBoth InGame (Platform.Cmd.map InGameMsg)



        ( TerminationMsg stateMsg, Termination message ) ->

            Termination.update stateMsg message
                |> Tuple.mapBoth Termination (Platform.Cmd.map TerminationMsg)

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
