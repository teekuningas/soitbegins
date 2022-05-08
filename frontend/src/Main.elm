module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewportOf)
import Common
    exposing
        ( Model(..)
        , viewportSize
        )
import Flags
import Html exposing (Html)
import Http
import Json.Decode
import Length exposing (Meters, meters)
import Obj.Decode exposing (expectObj)
import ObjLoader
import Platform.Cmd
import Platform.Sub
import States.InGame
import States.InGameLoader
import States.Initialization
import States.MainMenu
import States.Termination
import Task


type Msg
    = InitializationMsg States.Initialization.Msg
    | MainMenuMsg States.MainMenu.Msg
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
            let
                ( newModel, cmdMsg ) =
                    States.Initialization.init value
            in
            ( newModel
            , Platform.Cmd.map
                (\s -> InitializationMsg s)
                cmdMsg
            )



-- The view function


view : Model -> Html Msg
view model =
    case model of
        Termination message ->
            Html.map
                (\m -> TerminationMsg m)
                (States.Termination.view message)

        Initialization initData ->
            Html.map
                (\m -> InitializationMsg m)
                (States.Initialization.view initData)

        MainMenu menuData ->
            Html.map
                (\m -> MainMenuMsg m)
                (States.MainMenu.view menuData)

        InGameLoader gameLoaderData ->
            Html.map
                (\m -> InGameLoaderMsg m)
                (States.InGameLoader.view gameLoaderData)

        InGame gameData ->
            Html.map
                (\m -> InGameMsg m)
                (States.InGame.view gameData)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialization initData ->
            Platform.Sub.map
                (\s -> InitializationMsg s)
                (States.Initialization.subscriptions initData)

        MainMenu menuData ->
            Platform.Sub.map
                (\s -> MainMenuMsg s)
                (States.MainMenu.subscriptions menuData)

        InGameLoader gameLoaderData ->
            Platform.Sub.map
                (\s -> InGameLoaderMsg s)
                (States.InGameLoader.subscriptions gameLoaderData)

        InGame gameData ->
            Platform.Sub.map
                (\s -> InGameMsg s)
                (States.InGame.subscriptions gameData)

        Termination message ->
            Platform.Sub.map
                (\s -> TerminationMsg s)
                (States.Termination.subscriptions message)



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Initialization initData ->
            case msg of
                InitializationMsg convMsg ->
                    let
                        ( newModel, cmdMsg ) =
                            States.Initialization.update convMsg initData
                    in
                    ( newModel
                    , Platform.Cmd.map
                        (\s -> InitializationMsg s)
                        cmdMsg
                    )

                _ ->
                    ( model, Cmd.none )

        MainMenu menuData ->
            case msg of
                MainMenuMsg convMsg ->
                    let
                        ( newModel, cmdMsg ) =
                            States.MainMenu.update convMsg menuData
                    in
                    ( newModel
                    , Platform.Cmd.map
                        (\s -> MainMenuMsg s)
                        cmdMsg
                    )

                _ ->
                    ( model, Cmd.none )

        InGameLoader gameLoaderData ->
            case msg of
                InGameLoaderMsg convMsg ->
                    let
                        ( newModel, cmdMsg ) =
                            States.InGameLoader.update convMsg gameLoaderData
                    in
                    ( newModel
                    , Platform.Cmd.map
                        (\s -> InGameLoaderMsg s)
                        cmdMsg
                    )

                _ ->
                    ( model, Cmd.none )

        InGame gameData ->
            case msg of
                InGameMsg convMsg ->
                    let
                        ( newModel, cmdMsg ) =
                            States.InGame.update convMsg gameData
                    in
                    ( newModel
                    , Platform.Cmd.map
                        (\s -> InGameMsg s)
                        cmdMsg
                    )

                _ ->
                    ( model, Cmd.none )

        Termination message ->
            case msg of
                TerminationMsg convMsg ->
                    let
                        ( newModel, cmdMsg ) =
                            States.Termination.update convMsg message
                    in
                    ( newModel
                    , Platform.Cmd.map
                        (\s -> TerminationMsg s)
                        cmdMsg
                    )

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
