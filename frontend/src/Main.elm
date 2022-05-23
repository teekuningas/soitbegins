module Main exposing (main)

import Browser
import Communication.Types exposing (Connection, User)
import HUD.Types exposing (Canvas)
import Html exposing (Html)
import Json.Decode
import Platform.Cmd
import Platform.Sub
import States.GatherInfo as GatherInfo
import States.InGame as InGame
import States.InGameLoader as InGameLoader exposing (Preparing)
import States.Initialization as Initialization
import States.MainMenu as MainMenu
import States.Termination as Termination
import World.Types exposing (Data, World)


type Model
    = Initialization
    | GatherInfo Data User
    | MainMenu Data User
    | InGameLoader Data User Preparing
    | InGame Data User Connection Canvas World
    | Termination String


type Msg
    = InitializationMsg Initialization.Msg
    | GatherInfoMsg GatherInfo.Msg
    | MainMenuMsg MainMenu.Msg
    | InGameLoaderMsg InGameLoader.Msg
    | InGameMsg InGame.Msg



-- The model initialization


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flagsMsg =
    case Initialization.init of
        ( values, cmd ) ->
            ( Initialization
            , Platform.Cmd.map InitializationMsg cmd
            )


-- The view function


view : Model -> Html Msg
view model =
    case model of
        Termination message ->
            Termination.view { message = message }

        Initialization ->
            Html.map
                InitializationMsg
                Initialization.view

        MainMenu data user ->
            Html.map
                MainMenuMsg
                (MainMenu.view { user = user })

        GatherInfo data user ->
            Html.map
                GatherInfoMsg
                (GatherInfo.view { user = user })

        InGameLoader data user preparing ->
            Html.map
                InGameLoaderMsg
                InGameLoader.view

        InGame data user connection canvas world ->
            Html.map
                InGameMsg
                (InGame.view { data = data, user = user, canvas = canvas, world = world })



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialization ->
            Platform.Sub.map
                InitializationMsg
                Initialization.subscriptions

        MainMenu data user ->
            Platform.Sub.map
                MainMenuMsg
                MainMenu.subscriptions

        GatherInfo data user ->
            Platform.Sub.map
                GatherInfoMsg
                GatherInfo.subscriptions

        InGameLoader data user preparing ->
            Platform.Sub.map
                InGameLoaderMsg
                InGameLoader.subscriptions

        InGame data user connection canvas world ->
            Platform.Sub.map
                InGameMsg
                InGame.subscriptions

        _ ->
            Sub.none



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( InitializationMsg stateMsg, Initialization ) ->
            case stateMsg of
                Initialization.TransitionToGatherInfoMsg transitionData ->
                    case GatherInfo.init transitionData of
                        ( values, cmd ) ->
                            ( GatherInfo values.data values.user
                            , Platform.Cmd.map GatherInfoMsg cmd
                            )

                Initialization.TransitionToTerminationMsg message ->
                    case Termination.init message of
                        ( values, () ) ->
                            ( Termination values.message
                            , Cmd.none
                            )

                _ ->
                    case Initialization.update stateMsg of
                        ( _, cmd ) ->
                            ( Initialization
                            , Platform.Cmd.map InitializationMsg cmd
                            )

        ( GatherInfoMsg stateMsg, GatherInfo data user ) ->
            case stateMsg of
                GatherInfo.TransitionToMainMenuMsg ->
                    ( MainMenu data user
                    , Cmd.none
                    )

                _ ->
                    case GatherInfo.update stateMsg { user = user } of
                        ( values, cmd ) ->
                            ( GatherInfo data values.user
                            , Platform.Cmd.map GatherInfoMsg cmd
                            )

        ( MainMenuMsg stateMsg, MainMenu data user ) ->
            case stateMsg of
                MainMenu.TransitionToInGameLoaderMsg ->
                    case InGameLoader.init of
                        ( values, cmd ) ->
                            ( InGameLoader data user values.preparing
                            , Platform.Cmd.map InGameLoaderMsg cmd
                            )

                _ ->
                    case MainMenu.update stateMsg of
                        ( _, cmd ) ->
                            ( MainMenu data user
                            , Platform.Cmd.map MainMenuMsg cmd
                            )

        ( InGameLoaderMsg stateMsg, InGameLoader data user preparing ) ->
            case stateMsg of
                InGameLoader.TransitionToInGameMsg transitionData ->
                    case InGame.init transitionData of
                        ( values, cmd ) ->
                            ( InGame data user values.connection values.canvas values.world
                            , Platform.Cmd.map InGameMsg cmd
                            )

                _ ->
                    case InGameLoader.update stateMsg { preparing = preparing } of
                        ( values, cmd ) ->
                            ( InGameLoader data user values.preparing
                            , Platform.Cmd.map InGameLoaderMsg cmd
                            )

        ( InGameMsg stateMsg, InGame data user connection canvas world ) ->
            case stateMsg of
                InGame.TransitionToInGameLoaderMsg ->
                    case InGameLoader.init of
                        ( values, cmd ) ->
                            ( InGameLoader data user values.preparing
                            , Platform.Cmd.map InGameLoaderMsg cmd
                            )

                _ ->
                    case InGame.update stateMsg { connection = connection, canvas = canvas, world = world } of
                        ( values, cmd ) ->
                            ( InGame data user values.connection values.canvas values.world
                            , Platform.Cmd.map InGameMsg cmd
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
