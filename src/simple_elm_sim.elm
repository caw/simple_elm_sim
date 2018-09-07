module Main exposing (Model, Msg(..), Simulation, init, main, subscriptions, update, view)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type SimRunningState
    = NotStarted
    | Running
    | Paused
    | Finished


type Msg
    = Tick Time.Posix
    | Run
    | Pause
    | Finish


type alias Simulation =
    { runningState : SimRunningState
    , runningTime : Int
    }


initialSim : Simulation
initialSim =
    { runningState = NotStarted
    , runningTime = 0
    }


type alias Model =
    { sim : Simulation }


initialModel : Model
initialModel =
    { sim = initialSim
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                sim =
                    model.sim

                newSim =
                    { sim | runningTime = sim.runningTime + 1 }
            in
            ( { model | sim = newSim }
            , Cmd.none
            )

        Run ->
            let
                sim =
                    model.sim

                newSim =
                    { sim | runningState = Running }
            in
            ( { model | sim = newSim }, Cmd.none )

        Pause ->
            let
                sim =
                    model.sim

                newSim =
                    { sim | runningState = Paused }
            in
            ( { model | sim = newSim }, Cmd.none )

        Finish ->
            let
                sim =
                    model.sim

                newSim =
                    { sim | runningState = Finished }
            in
            ( { model | sim = newSim }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        rt =
            String.fromInt model.sim.runningTime

        sim =
            model.sim
    in
    div []
        [ h1 []
            [ text (rt ++ ":" ++ toString sim.runningState)
            , div []
                [ button [ onClick Run ] [ text "Run" ]
                , button [ onClick Pause ] [ text "Pause" ]
                , button [ onClick Finish ] [ text "Finish" ]
                ]
            ]
        ]
