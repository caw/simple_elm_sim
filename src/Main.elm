port module Main exposing (Model, Simulation, init, main, subscriptions, update, view)

import Browser
import Circ exposing (Circulation, circ, circView, updateCirculation)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onInput)
import Json.Encode as E
import Messages exposing (Msg(..))
import Round exposing (..)
import Task
import Time
import Utilities exposing (ntimes)



-- MAIN


port play : E.Value -> Cmd msg


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


type UIButton
    = RunButton
    | PauseButton
    | FinishButton


type alias Simulation =
    { runningState : SimRunningState
    , speedUp : Float
    , runningTime : Int
    , circ : Circulation
    }


initialSim : Simulation
initialSim =
    { runningState = NotStarted
    , speedUp = 1.0
    , runningTime = 0
    , circ = circ
    }


type alias Model =
    { sim : Simulation
    , rangeDemo : Int
    }


initialModel : Model
initialModel =
    { sim = initialSim
    , rangeDemo = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


btnState : UIButton -> Model -> Bool
btnState btn model =
    let
        state =
            model.sim.runningState
    in
    case btn of
        RunButton ->
            state == Running || state == Finished

        PauseButton ->
            state == NotStarted || state == Finished || state == Paused

        FinishButton ->
            state == NotStarted || state == Finished



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.sim.runningState == Running then
                let
                    sim =
                        model.sim

                    circ_ =
                        ntimes 1000 updateCirculation sim.circ

                    newSim =
                        { sim
                            | runningTime = sim.runningTime + 1
                            , circ = circ_
                        }
                in
                ( { model | sim = newSim }
                , play (E.bool True)
                )

            else
                ( model, Cmd.none )

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

        SpeedUpSim ->
            let
                sim =
                    model.sim

                sim_ =
                    { sim | speedUp = Basics.min (sim.speedUp + 0.1) 10 }
            in
            ( { model | sim = sim_ }, Cmd.none )

        SlowDownSim ->
            let
                sim =
                    model.sim

                sim_ =
                    { sim | speedUp = Basics.max (sim.speedUp - 0.1) 0.1 }
            in
            ( { model | sim = sim_ }, Cmd.none )

        HSUpdate str ->
            case String.toFloat str of
                Just f ->
                    let
                        sim =
                            model.sim

                        circ =
                            sim.circ

                        hs_ =
                            f / 10

                        circ_ =
                            { circ | hs = hs_ }

                        sim_ =
                            { sim | circ = circ_ }
                    in
                    ( { model | sim = sim_ }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1000 / model.sim.speedUp) Tick



-- VIEW


toMinSec : Int -> String
toMinSec seconds =
    let
        m =
            toString (seconds // 60)

        s =
            remainderBy 60 seconds

        s_pad =
            if s < 10 then
                "0" ++ toString s

            else
                toString s
    in
    m ++ ":" ++ s_pad


view : Model -> Html Msg
view model =
    let
        rt =
            model.sim.runningTime

        sim =
            model.sim
    in
    div []
        [ div [ id "header" ]
            [ img [ src "images/MULogo.jpg", width 100, height 100 ] []
            , div [ id "MuSIM" ] [ text "MuSIM" ]
            , div [ id "sim_ui_mode" ]
                [ div [ class "ui_mode" ] [ text "End User" ]
                , div [ class "ui_mode" ] [ text "Design" ]
                , div [ class "active_ui_mode" ] [ text "Low Level" ]
                ]
            ]
        , hr [] []
        , div [ class "sim_control_container" ]
            [ div [ id "running_time" ]
                [ div [] [ text (toMinSec rt) ]
                , div [ id "time_dilation" ]
                    [ button [ onClick SlowDownSim ] [ text "Slower" ]
                    , div [ id "speedup" ] [ text (Round.round 1 model.sim.speedUp) ]
                    , button [ onClick SpeedUpSim ] [ text "Faster" ]
                    ]
                , div [ id "sim_run_state" ]
                    [ button [ id "run", onClick Run, disabled (btnState RunButton model) ] [ i [ class "fa fa-play" ] [] ]
                    , button [ id "pause", onClick Pause, disabled (btnState PauseButton model) ] [ i [ class "fa fa-pause" ] [] ]
                    , button [ id "stop", onClick Finish, disabled (btnState FinishButton model) ] [ i [ class "fa fa-stop" ] [] ]
                    ]
                ]
            ]
        , hr [] []
        , div [ class "circulation_container" ] (circView model.sim.circ)
        , hr [] []
        , div [ id "sliders_and_values" ]
            [ div [ id "hs_slider", class "slide_wrapper" ]
                [ input
                    [ class "slider"
                    , type_ "range"
                    , Attr.min "0"
                    , Attr.max "10"
                    , Attr.step "1"
                    , value <| Round.round 1 (model.sim.circ.hs * 10)
                    , onInput HSUpdate
                    ]
                    []
                ]
            , div
                [ id "hs_slider_value" ]
                [ text <| Round.round 1 model.sim.circ.hs ]
            ]

        -- Ellie demo at https://ellie-app.com/3r83NMJQQkxa1
        , div [ id "audio" ]
            [ audio
                [ id "pulse-beep"
                , src "short-beep.mp3"
                , controls False
                ]
                []
            ]
        ]
