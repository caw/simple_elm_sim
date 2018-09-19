module Main exposing (Model, Simulation, init, main, subscriptions, update, view)

import Browser
import Circ exposing (Circulation, circ, circView)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
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


type UIButton
    = RunButton
    | PauseButton
    | FinishButton


type Result
    = IntResult Int
    | FloatResult Float
    | StringResult String


type Effect
    = OximetryOn
    | IVIn


type alias Simulation =
    { runningState : SimRunningState
    , runningTime : Int
    , circ : Circulation
    }



{-
   Investigation: things like FBE, ECG
   timeLocked = time (number of seconds) it takes to get the sample / do the ECG / do the CxR
   timeForResult = time it takes to get the result back
-}


type alias Investigation =
    { description : String
    , timeTaken : Int
    , timeForResult : Int
    , result : Result
    }



{-
   Intervention : things like put on sat probe, insert IV
   timeTaken = time (number of seconds) to do the thing

-}


initialSim : Simulation
initialSim =
    { runningState = NotStarted
    , runningTime = 0
    , circ = circ
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

                    newSim =
                        { sim | runningTime = sim.runningTime + 1 }
                in
                ( { model | sim = newSim }
                , Cmd.none
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



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
        [ h1 [] [ text (toMinSec rt ++ ":" ++ toString sim.runningState) ]
        , div []
            [ button [ onClick Run, disabled (btnState RunButton model) ] [ text "Run" ]
            , button [ onClick Pause, disabled (btnState PauseButton model) ] [ text "Pause" ]
            , button [ onClick Finish, disabled (btnState FinishButton model) ] [ text "Finish" ]
            ]
        , hr [] []
        , div [ class "circulation_container" ] (circView model.sim.circ)
        ]
