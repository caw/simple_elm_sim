module Circ exposing (Circulation, circ, circView, dt, secondsInMinute, starling, updateCirculation)

import Html exposing (Html, div, h6, p, span, text)
import Html.Attributes exposing (class, id)
import Messages exposing (Msg(..))
import Round exposing (..)


secondsInMinute : Float
secondsInMinute =
    60.0


type alias Circulation =
    { cra : Float
    , ca : Float
    , cv : Float
    , vra : Float
    , va : Float
    , vv : Float
    , vrae : Float
    , vae : Float
    , vve : Float
    , vra0 : Float
    , va0 : Float
    , vv0 : Float
    , fa : Float
    , fan : Float
    , fc : Float
    , fv : Float
    , ra : Float
    , rv : Float
    , pga : Float
    , pgv : Float
    , pa : Float
    , pv : Float
    , pra : Float
    , hs : Float
    }


circ : Circulation
circ =
    { cra = 0.005
    , ca = 0.00355
    , cv = 0.0825
    , vra = 0.1
    , va = 0.85
    , vv = 3.25
    , vrae = 0.0
    , vae = 0.355
    , vve = 0.3
    , vra0 = 0.1
    , va0 = 0.495
    , vv0 = 2.95
    , fa = 5.0 / secondsInMinute
    , fc = 5.0 / secondsInMinute
    , fv = 5.0 / secondsInMinute
    , fan = 5.0 / secondsInMinute
    , ra = 19.34 * secondsInMinute
    , rv = 0.74 * secondsInMinute
    , pga = 96.3
    , pgv = 3.7
    , pa = 100.0
    , pv = 3.7
    , pra = 0.0
    , hs = 1.0
    }


dt : Float
dt =
    0.001


starling : Float -> Float
starling pra =
    let
        r2 =
            6.0 * (pra + 4)

        r3 =
            r2 ^ 2.5
    in
    ((r3 / (5000 + r3)) * 13 + 0.5) / secondsInMinute


updateCirculation : Circulation -> Circulation
updateCirculation c =
    -- dt is defined above as a global in this module
    let
        vrae =
            c.vra - c.vra0

        pra =
            vrae / c.cra

        fan =
            starling pra

        fa =
            fan * c.hs

        dva =
            fa - c.fc

        vae =
            c.va - c.va0

        pa =
            vae / c.ca

        pga =
            pa - c.pv

        fc =
            pga / c.ra

        dvv =
            fc - c.fv

        vve =
            c.vv - c.vv0

        pv =
            vve / c.cv

        pgv =
            pv - pra

        fv =
            pgv / c.rv

        dvra =
            fv - fa

        va =
            c.va + dva * dt

        vv =
            c.vv + dvv * dt

        vra =
            c.vra + dvra * dt
    in
    { c
        | vrae = vrae
        , pra = pra
        , fan = fan
        , fa = fa
        , vae = vae
        , pa = pa
        , pga = pga
        , fc = fc
        , vve = vve
        , pv = pv
        , pgv = pgv
        , fv = fv
        , va = va
        , vv = vv
        , vra = vra
    }


makeResult : String -> Int -> Float -> String -> Html Msg
makeResult label rounding value units =
    div [ class "result_box" ]
        [ p [ class "label" ] [ text label ]
        , p [ class "value" ] [ text (Round.round rounding value), span [ class "units" ] [ text units ] ]
        ]



-- maybe make a type = Int | Float | String for displayed values... maybe


type alias Displayed =
    { name : String, rounding : Int, value : Float, units : String }


circView : Circulation -> List (Html Msg)
circView c =
    let
        displayed =
            [ Displayed "Pa" 0 c.pa "mmHg"
            , Displayed "Pv" 1 c.pv "mmHg"
            , Displayed "Pra" 1 c.pra "mmHg"
            , Displayed "Ca" 5 c.ca ""
            , Displayed "Cv" 4 c.cv ""
            , Displayed "Cra" 3 c.cra ""
            , Displayed "Va" 2 c.va "L"
            , Displayed "Vv" 2 c.vv "L"
            , Displayed "Vra" 2 c.vra "L"
            , Displayed "Vae" 3 c.vae "mmHg"
            , Displayed "Vve" 3 c.vve "mmHg"
            , Displayed "Vrae" 3 c.vrae "mmHg"
            , Displayed "Va0" 0 c.va0 "mmHg"
            , Displayed "Vv0" 1 c.vv0 "mmHg"
            , Displayed "Vra0" 1 c.vra0 "mmHg"
            , Displayed "Fa" 1 c.fa "L/min"
            , Displayed "Fc" 1 c.fc "L/min"
            , Displayed "Fv" 1 c.fv "L/min"
            , Displayed "Fan" 1 c.fan "L/min"
            , Displayed "Ra" 2 c.ra ""
            , Displayed "Rv" 2 c.rv ""
            , Displayed "Pga" 1 c.pga "mmHg"
            , Displayed "Pgv" 1 c.pgv "mmHg"
            , Displayed "HS" 1 c.hs ""
            ]
    in
    List.map
        (\{ name, rounding, value, units } -> makeResult name rounding value units)
        displayed
