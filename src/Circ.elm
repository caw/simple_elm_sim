module Circ exposing (Circulation, circ, circView, dt, secondsInMinute, starling, updateCirculation)

import Html exposing (Html, div, p, text)
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


circView : Circulation -> Html Msg
circView c =
    let
        pa_ =
            text (Round.round 0 c.pa)

        pra_ =
            text (Round.round 1 c.pra)

        pv_ =
            text (Round.round 1 c.pv)

        fa_ =
            text (Round.round 1 (c.fa * 60))

        ra_ =
            text (Round.round 2 c.ra)
    in
    div []
        [ text "PA: "
        , pa_
        , p [] []
        , text "PV: "
        , pv_
        , p [] []
        , text "PRA: "
        , pra_
        , p [] []
        , text "CO: "
        , fa_
        , p [] []
        , text "RA: "
        , ra_
        ]