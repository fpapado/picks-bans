module Route exposing (..)

import String exposing (split)
import Navigation


type Location
    = Setup
    | Play


type alias Model =
    Maybe Location


init : Maybe Location -> Model
init location =
    location


locFor : Navigation.Location -> Maybe Location
locFor path =
    let
        segments =
            path.hash
                |> split "/"
                |> List.filter (\seg -> seg /= "" && seg /= "#")
    in
        case segments of
            [] ->
                Just Setup

            [ "play" ] ->
                Just Play

            _ ->
                Nothing
