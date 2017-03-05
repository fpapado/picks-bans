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

            [ "selection" ] ->
                Just Play

            _ ->
                Nothing


urlFor : Location -> String
urlFor location =
    let
        url =
            case location of
                Setup ->
                    "/"

                Play ->
                    "/selection"
    in
        "#" ++ url
