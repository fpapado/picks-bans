module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, rel, src, style)
import Html.Events exposing (..)
import Task


-- Model
-- TODO: event name


type alias Model =
    { maps : List Map
    , currentPhase : Phase
    , currentTeam : Int
    , teams : List Team
    , mode : Mode
    }


type Mode
    = Bo3
    | Bo5


type alias Map =
    { id : Int
    , imgurl : String
    , title : String
    , status : Maybe Phase
    }


type alias Team =
    { id : Int
    , name : String
    }


type Phase
    = Pick
    | Ban



-- Update


type Msg
    = SetMapStatus Int
    | AdvancePhase


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMapStatus map_id ->
            ( { model
                | maps = List.map (changeMapStatus map_id model.currentPhase) model.maps
              }
            , Task.perform identity (Task.succeed AdvancePhase)
            )

        AdvancePhase ->
            let
                ( next_phase, next_team ) =
                    getNextPhase model.currentPhase model.currentTeam
            in
                ( { model
                    | currentPhase = next_phase
                    , currentTeam = next_team
                  }
                , Cmd.none
                )


changeMapStatus : Int -> Phase -> Map -> Map
changeMapStatus map_id phase mp =
    case mp.id == map_id of
        True ->
            { mp | status = Just phase }

        False ->
            mp


getNextPhase : Phase -> Int -> ( Phase, Int )
getNextPhase phase team =
    ( Pick, 1 )



-- View


view : Model -> Html Msg
view model =
    let
        currentTeam =
            List.filter (\t -> t.id == model.currentTeam) model.teams
                |> List.head
                |> Maybe.withDefault { name = "", id = -1 }
    in
        div [ class "mw8-ns pa3 center" ]
            [ h1 [ class "f2 f1-ns tc sans-serif navy" ] [ text "Picks and Bans" ]
            , phaseView currentTeam model.currentPhase
            , div [ class "flex flex-wrap" ]
                (List.map mapView model.maps)
            ]


phaseView : Team -> Phase -> Html Msg
phaseView team phase =
    let
        teamText =
            team.name

        phaseText =
            case phase of
                Pick ->
                    "is picking"

                Ban ->
                    "is banning"
    in
        h2 [ class "f3 f2-ns tc sans-serif" ] [ text <| teamText ++ " " ++ phaseText ]


mapView : Map -> Html Msg
mapView map =
    div [ class "ph2 mb3", style [ ( "flex", "0 0 33%" ) ] ]
        [ statusView map.status
        , img
            [ src map.imgurl
            , class "w-100"
            , onClick <| SetMapStatus map.id
            ]
            []
        ]


statusView : Maybe Phase -> Html Msg
statusView status =
    let
        mapStateMark =
            case status of
                Just Pick ->
                    "✔"

                Just Ban ->
                    "✘"

                Nothing ->
                    ""
    in
        span [ class "absolute f1 red" ]
            [ text mapStateMark
            ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Init


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { maps = initMaps, teams = initTeams, currentPhase = Pick, currentTeam = 0, mode = Bo3 }


initTeams : List Team
initTeams =
    [ { id = 0, name = "Team 1" }
    , { id = 1, name = "Team 2" }
    ]


initMaps : List Map
initMaps =
    [ { id = 0, imgurl = "images/Cache.jpg", title = "Cache", status = Nothing }
    , { id = 1, imgurl = "images/Cobblestone.jpg", title = "Cobblestone", status = Nothing }
    , { id = 2, imgurl = "images/Dust2.jpg", title = "Dust 2", status = Nothing }
    , { id = 3, imgurl = "images/Inferno.jpg", title = "Inferno", status = Nothing }
    , { id = 4, imgurl = "images/Mirage.jpg", title = "Mirage", status = Nothing }
    , { id = 5, imgurl = "images/Overpass.jpg", title = "Overpass", status = Nothing }
    , { id = 6, imgurl = "images/Train.jpg", title = "Train", status = Nothing }
    ]


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
