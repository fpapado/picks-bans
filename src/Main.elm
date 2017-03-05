module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, rel, src, style)
import Html.Events exposing (..)
import Random
import Random.Extra exposing (sample)
import Svg
import Svg.Attributes exposing (version, viewBox, width, height, fill, d)
import Task
import Route
import Navigation


-- Model
-- TODO: event name


type alias Model =
    { maps : List Map
    , currentModeState : Maybe State
    , currentMode : Int
    , modes : List Mode
    , teams : List Team
    , route : Route.Model
    }


type alias Mode =
    { id : Int
    , mtype : ModeType
    , title : String
    , states : List State
    , currentState : Int
    }


type alias State =
    { id : Int
    , phase : Phase
    , teamID : Int
    }


type ModeType
    = Bo3
    | Bo1


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
    | Rand



-- Update


type Msg
    = SetMapStatus Int
    | AdvancePhase
    | SetRandom
    | NoOp
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modeState =
            model.currentModeState
    in
        case msg of
            SetMapStatus map_id ->
                ( { model
                    | maps = List.map (changeMapStatus map_id modeState) model.maps
                  }
                , Task.perform identity (Task.succeed AdvancePhase)
                )

            AdvancePhase ->
                let
                    currentMode =
                        List.filter (\m -> m.id == model.currentMode) model.modes
                            |> List.head
                            |> Maybe.withDefault bo3Mode

                    nextModeState =
                        getNextModeState modeState currentMode

                    nextCmd =
                        getNextStateCmd nextModeState
                in
                    ( { model
                        | currentModeState = nextModeState
                      }
                    , nextCmd
                    )

            SetRandom ->
                let
                    unpickedIDs =
                        List.filter (\m -> m.status == Nothing) model.maps
                            |> List.map (\m -> m.id)

                    unpickedIDGen =
                        sample unpickedIDs
                            |> Random.map (Maybe.withDefault 1)
                in
                    ( model, Random.generate SetMapStatus unpickedIDGen )

            NoOp ->
                ( model, Cmd.none )

            UrlChange location ->
                let
                    newRoute =
                        Route.locFor location
                in
                    ( { model | route = newRoute }, Cmd.none )


getNextStateCmd : Maybe State -> Cmd Msg
getNextStateCmd nextModeState =
    case nextModeState of
        Just modeState ->
            case modeState.phase of
                Rand ->
                    Task.perform identity (Task.succeed SetRandom)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


changeMapStatus : Int -> Maybe State -> Map -> Map
changeMapStatus map_id maybeState mp =
    case maybeState of
        Just state ->
            case mp.id == map_id of
                True ->
                    { mp | status = Just state.phase }

                False ->
                    mp

        Nothing ->
            mp


getNextModeState : Maybe State -> Mode -> Maybe State
getNextModeState maybeState currentMode =
    case maybeState of
        Just state ->
            let
                nextState =
                    state.id + 1

                nextModeState =
                    currentMode.states
                        |> List.filter (\s -> s.id == nextState)
                        |> List.head
            in
                nextModeState

        Nothing ->
            Nothing



-- View


view : Model -> Html Msg
view model =
    let
        body =
            case model.route of
                Just (Route.Setup) ->
                    text "Setup view"

                Just (Route.Play) ->
                    playView model

                Nothing ->
                    text "Page not found :("
    in
        main_ []
            [ headerView
            , Html.body [ class "mw8-ns pa3 center" ]
                [ body ]
            ]


headerView : Html Msg
headerView =
    header [ class "bg-black-90 w-100 ph3 pv3 pv4-ns ph4-m ph5-l" ]
        [ nav [ class "f6 fw6 ttu tracked" ] <|
            [ h1 [ class "f6 dib white mr5" ]
                [ text "Picks and Bans" ]
            ]
                ++ List.map link links
        ]


link : ( Route.Location, String ) -> Html Msg
link ( location, label ) =
    a [ href <| Route.urlFor location, class "dib dim link white mr3" ]
        [ text label ]


playView : Model -> Html Msg
playView model =
    let
        modeState =
            model.currentModeState

        currentTeam =
            case modeState of
                Nothing ->
                    { name = "", id = -1 }

                Just modeS ->
                    List.filter (\t -> t.id == modeS.teamID) model.teams
                        |> List.head
                        |> Maybe.withDefault { name = "", id = -1 }
    in
        div []
            [ stateView currentTeam modeState
            , div [ class "flex flex-wrap" ]
                (List.map mapView model.maps)
            , picksBansView model.maps
            ]


picksBansView : List Map -> Html Msg
picksBansView maps =
    let
        picked =
            List.filter (\m -> m.status == Just Pick || m.status == Just Rand) maps

        banned =
            List.filter (\m -> m.status == Just Ban) maps
    in
        div [ class "flex flex-wrap" ]
            [ listView "Picked" picked "blue"
            , listView "Banned" banned "red"
            ]


listView : String -> List Map -> String -> Html Msg
listView title maps style_ =
    let
        cellView =
            \m -> span [ class "db pa3 mb2 f5 b bg-near-white" ] [ text m.title ]
    in
        div [ class "ph2 mb3 mt3", style [ ( "flex", "0 0 50%" ) ] ]
            [ h3 [ class <| "f3 tc " ++ style_ ] [ text title ]
            , div [ class "" ] <| List.map cellView maps
            ]


stateView : Team -> Maybe State -> Html Msg
stateView team maybeState =
    case maybeState of
        Just state ->
            let
                phase =
                    state.phase

                teamText =
                    team.name

                phaseText =
                    case phase of
                        Pick ->
                            "is picking"

                        Ban ->
                            "is banning"

                        Rand ->
                            "Final map selected randomly"
            in
                h2 [ class "f3 f2-ns tc sans-serif" ] [ text <| teamText ++ " " ++ phaseText ]

        Nothing ->
            h2 [ class "f3 f2-ns tc sans-serif" ] [ text "Selection complete" ]


mapView : Map -> Html Msg
mapView map =
    let
        clickEffect =
            case map.status of
                Nothing ->
                    SetMapStatus map.id

                _ ->
                    NoOp
    in
        div
            [ class "ph2 mb3"
            , style [ ( "flex", "0 0 33%" ) ]
            , onClick <| clickEffect
            ]
            [ div [ class "relative" ]
                [ statusView map.status
                , img
                    [ src map.imgurl
                    , class "w-100"
                    ]
                    []
                ]
            ]


statusView : Maybe Phase -> Html Msg
statusView status =
    let
        mapStateMark =
            case status of
                Just Pick ->
                    Svg.svg
                        [ version "1.1"
                        , viewBox "0 0 32 32"
                        , width "100%"
                        , height "100%"
                        , fill "#0fb800"
                        ]
                        [ Svg.path [ d "M1 14 L5 10 L13 18 L27 4 L31 8 L13 26 z" ]
                            []
                        ]

                Just Ban ->
                    Svg.svg
                        [ version "1.1"
                        , viewBox "0 0 32 32"
                        , width "100%"
                        , height "100%"
                        , fill "#cc0011"
                        ]
                        [ Svg.path [ d "M16 0 A16 16 0 0 0 0 16 A16 16 0 0 0 16 32 A16 16 0 0 0 32 16 A16 16 0 0 0 16 0 M16 6 A10 10 0 0 1 20.675 7 L7 20.675 A10 10 0 0 1 6 16 A10 10 0 0 1 16 6 M26 16 A10 10 0 0 1 16 26 A10 10 0 0 1 11.325 25 L25 11.325 A10 10 0 0 1 26 16" ]
                            []
                        ]

                Just Rand ->
                    Svg.svg
                        [ version "1.1"
                        , viewBox "0 0 32 32"
                        , width "100%"
                        , height "100%"
                        , fill "#006bb7"
                        ]
                        [ Svg.path [ d "M1 14 L5 10 L13 18 L27 4 L31 8 L13 26 z" ]
                            []
                        ]

                Nothing ->
                    Svg.svg [] []
    in
        div [ class "absolute w-100 h-100" ]
            [ div
                [ class "pa3 w-100 h-100"
                ]
                [ mapStateMark ]
            ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Init


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( initModel, Cmd.none )


links : List ( Route.Location, String )
links =
    [ ( Route.Setup, "Setup" )
    , ( Route.Play, "Selection" )
    ]


bo3Mode : Mode
bo3Mode =
    { id = 0, mtype = Bo3, title = "Best of 3", states = bo3States, currentState = 0 }


bo1Mode : Mode
bo1Mode =
    { id = 0, mtype = Bo1, title = "Best of 5", states = bo1States, currentState = 0 }


bo3States : List State
bo3States =
    [ { id = 0, teamID = 0, phase = Ban }
    , { id = 1, teamID = 1, phase = Ban }
    , { id = 2, teamID = 1, phase = Pick }
    , { id = 3, teamID = 0, phase = Pick }
    , { id = 4, teamID = -1, phase = Rand }
    ]


bo1States : List State
bo1States =
    [ { id = 0, teamID = 0, phase = Ban }
    , { id = 1, teamID = 1, phase = Ban }
    , { id = 2, teamID = 1, phase = Ban }
    , { id = 3, teamID = 0, phase = Ban }
    , { id = 4, teamID = -1, phase = Rand }
    ]


initModel : Model
initModel =
    { maps = initMaps
    , teams = initTeams
    , currentModeState = Just initModeState
    , currentMode = 0
    , modes = [ bo3Mode, bo1Mode ]
    , route = Route.init (Just Route.Setup)
    }


initModeState : State
initModeState =
    { id = 0
    , phase = Ban
    , teamID = 0
    }


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
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
