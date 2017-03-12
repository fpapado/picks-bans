module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, href, placeholder, rel, src, style, checked, type_, for, id)
import Html.Events exposing (..)
import Model exposing (Model, links)
import Msg exposing (Msg(..))
import Route
import Svg
import Svg.Attributes exposing (version, viewBox, width, height, fill, d)
import Types exposing (..)
import Dict


view : Model -> Html Msg
view model =
    let
        body =
            case model.route of
                Just (Route.Setup) ->
                    setupView model

                Just (Route.Play) ->
                    playView model

                Nothing ->
                    text "Page not found :("
    in
        main_ [ class "sans-serif" ]
            [ headerView
            , Html.body [ class "mw8-ns pa3 center" ]
                [ body ]
            ]


headerView : Html Msg
headerView =
    header [ class "bg-navy w-100 ph3 pv3 pv4-ns ph4-m ph5-l" ]
        [ nav [ class "mw8-ns f6 fw6 ttu tracked center" ] <|
            [ h1 [ class "f6 dib white mr5" ]
                [ text "Picks and Bans" ]
            ]
                ++ List.map link links
                ++ [ a
                        [ class "dib pointer dim link white mr3"
                        , onClick RestartSelection
                        ]
                        [ text "Restart" ]
                   ]
        ]


link : ( Route.Location, String ) -> Html Msg
link ( location, label ) =
    a [ href <| Route.urlFor location, class "dib dim link white mr3" ]
        [ text label ]


btnLink : ( Route.Location, String ) -> Html Msg
btnLink ( location, title ) =
    a
        [ class "w-100 w-50-ns input-reset dib mr3 f5 pa3 link bw1 ba b--dark-blue bg-white black-80 pointer bg-animate hover-white hover-bg-dark-blue"
        , href <| Route.urlFor location
        ]
        [ text title ]


setupView : Model -> Html Msg
setupView model =
    -- set mode: set currentMode and currentModeState
    -- set maps:
    -- setTeamName: ...
    div []
        [ h2 [ class "f3 f2-ns tc sans-serif" ] [ text "Setup" ]
        , div [ class "clearfix" ]
            [ div [ class "fl w-100 w-50-ns pa3" ]
                [ modeSelectView model
                , teamNameSelectView model
                , eventNameSelectView model
                ]
            , div [ class "fl w-100 w-50-ns pv0 ph3 pv3-ns" ]
                [ mapSelectView model
                , btnLink ( Route.Play, "Start" )
                ]
            ]
        ]


modeSelectView : Model -> Html Msg
modeSelectView model =
    div [ class "" ]
        [ h3 [ class "mt0 mb2 f4 navy" ] [ text "Mode Select" ]
        , fieldset [] <|
            List.map
                (\mt -> checkBox (SetMode mt.id) mt.title (mt.id == model.currentMode))
                model.modes
        ]


mapSelectView : Model -> Html Msg
mapSelectView model =
    let
        checkBoxItem mp =
            checkBox (ToggleMapInPlay mp.title) mp.title mp.inPlay
    in
        div [ class "mb3" ]
            [ h3 [ class "mt4 mt0-ns mb2 f4 navy" ] [ text "Map Select" ]
            , fieldset [] <|
                List.map checkBoxItem (Dict.values model.allMaps)
            ]


teamNameSelectView : Model -> Html Msg
teamNameSelectView model =
    let
        inputItem tm =
            input
                [ type_ "text"
                , onInput (SetTeamName tm.id)
                , class "input-reset ba b--black-20 pa2 mt3 mb3 db w-100"
                , placeholder tm.name
                ]
                []
    in
        div [ class "" ]
            [ h3 [ class "mt4 mb2 f4 navy" ] [ text "Team Names" ]
            , fieldset [] <| List.map inputItem model.teams
            ]


eventNameSelectView : Model -> Html Msg
eventNameSelectView model =
    div [ class "" ]
        [ h3 [ class "mt4 mb2 f4 navy" ] [ text "Event name" ]
        , fieldset []
            [ input
                [ type_ "text"
                , onInput (SetEventName)
                , class "input-reset ba b--black-20 pa2 mt3 mb3 db w-100"
                , placeholder model.eventName
                ]
                []
            ]
        ]


checkBox : msg -> String -> Bool -> Html msg
checkBox msg title check =
    let
        titleAsID =
            title
                |> String.split " "
                |> String.join ""
                |> String.toLower
    in
        div [ class "flex items-center mb2" ]
            [ input [ type_ "checkbox", onClick msg, checked check, class "mr2", id titleAsID ] []
            , label [ class "lh-copy", for titleAsID ] [ text title ]
            ]


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
            [ eventNameView model.eventName
            , warningView model
            , stateView currentTeam modeState
            , div [ class "flex flex-wrap" ]
                (List.map mapView (Dict.values model.playMaps))
            , picksBansView (Dict.values model.playMaps)
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


eventNameView : String -> Html Msg
eventNameView name =
    h3 [ class "f3 dark-blue tc" ] [ text name ]


warningView : Model -> Html Msg
warningView model =
    let
        warning content =
            case content of
                "" ->
                    div [] []

                _ ->
                    div [ class "db pa3 f5 bg-red white" ] [ text content ]

        emptyMapsWarn =
            case (Dict.isEmpty model.playMaps) of
                True ->
                    "You have not selected any maps. Go back to the setup screen and check the relevant boxes."

                False ->
                    ""
    in
        warning emptyMapsWarn


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
                    SetMapStatus map.title

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
