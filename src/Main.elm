module Main exposing (..)

import Model exposing (Model)
import View
import Update
import Msg exposing (Msg(..))
import Navigation


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = Model.init
        , update = Update.update
        , subscriptions = subscriptions
        , view = View.view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
