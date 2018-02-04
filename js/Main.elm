module Main exposing (main)

import Model exposing (Model)
import Msg
import Update
import View
import Html


main : Program Never Model Msg.Msg
main =
    Html.program
        { init = ( Model.initModel, Cmd.none )
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
