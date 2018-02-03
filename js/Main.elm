module Main exposing (..)

import Model
import Msg
import Update
import View
import Html


main : Program Never Model.Model Msg.Msg
main =
    Html.program
        { init = Model.init
        , view = View.view
        , update = Update.update
        , subscriptions = Update.subscriptions
        }
