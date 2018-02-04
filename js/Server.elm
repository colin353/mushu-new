module Server exposing (..)

import WebSocket
import Msg exposing (..)
import Api exposing (..)


type alias SendToServer =
    Api.ServerAction -> Cmd Msg


wsURL : String -> String
wsURL hostname =
    ("ws://" ++ hostname ++ "/join")


send : { m | hostname : String } -> Api.ServerAction -> Cmd Msg
send { hostname } =
    WebSocket.send (wsURL hostname) << Api.encodeToMessage


listen : { m | hostname : String } -> (Result String Api.Action -> Msg) -> Sub Msg
listen { hostname } handler =
    WebSocket.listen (wsURL hostname) (handler << Api.decodeMessage)
