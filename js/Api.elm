module Api exposing (..)

import Json.Decode as D
import Json.Encode as E


decodeMessage : String -> Result String Action
decodeMessage =
    D.decodeString action



{- {"action":"game_state_changed","new_state":"auction"} -}


type GameStage
    = ReadyStage
    | ProductionStage


type Action
    = GameStateChanged GameStage


action : D.Decoder Action
action =
    D.field "action" D.string |> D.andThen actionHelp


actionHelp : String -> D.Decoder Action
actionHelp a =
    case a of
        "game_state_changed" ->
            D.map GameStateChanged <|
                (D.field "new_state" D.string
                    |> D.andThen
                        (\s ->
                            case s of
                                "ready" ->
                                    D.succeed ReadyStage

                                "production" ->
                                    D.succeed ProductionStage

                                _ ->
                                    D.fail "Unrecognized stage name"
                        )
                )

        _ ->
            D.fail ("Received unrecognized action from server: " ++ a)


type ServerAction
    = Ready


encodeServerAction : ServerAction -> String
encodeServerAction =
    E.encode 0 << serverAction


serverAction : ServerAction -> E.Value
serverAction a =
    case a of
        Ready ->
            E.object
                [ ( "action", E.string "ready" )
                ]
