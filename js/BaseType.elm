module BaseType exposing (..)

import Material exposing (Material)


type StageType
    = ReadyStageType
    | ProductionStageType
    | AuctionStageType
    | TradeStageType


type alias CardSeed =
    Int


type alias Price =
    Material Float


type Uber number
    = Finite number
    | Infinite


add : Uber number -> number -> Uber number
add x y =
    case x of
        Finite z ->
            Finite (z + y)

        Infinite ->
            Infinite
