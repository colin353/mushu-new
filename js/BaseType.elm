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
