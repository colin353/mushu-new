module Helper exposing (..)

import Model exposing (..)


bidIncrement : number
bidIncrement =
    5


nextBid : Auction -> Int
nextBid auction =
    case auction.highestBid of
        Just { bid } ->
            bid + bidIncrement

        Nothing ->
            auction.card.startingBid
