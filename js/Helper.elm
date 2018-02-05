module Helper exposing (..)

import Material exposing (Fruit, Material)
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


move :
    Fruit
    -> Int
    -> Material Int
    -> Material Int
    -> Maybe ( Material Int, Material Int )
move fruit count mat1 mat2 =
    let
        newMat1 =
            Material.update fruit
                (flip (-) count)
                mat1

        newMat2 =
            Material.update fruit
                ((+) count)
                mat2
    in
        if
            Material.lookup fruit newMat1
                < 0
                || Material.lookup fruit newMat2
                < 0
        then
            Nothing
        else
            Just ( newMat1, newMat2 )
