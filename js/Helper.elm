module Helper exposing (..)

import Model exposing (..)
import Msg exposing (Msg)
import Material exposing (Fruit, Material)
import Card exposing (Card)


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


tryApplyCardEffect : Card -> Model -> Result String ( Model, Cmd Msg )
tryApplyCardEffect card model =
    let
        setInv inv m =
            ( { m | inventory = inv }, Cmd.none )
    in
        (case model.inventory |> Material.trySubtract card.resourceCost of
            Just inv ->
                Ok inv

            Nothing ->
                Err
                    ("Not enough resources."
                        ++ "Card shouldn't have been activatable"
                    )
        )
            |> Result.map (flip setInv model)


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


isOk : Result e a -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result e a -> Bool
isErr =
    not << isOk
