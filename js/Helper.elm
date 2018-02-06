module Helper exposing (..)

import BaseType exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Material exposing (Fruit, Material)
import Api
import Server
import Array exposing (Array)


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


tryApplyCardEffect : Int -> Model -> Result String ( Model, Cmd Msg )
tryApplyCardEffect index model =
    model.cards
        |> Array.fromList
        |> Array.get index
        |> Result.fromMaybe "Cannot found card. Index mismatch."
        |> Result.andThen
            (\card ->
                let
                    removeFromInv : Model -> Result String Model
                    removeFromInv m =
                        m.inventory
                            |> Material.trySubtract card.resourceCost
                            |> Result.fromMaybe
                                ("Not enough resources."
                                    ++ "Card shouldn't have been activatable"
                                )
                            |> Result.map (\inv -> { m | inventory = inv })

                    removeCharge : Model -> Model
                    removeCharge m =
                        { m
                            | cards =
                                let
                                    chargeLeft =
                                        BaseType.add card.charge -1
                                in
                                    m.cards
                                        |> Array.fromList
                                        |> (if chargeLeft == Finite 0 then
                                                (\a ->
                                                    case arrayRemove index a of
                                                        Just a ->
                                                            a

                                                        Nothing ->
                                                            Debug.crash "Index"
                                                )
                                            else
                                                Array.set index
                                                    { card
                                                        | charge =
                                                            chargeLeft
                                                    }
                                           )
                                        |> Array.toList
                        }

                    toServer : Model -> Cmd Msg
                    toServer =
                        flip Server.send
                            (Api.ApplyEffect
                                { yieldRateModifier = card.yieldRateModifier
                                , priceModifier = card.priceModifier
                                }
                            )
                in
                    model
                        |> removeFromInv
                        |> Result.andThen (removeCharge >> Result.Ok)
                        |> Result.map (\m -> ( m, toServer m ))
            )


arrayRemove : Int -> Array a -> Maybe (Array a)
arrayRemove i array =
    Array.get i array
        |> Maybe.map
            (always
                (Array.append (Array.slice 0 i array)
                    (Array.slice (i + 1) (Array.length array) array)
                )
            )


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
