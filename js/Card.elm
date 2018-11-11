module Card exposing
    ( Card
    , allCards
    , baseCard
    , blueberryJam
    , famines
    , fromSeed
    , marketDepressions
    , noModifier
    , taxes
    , tradeWar
    )

import Array
import BaseType exposing (..)
import Dict
import Material exposing (Fruit(..), Material)


type alias Card =
    { id_ : Int
    , name : String
    , description : String
    , startingBid : Int
    , yieldRateModifier : Material Float
    , priceModifier : Material Float
    , resourceCost : Material Int
    , charge : Uber Int
    }


fromSeed : Int -> Card
fromSeed seed =
    let
        id_ =
            seed % List.length allCards
    in
    case
        Array.get id_ (Array.fromList allCards)
    of
        Just c ->
            c

        Nothing ->
            Debug.crash "Should never go out of bound after modding"


allCards : List Card
allCards =
    List.indexedMap (\i c -> { c | id_ = i }) <|
        List.concat
            [ [ blueberryJam, tradeWar ]
            , Material.values famines
            , Material.values taxes
            , Material.values marketDepressions
            ]


baseCard : Card
baseCard =
    { id_ = -1 -- [hack] bogus value
    , name = "Untitled"
    , description = "No description"
    , startingBid = 3
    , yieldRateModifier = noModifier
    , priceModifier = noModifier
    , resourceCost = Material.empty
    , charge = Finite 1
    }


{-| @local
-}
blueberryJam : Card
blueberryJam =
    { baseCard
        | name = "Blueberry Jam"
        , resourceCost = Material.set Blueberry 10 Material.empty
    }


tradeWar : Card
tradeWar =
    { baseCard
        | name = "Trade War"
        , description = "When activated, the prices of all fruits will drop!"
        , priceModifier = Material.create (always 0.5)
        , resourceCost = Material.empty
    }


taxes : Material Card
taxes =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Tax"
                , priceModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Blueberry 5 Material.empty
            }
        )


{-| @global
[tofix] not effects yet; server needs to push this to all players
-}
famines : Material Card
famines =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Famine"
                , description = "When activated, the factories will yield less."
                , yieldRateModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Tomato 5 Material.empty
            }
        )


{-| @global
[tofix] not effects yet; server needs to push this to all players
-}
marketDepressions : Material Card
marketDepressions =
    Material.create
        (\fr ->
            { baseCard
                | name = toString fr ++ " Depression"
                , description = "When activated, demand for the fruit will drop."
                , priceModifier = Material.set fr 0.8 noModifier
                , resourceCost = Material.set Tomato 4 Material.empty
            }
        )



-- Helpers


noModifier : Material Float
noModifier =
    Material.create (always 1)
