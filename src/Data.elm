module Data
    exposing
        ( Era
        , Item
        , ItemCollection
        , ItemId
        , Relic
        , containsItemWith
        , fetchRelicData
        , itemStartsWith
        , toItemList
        )

import Http
import Json.Decode exposing (Decoder, float, list, map, string)
import Json.Decode.Pipeline exposing (decode, required)
import String exposing (startsWith, toLower)


type Era
    = Lith
    | Meso
    | Neo
    | Axi


type alias ItemId =
    String


type alias Item =
    { itemId : ItemId
    , name : String
    }


type alias ItemCollection =
    ( Item, Item, Item, Item, Item, Item )


type Rarity
    = Common
    | Uncommon
    | Rare


type alias Relic =
    { era : Era
    , name : String
    , items : ItemCollection
    , vaulted : Bool
    }


type alias Reward =
    { item : Item
    , rarity : Rarity
    }


containsItemWith : String -> Relic -> Bool
containsItemWith term { items } =
    List.any (itemStartsWith term) (toItemList items)


itemStartsWith : String -> Item -> Bool
itemStartsWith term { name } =
    startsWith (toLower term) (toLower name)


toItemList : ItemCollection -> List Item
toItemList items =
    let
        ( c1, c2, c3, u1, u2, r ) =
            items
    in
    [ c1, c2, c3, u1, u2, r ]


toItemCollection : List Item -> Maybe ItemCollection
toItemCollection items =
    case items of
        [ c1, c2, c3, u1, u2, r ] ->
            Just ( c1, c2, c3, u1, u2, r )

        _ ->
            Nothing


fetchRelicData : (Result Http.Error (List Relic) -> msg) -> Cmd msg
fetchRelicData msg =
    let
        request =
            Http.get "http://drops.warframestat.us/data/relics.json" relicsDecoder
    in
    Http.send msg request


relicsDecoder : Decoder (List Relic)
relicsDecoder =
    let
        onlyValid =
            List.foldr
                (\r acc ->
                    case r of
                        Nothing ->
                            acc

                        Just relic ->
                            relic :: acc
                )
                []

        sorted =
            List.sortWith relicComparison
    in
    decode (onlyValid >> sorted) |> required "relics" (list relicDecoder)


relicDecoder : Decoder (Maybe Relic)
relicDecoder =
    decode mkRelic
        |> required "tier" string
        |> required "relicName" string
        |> required "state" string
        |> required "rewards" (list rewardDecoder)


rewardDecoder : Decoder Reward
rewardDecoder =
    decode mkReward
        |> required "_id" string
        |> required "itemName" string
        |> required "chance" (map mkRarity float)


mkReward : ItemId -> String -> Rarity -> Reward
mkReward itemId itemName rarity =
    let
        item =
            Item itemId itemName
    in
    Reward item rarity


mkRelic : String -> String -> String -> List Reward -> Maybe Relic
mkRelic era name state rewards =
    case state of
        "Intact" ->
            let
                mEra =
                    mkEra era

                mName =
                    Just name

                mItems =
                    List.sortWith rarityComparison rewards |> List.map .item |> toItemCollection

                mVaulted =
                    Maybe.map2 isVaulted mEra mName
            in
            Maybe.map4
                Relic
                mEra
                mName
                mItems
                mVaulted

        _ ->
            Nothing


mkEra : String -> Maybe Era
mkEra str =
    case str of
        "Lith" ->
            Just Lith

        "Meso" ->
            Just Meso

        "Neo" ->
            Just Neo

        "Axi" ->
            Just Axi

        _ ->
            Nothing


mkRarity : Float -> Rarity
mkRarity dropChance =
    case dropChance of
        2 ->
            Rare

        11 ->
            Uncommon

        _ ->
            Common


relicComparison : Relic -> Relic -> Order
relicComparison a b =
    let
        ea =
            toInt a.era

        eb =
            toInt b.era

        toInt =
            \era ->
                case era of
                    Lith ->
                        1

                    Meso ->
                        2

                    Neo ->
                        3

                    Axi ->
                        4

        eraComparison =
            compare ea eb
    in
    case eraComparison of
        EQ ->
            compare a.name b.name

        _ ->
            eraComparison


rarityComparison : Reward -> Reward -> Order
rarityComparison a b =
    let
        ra =
            toInt a.rarity

        rb =
            toInt b.rarity

        toInt =
            \rarity ->
                case rarity of
                    Common ->
                        1

                    Uncommon ->
                        2

                    Rare ->
                        3
    in
    compare ra rb


isVaulted : Era -> String -> Bool
isVaulted era name =
    let
        vaultedRelics =
            case era of
                Lith ->
                    [ "A1"
                    , "B1"
                    , "C1"
                    , "F1"
                    , "F2"
                    , "G1"
                    , "K1"
                    , "M1"
                    , "N1"
                    , "N2"
                    , "S1"
                    , "S2"
                    , "S3"
                    , "S4"
                    , "S5"
                    , "S6"
                    , "V1"
                    ]

                Meso ->
                    [ "B1"
                    , "C1"
                    , "C2"
                    , "D1"
                    , "F1"
                    , "F2"
                    , "M1"
                    , "N1"
                    , "N2"
                    , "N3"
                    , "S1"
                    , "S3"
                    , "S4"
                    , "V1"
                    , "V2"
                    , "V3"
                    , "V4"
                    ]

                Neo ->
                    [ "A1"
                    , "B1"
                    , "B3"
                    , "D1"
                    , "N1"
                    , "N2"
                    , "N3"
                    , "N4"
                    , "N5"
                    , "N6"
                    , "N7"
                    , "S1"
                    , "S2"
                    , "S3"
                    , "S5"
                    , "S6"
                    , "T1"
                    , "V1"
                    , "V3"
                    , "V4"
                    ]

                Axi ->
                    [ "A1"
                    , "A2"
                    , "B1"
                    , "C1"
                    , "C2"
                    , "E1"
                    , "G1"
                    , "H1"
                    , "H2"
                    , "K1"
                    , "N1"
                    , "N2"
                    , "N3"
                    , "R1"
                    , "S1"
                    , "T1"
                    , "V1"
                    , "V2"
                    , "V3"
                    , "V4"
                    , "V5"
                    ]
    in
    List.member name vaultedRelics
