module Data
    exposing
        ( Era
        , Item
        , ItemCollection
        , Relic
        , fetchRelicData
        )

import Http
import Json.Decode exposing (Decoder, float, list, map, string)
import Json.Decode.Pipeline exposing (decode, required)


type Era
    = Lith
    | Meso
    | Neo
    | Axi


type alias Item =
    { name : String
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
    }


type alias Reward =
    { item : Item
    , rarity : Rarity
    }


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
            \relics ->
                List.foldr
                    (\r acc ->
                        case r of
                            Nothing ->
                                acc

                            Just relic ->
                                relic :: acc
                    )
                    []
                    relics
    in
    decode onlyValid |> required "relics" (list relicDecoder)


relicDecoder : Decoder (Maybe Relic)
relicDecoder =
    decode mkRelic
        |> required "tier" string
        |> required "relicName" string
        |> required "state" string
        |> required "rewards" (list rewardDecoder)


rewardDecoder : Decoder Reward
rewardDecoder =
    decode Reward
        |> required "itemName" (map Item string)
        |> required "chance" (map mkRarity float)


mkRelic : String -> String -> String -> List Reward -> Maybe Relic
mkRelic era name state rewards =
    case state of
        "Intact" ->
            let
                items =
                    List.sortWith rarityComparison rewards |> List.map .item |> toTuple

                toTuple =
                    \collection ->
                        case collection of
                            [ c1, c2, c3, u1, u2, r ] ->
                                Just ( c1, c2, c3, u1, u2, r )

                            _ ->
                                Nothing
            in
            Maybe.map3 Relic (mkEra era) (Just name) items

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
