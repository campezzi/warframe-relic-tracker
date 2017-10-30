module Data
    exposing
        ( Era
        , Item
        , Rarity
        , Relic
        , fetchRelicData
        , relics
        )

import Http
import Json.Decode exposing (Decoder, andThen, map, list, string)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type Era
    = Lith
    | Meso
    | Neo
    | Axi


type alias Item =
    { name : String
    }


type Rarity
    = Common
    | Uncommon
    | Rare


type alias Relic =
    { era : Era
    , name : String
    , c1 : Item
    , c2 : Item
    , c3 : Item
    , u1 : Item
    , u2 : Item
    , r : Item
    }


fetchRelicData : (Result Http.Error Relic -> msg) -> Cmd msg
fetchRelicData msg =
    let
        request =
            Http.get "http://drops.warframestat.us/data/relics/Lith/A1.json" relicDecoder
    in
        Http.send msg request


relicDecoder : Decoder Relic
relicDecoder =
    decode Relic
        |> required "tier" (map eraDecoder string)
        |> required "name" string
        |> hardcoded (Item "a")
        |> hardcoded (Item "a")
        |> hardcoded (Item "a")
        |> hardcoded (Item "a")
        |> hardcoded (Item "a")
        |> hardcoded (Item "a")


eraDecoder : String -> Era
eraDecoder era =
    case era of
        "Lith" ->
            Lith

        "Meso" ->
            Meso

        "Neo" ->
            Neo

        _ ->
            Axi


relics : List Relic
relics =
    [ Relic
        Lith
        "A2"
        (Item "Lex Prime Barrel")
        (Item "Forma Blueprint")
        (Item "Valkyr Prime Blueprint")
        (Item "Cernos Prime Blueprint")
        (Item "Akbronco Prime Link")
        (Item "Akstiletto Prime Blueprint")
    , Relic
        Lith
        "B2"
        (Item "Paris Prime Lower Limb")
        (Item "Tigris Prime Stock")
        (Item "Forma Blueprint")
        (Item "Braton Prime Receiver")
        (Item "Orthos Prime Blade")
        (Item "Ballistica Prime Blueprint")
    ]
