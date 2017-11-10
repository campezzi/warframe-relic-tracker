module Main exposing (..)

import Data exposing (..)
import Html exposing (Html, button, div, h1, h3, input, label, li, text, ul)
import Html.Attributes exposing (class, for, id, style, type_)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Ports.LocalStorage as LocalStorage


type alias Model =
    { relics : List Relic
    , acquired : List ItemId
    , searchTerm : String
    , showVaulted : Bool
    }


type Msg
    = AcquiredItemsLoaded ( LocalStorage.Key, LocalStorage.Value )
    | RelicDataResponse (Result Http.Error (List Relic))
    | ItemClicked Item
    | SearchTermChanged String
    | ShowVaultedChanged Bool
    | ClearAcquiredItems


localStorageKey : String
localStorageKey =
    "acquiredItems"


main : Program Never Model Msg
main =
    let
        commands =
            Cmd.batch
                [ fetchRelicData RelicDataResponse
                , LocalStorage.storageGetItem localStorageKey
                ]
    in
    Html.program
        { init = ( model, commands )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


model : Model
model =
    { relics = []
    , acquired = []
    , searchTerm = ""
    , showVaulted = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    LocalStorage.storageGetItemResponse AcquiredItemsLoaded


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AcquiredItemsLoaded ( _, value ) ->
            case JD.decodeValue (JD.list JD.string) value of
                Ok loadedItems ->
                    ( { model | acquired = loadedItems }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RelicDataResponse result ->
            case result of
                Ok relics ->
                    ( { model | relics = relics }, Cmd.none )

                Err e ->
                    let
                        log =
                            Debug.log "error" e
                    in
                    ( model, Cmd.none )

        ItemClicked { itemId } ->
            let
                ( acquired_, cmd ) =
                    if List.member itemId model.acquired then
                        ( List.filter (\i -> i /= itemId) model.acquired
                        , LocalStorage.storageRemoveFromSet ( localStorageKey, JE.string itemId )
                        )
                    else
                        ( itemId :: model.acquired
                        , LocalStorage.storagePushToSet ( localStorageKey, JE.string itemId )
                        )
            in
            ( { model | acquired = acquired_ }, cmd )

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        ShowVaultedChanged showVaulted ->
            ( { model | showVaulted = showVaulted }, Cmd.none )

        ClearAcquiredItems ->
            ( { model | acquired = [] }, LocalStorage.storageRemoveItem localStorageKey )


view : Model -> Html Msg
view { relics, searchTerm, showVaulted, acquired } =
    let
        searchFilter =
            List.filter (containsItemWith searchTerm)

        vaultedFilter =
            if showVaulted then
                identity
            else
                List.filter (not << .vaulted)

        filteredRelics =
            relics |> searchFilter |> vaultedFilter

        titleView =
            h1 [ class "center" ] [ text "Warframe Relic Tracker" ]

        searchField =
            div []
                [ text "Find Item: "
                , input [ onInput SearchTermChanged ] [ text searchTerm ]
                ]

        toggleVaulted =
            div []
                [ input [ id "vaultedCheckbox", type_ "checkbox", onCheck ShowVaultedChanged ] []
                , label [ for "vaultedCheckbox" ] [ text "Show vaulted relics" ]
                ]

        relicList =
            div [ class "flex flex-wrap justify-center" ] (List.map relicView filteredRelics)

        relicView =
            \{ era, name, items, vaulted } ->
                let
                    ( vaultedText, bgColor ) =
                        if vaulted then
                            ( " (Vaulted)", "gray" )
                        else
                            ( "", "blue" )
                in
                div [ class "overflow-hidden border rounded relic-box" ]
                    [ div [ class ("p2 bold white bg-" ++ bgColor) ] [ text (toString era ++ " " ++ name ++ vaultedText) ]
                    , div [ class "p2" ] [ div [] (List.indexedMap itemView (toItemList items)) ]
                    ]

        itemView =
            \index ({ name, itemId } as item) ->
                let
                    searchMatchStyle =
                        if searchTerm /= "" && itemStartsWith searchTerm item then
                            [ ( "background-color", "#EFEFEF" ) ]
                        else
                            []

                    color =
                        if index <= 2 then
                            "rgb(198,124,68)"
                        else if index <= 4 then
                            "rgb(147,144,145)"
                        else
                            "rgb(213,165,73)"

                    colorStyle =
                        [ ( "color", color ) ]

                    styles =
                        searchMatchStyle ++ colorStyle

                    itemText =
                        if List.member itemId acquired then
                            name ++ " ✔"
                        else
                            name
                in
                div [ onClick (ItemClicked item), style styles ] [ text itemText ]

        clearAcquiredItemsButton =
            button [ onClick ClearAcquiredItems ] [ text "Clear Acquired Items" ]
    in
    div []
        [ titleView
        , searchField
        , toggleVaulted
        , clearAcquiredItemsButton
        , relicList
        ]
