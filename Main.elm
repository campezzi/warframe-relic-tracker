module Main exposing (..)

import Data exposing (..)
import Html exposing (Html, div, h3, input, label, li, text, ul)
import Html.Attributes exposing (for, id, style, type_)
import Html.Events exposing (onCheck, onClick, onInput)
import Http


type alias Model =
    { relics : List Relic
    , acquired : List Item
    , searchTerm : String
    , showVaulted : Bool
    }


type Msg
    = RelicDataResponse (Result Http.Error (List Relic))
    | ItemClicked Item
    | SearchTermChanged String
    | ShowVaultedChanged Bool


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, fetchRelicData RelicDataResponse )
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
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemClicked item ->
            let
                acquired_ =
                    if List.member item model.acquired then
                        List.filter (\i -> i /= item) model.acquired
                    else
                        item :: model.acquired
            in
            ( { model | acquired = acquired_ }, Cmd.none )

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

        SearchTermChanged searchTerm ->
            ( { model | searchTerm = searchTerm }, Cmd.none )

        ShowVaultedChanged showVaulted ->
            ( { model | showVaulted = showVaulted }, Cmd.none )


view : Model -> Html Msg
view { relics, searchTerm, showVaulted } =
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
            div [] (List.map relicView filteredRelics)

        relicView =
            \{ era, name, items, vaulted } ->
                let
                    vaultedText =
                        if vaulted then
                            " (Vaulted)"
                        else
                            ""
                in
                div []
                    [ h3 [] [ text (toString era ++ " " ++ name ++ vaultedText) ]
                    , ul [] (List.indexedMap itemView (toItemList items))
                    ]

        itemView =
            \index ({ name } as item) ->
                let
                    boldStyle =
                        if searchTerm /= "" && itemStartsWith searchTerm item then
                            [ ( "font-weight", "bold" ) ]
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
                        boldStyle ++ colorStyle
                in
                li [ onClick (ItemClicked item), style styles ] [ text name ]
    in
    div []
        [ searchField
        , toggleVaulted
        , relicList
        ]
