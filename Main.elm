module Main exposing (..)

import Data exposing (..)
import Html exposing (Html, div, h3, input, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Http
import String exposing (startsWith)


type alias Model =
    { relics : List Relic
    , acquired : List Item
    , searchTerm : String
    }


type Msg
    = RelicDataResponse (Result Http.Error (List Relic))
    | ItemClicked Item
    | SearchTermChanged String


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


view : Model -> Html Msg
view { relics, searchTerm } =
    let
        filteredRelics =
            List.filter (containsItemWith searchTerm) relics

        searchField =
            div []
                [ text "Filter: "
                , input [ onInput SearchTermChanged ] [ text searchTerm ]
                ]

        relicList =
            div [] (List.map relicView filteredRelics)

        relicView =
            \{ era, name, items } ->
                div []
                    [ h3 [] [ text (toString era ++ " " ++ name) ]
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
        , relicList
        ]


itemView : Item -> Html Msg
itemView ({ name } as item) =
    li [ onClick (ItemClicked item) ] [ text name ]
