module Main exposing (..)

import Data exposing (..)
import Html exposing (Html, div, h3, input, li, text, ul)
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
    in
    div []
        [ searchField searchTerm
        , div [] (List.map relicView filteredRelics)
        ]


searchField : String -> Html Msg
searchField searchTerm =
    div []
        [ text "Filter: "
        , input [ onInput SearchTermChanged ] [ text searchTerm ]
        ]


relicView : Relic -> Html Msg
relicView ({ era, name } as relic) =
    div []
        [ h3 [] [ text (toString era ++ " " ++ name) ]
        , ul [] (itemViews relic.items)
        ]


itemViews : ItemCollection -> List (Html Msg)
itemViews ( c1, c2, c3, u1, u2, r ) =
    List.map itemView [ c1, c2, c3, u1, u2, r ]


itemView : Item -> Html Msg
itemView ({ name } as item) =
    li [ onClick (ItemClicked item) ] [ text name ]
