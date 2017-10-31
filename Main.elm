module Main exposing (..)

import Data exposing (..)
import Html exposing (Html, div, h3, li, text, ul)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { relics : List Relic
    , acquired : List Item
    }


type Msg
    = RelicDataResponse (Result Http.Error (List Relic))
    | ItemClicked Item


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, (fetchRelicData RelicDataResponse) )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


model : Model
model =
    { relics = []
    , acquired = []
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


view : Model -> Html Msg
view { relics } =
    div [] (List.map relicView relics)


relicView : Relic -> Html Msg
relicView ({ era, name } as relic) =
    div []
        [ h3 [] [ text (toString era ++ " " ++ name) ]
        , ul [] (dropViews relic)
        ]


dropViews : Relic -> List (Html Msg)
dropViews { c1, c2, c3, u1, u2, r } =
    List.map dropView [ c1, c2, c3, u1, u2, r ]


dropView : Maybe Item -> Html Msg
dropView item =
    case item of
        Nothing ->
            li [] [ text "Item information could not be extracted, check your data" ]

        Just ({ name } as i) ->
            li [ onClick (ItemClicked i) ] [ text name ]
