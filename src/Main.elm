module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder)
import List


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Card =
    { id : String
    , value : String
    , flipped : Bool
    }


type alias Model =
    { player : String
    , counter : Int
    , cards : List Card
    }


model : Model
model =
    Model "" 0 []



-- UPDATE


type Msg
    = UpdatePlayer String
    | ShuffleCards
    | FlipCard Card


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatePlayer newPlayer ->
            { model | player = newPlayer }

        ShuffleCards ->
            { model
                | cards =
                    List.map (\( id, value ) -> (Card id value False))
                        [ ( "1", "A" ), ( "2", "C" ), ( "3", "B" ), ( "4", "A" ), ( "5", "B" ), ( "6", "C" ) ]
            }

        FlipCard selectedCard ->
            let
                updateCard card =
                    if card.id == selectedCard.id then
                        { card | flipped = not card.flipped }
                    else
                        card
            in
                { model | cards = (List.map updateCard model.cards) }


drawCard : Card -> Html Msg
drawCard card =
    if card.flipped then
        div [ onClick (FlipCard card) ] [ text card.value ]
    else
        div [ onClick (FlipCard card) ] [ text "card" ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Player name", onInput UpdatePlayer ] []
        , div [] [ text model.player ]
        , button [ onClick ShuffleCards ] [ text "Shuffle cards" ]
        , div [] (List.map drawCard model.cards)
        ]
