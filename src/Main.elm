module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder)
import List
import Debug


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Card =
    { id : String
    , value : String
    , flipped : Bool
    , found : Bool
    }


type alias Model =
    { counter : Int
    , cards : List Card
    , player : String
    }


model : Model
model =
    Model 0 [] ""



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
                    List.map (\( id, value ) -> (Card id value False False))
                        [ ( "1", "A" ), ( "2", "C" ), ( "3", "B" ), ( "4", "A" ), ( "5", "B" ), ( "6", "C" ) ]
            }

        FlipCard selectedCard ->
            let
                flipCard card =
                    if not (card.found) && (card.id == selectedCard.id || (card.flipped && card.value /= selectedCard.value)) then
                        { card | flipped = not card.flipped }
                    else
                        card

                flipCards =
                    List.map flipCard model.cards

                getGuesses cards =
                    List.filter (\card -> (isAGuess card selectedCard)) cards

                foundCards =
                    getGuesses flipCards

                markAsFound card =
                    if List.member card foundCards then
                        { card | found = True }
                    else
                        card

                updateCards =
                    List.map markAsFound flipCards
            in
                case List.length foundCards of
                    2 ->
                        { model | cards = updateCards, counter = model.counter + 1 }

                    _ ->
                        { model | cards = flipCards }


isAGuess : Card -> Card -> Bool
isAGuess card1 card2 =
    card1.value == card2.value && card1.flipped && not (card1.found)


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
        , div [] [ text (toString model.counter) ]
        ]
