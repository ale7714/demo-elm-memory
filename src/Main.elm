module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder)
import List
import Debug


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type StateCard
    = Back
    | Front
    | Found


type alias Card =
    { id : String, value : String, state : StateCard }


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
                    List.map (\( id, value ) -> (Card id value Back))
                        [ ( "1", "A" ), ( "2", "C" ), ( "3", "B" ), ( "4", "A" ), ( "5", "B" ), ( "6", "C" ) ]
            }

        FlipCard selectedCard ->
            case selectedCard.state of
                Back ->
                    let
                        updateStateCard card =
                            case card.state of
                                Back ->
                                    if card == selectedCard then
                                        { card | state = Front }
                                    else
                                        card

                                Front ->
                                    if card.value /= selectedCard.value then
                                        { card | state = Back }
                                    else
                                        card

                                Found ->
                                    card

                        flipCards =
                            List.map updateStateCard model.cards

                        getGuesses cards =
                            List.filter (\card -> (isAGuess card selectedCard)) cards

                        foundCards =
                            getGuesses flipCards

                        markAsFound card =
                            case card.state of
                                Front ->
                                    if List.member card foundCards then
                                        { card | state = Found }
                                    else
                                        card

                                _ ->
                                    card

                        updateCards =
                            List.map markAsFound flipCards
                    in
                        case List.length foundCards of
                            2 ->
                                { model | cards = updateCards, counter = model.counter + 1 }

                            _ ->
                                { model | cards = flipCards }

                Front ->
                    let
                        flipBackCard card =
                            case card.state of
                                Front ->
                                    if card == selectedCard then
                                        { card | state = Back }
                                    else
                                        card

                                _ ->
                                    card
                    in
                        { model | cards = (List.map flipBackCard model.cards) }

                Found ->
                    model


isAGuess : Card -> Card -> Bool
isAGuess other_card guess =
    case other_card.state of
        Front ->
            if other_card.value == guess.value then
                True
            else
                False

        _ ->
            False


drawCard : Card -> Html Msg
drawCard card =
    case card.state of
        Back ->
            div [ onClick (FlipCard card) ] [ text "card" ]

        _ ->
            div [ onClick (FlipCard card) ] [ text card.value ]



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
