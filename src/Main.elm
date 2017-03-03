module Main exposing (..)

import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (class, placeholder, style)
import List
import Debug


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type StateCard
    = Back
    | Front
    | Found


type StateGame
    = Init
    | Playing


type alias Card =
    { id : String, value : String, state : StateCard }


type alias Model =
    { counter : Int
    , cards : List Card
    , player : String
    , state : StateGame
    }


model : Model
model =
    Model 0 [] "" Init



-- UPDATE


type Msg
    = UpdatePlayer String
    | ShuffleCards
    | FlipCard Card
    | RestartGame


update : Msg -> Model -> Model
update msg model =
    case msg of
        RestartGame ->
            Model 0 [] "" Init

        UpdatePlayer newPlayer ->
            { model | player = newPlayer }

        -- TODO: Implement with random
        ShuffleCards ->
            { model
                | cards =
                    List.map (\( id, value ) -> (Card id value Back))
                        [ ( "1", "A" ), ( "2", "C" ), ( "3", "B" ), ( "4", "A" ), ( "5", "B" ), ( "6", "C" ) ]
                , state = Playing
            }

        --- TODO: remove unneeded parenthesis in favor of the infix operator
        --- TODO: Move cards logic into its own module
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
            div [ onClick (FlipCard card), backCardStyle ] [ text "♦" ]

        _ ->
            div [ onClick (FlipCard card), frontCardStyle ] [ text card.value ]


bodyStyle : Html.Attribute msg
bodyStyle =
    style
        [ ( "backgroundColor", "lightgrey" )
        , ( "height", "100%" )
        , ( "width", "100%" )
        , ( "borderSpacing", "10px" )
        ]


backCardStyle : Html.Attribute msg
backCardStyle =
    style
        [ ( "backgroundColor", "salmon" )
        , ( "borderRadius", "5px" )
        , ( "boxShadow", "0 4px 8px 0 rgba(27, 27, 27, 0.21)" )
        , ( "color", "beige" )
        , ( "display", "table-cell" )
        , ( "height", "200px" )
        , ( "font-size", "50px" )
        , ( "textAlign", "center" )
        , ( "transition", "0.3s" )
        , ( "verticalAlign", "middle" )
        , ( "width", "150px" )
        ]


frontCardStyle : Html.Attribute msg
frontCardStyle =
    style
        [ ( "backgroundColor", "salmon" )
        , ( "borderRadius", "5px" )
        , ( "boxShadow", "0 4px 8px 0 rgba(27, 27, 27, 0.21)" )
        , ( "color", "beige" )
        , ( "display", "table-cell" )
        , ( "height", "200px" )
        , ( "font-size", "70px" )
        , ( "textAlign", "center" )
        , ( "transition", "0.3s" )
        , ( "verticalAlign", "middle" )
        , ( "width", "150px" )
        ]


divStyle : Html.Attribute msg
divStyle =
    style
        [ ( "display", "block" )
        , ( "paddingLeft", "30px" )
        , ( "paddingTop", "30px" )
        , ( "width", "100%" )
        ]


inputStyle : Html.Attribute msg
inputStyle =
    style
        [ ( "fontSize", "15px" )
        , ( "marginRight", "10px" )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            div [ bodyStyle ]
                [ div [ divStyle ]
                    [ input [ placeholder "Enter your name", onInput UpdatePlayer, inputStyle ] []
                    , button [ onClick ShuffleCards, inputStyle ] [ text "Shuffle cards" ]
                    ]
                ]

        Playing ->
            div [ bodyStyle ]
                [ div [ divStyle ]
                    [ div [] [ model.player |> String.append "Player: " |> text ]
                    , div [] [ model.counter |> toString |> String.append "Count: " |> text ]
                    ]
                , div [ divStyle ] (List.map drawCard model.cards)
                , div [ divStyle ] [ button [ onClick RestartGame, inputStyle ] [ text "RestartGame" ] ]
                ]
