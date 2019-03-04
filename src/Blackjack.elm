module Blackjack exposing (main)

import Browser
import Html exposing (Html, button, div, h2, text)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.List exposing (shuffle)


type Msg
    = NoOp
    | NewGame
    | NewDeck (List Card)


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Diamond
    | Club
    | Heart
    | Spade


type alias Card =
    { suit : Suit, rank : Rank }


type alias Deck =
    List Card


type alias Hand =
    List Card


type alias Player =
    { name : String
    , hand : Hand
    , limit : Maybe Int
    }


playerWantsCard : Model -> Bool
playerWantsCard model =
    let
        player =
            model.player

        playerHand =
            player.hand

        lim =
            case player.limit of
                Just l ->
                    l

                Nothing ->
                    17
    in
    score playerHand < lim


isBust : Player -> Bool
isBust player =
    score player.hand > 21


dealerWantsCard : Model -> Bool
dealerWantsCard model =
    let
        dealer =
            model.dealer

        dealerHand =
            dealer.hand

        player =
            model.player

        playerHand =
            player.hand
    in
    not (isBust player) && not (isBust dealer) && score dealerHand < score playerHand


score : Hand -> Int
score hand =
    List.sum (List.map scoreCard hand)


scoreCard : Card -> Int
scoreCard c =
    case c.rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ace ->
            11

        _ ->
            10


type alias Model =
    { deck : Deck, player : Player, dealer : Player, playerWins : Int, dealerWins : Int }


rankForSuit : Suit -> List Card
rankForSuit suit =
    let
        ranks =
            [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]
    in
    List.map (\r -> Card suit r) ranks


newDeck : Deck
newDeck =
    let
        suits =
            [ Diamond, Heart, Spade, Club ]
    in
    List.concatMap rankForSuit suits


initialModel : Model
initialModel =
    { deck = newDeck
    , player = Player "sam" [] (Just 17)
    , dealer = Player "dealer" [] Nothing
    , playerWins = 0
    , dealerWins = 0
    }


suitAsString : Suit -> String
suitAsString suit =
    case suit of
        Diamond ->
            "D"

        Heart ->
            "H"

        Club ->
            "C"

        Spade ->
            "S"


rankAsString : Rank -> String
rankAsString rank =
    case rank of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Ace ->
            "A"


cardAsString : Card -> String
cardAsString card =
    let
        s =
            suitAsString card.suit

        r =
            rankAsString card.rank
    in
    s ++ r


viewHand : Hand -> Html msg
viewHand hand =
    let
        cards =
            List.map cardAsString hand

        cardString =
            String.join "," cards
    in
    text cardString


playerView : Player -> Html msg
playerView p =
    div []
        [ h2 [] [ text p.name ]
        , div [] [ text "Cards: " ]
        , div [] [ viewHand p.hand ]
        ]


deckView : Deck -> Html msg
deckView deck =
    let
        size =
            List.length deck

        sizeStr =
            String.fromInt size
    in
    div []
        [ div [] [ viewHand deck ]
        , div [] [ text ("Size of deck: " ++ sizeStr) ]
        ]


gameControl : Html Msg
gameControl =
    div [] [ button [ onClick NewGame ] [ text "Start new game" ] ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ playerView model.player
            , playerView model.dealer
            ]
        , div []
            [ deckView model.deck ]
        , div [] [ gameControl ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                shuffler =
                    shuffle model.deck
            in
            ( model, Random.generate NewDeck shuffler )

        NewDeck d ->
            ( { model | deck = d }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
