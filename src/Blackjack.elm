module Blackjack exposing (main)

import Browser
import Html exposing (Html, button, div, h2, hr, text)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.List exposing (shuffle)


type Msg
    = NoOp
    | NewGame
    | NewDeck (List Card)
    | Step


type GameState
    = NotPlaying
    | InitialDeal
    | PlayerDraw
    | DealerDraw
    | PlayerWon
    | DealerWon


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



-- Card draws


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
    not (isBust player)
        && not (isBust dealer)
        && score dealerHand
        < score playerHand



--- Scores


score : Hand -> Int
score hand =
    List.sum (List.map scoreCard hand)


isBust : Player -> Bool
isBust player =
    score player.hand > 21


hasBlackjack : Player -> Bool
hasBlackjack p =
    List.length p.hand == 2 && score p.hand == 21


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
    { deck : Deck, player : Player, dealer : Player, playerWins : Int, dealerWins : Int, state : GameState }


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
    , state = NotPlaying
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
        , div [] [ text "Score: " ]
        , div [] [ text (String.fromInt (score p.hand)) ]
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
        [ div [] [ text "Deck" ]
        , div [] [ viewHand deck ]
        , div [] [ text ("Size of deck: " ++ sizeStr) ]
        ]


gameControl : Model -> Html Msg
gameControl model =
    div []
        [ button [ onClick NewGame ] [ text "Start new game" ]
        , button [ onClick Step ] [ text "Next Action" ]
        , hr [] []
        , div [] [ text (Debug.toString model.state) ]
        , div []
            [ div []
                [ text "Player wins: "
                , text (String.fromInt model.playerWins)
                ]
            , div []
                [ text "Dealer wins: "
                , text (String.fromInt model.dealerWins)
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ playerView model.player
            , hr [] []
            , playerView model.dealer
            , hr [] []
            ]
        , div []
            [ deckView model.deck ]
        , div [] [ gameControl model ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            let
                shuffler =
                    shuffle newDeck
            in
            ( model, Random.generate NewDeck shuffler )

        NewDeck d ->
            ( { model | state = InitialDeal, deck = d } |> deal |> checkBlackjack, Cmd.none )

        Step ->
            ( draw model, Cmd.none )


playerGetCard : Model -> Model
playerGetCard model =
    let
        ( nextCard, restOfDeck ) =
            case model.deck of
                x :: xs ->
                    ( Just x, xs )

                _ ->
                    ( Nothing, [] )

        p =
            model.player

        player_ =
            case nextCard of
                Just c ->
                    { p | hand = c :: p.hand }

                Nothing ->
                    p
    in
    { model | player = player_, deck = restOfDeck }


dealerGetCard : Model -> Model
dealerGetCard model =
    let
        ( nextCard, restOfDeck ) =
            case model.deck of
                x :: xs ->
                    ( Just x, xs )

                _ ->
                    ( Nothing, [] )

        d =
            model.dealer

        dealer_ =
            case nextCard of
                Just c ->
                    { d | hand = d.hand ++ [ c ] }

                Nothing ->
                    d
    in
    { model | dealer = dealer_, deck = restOfDeck }


draw : Model -> Model
draw model =
    case model.state of
        PlayerDraw ->
            if playerWantsCard model then
                playerGetCard model

            else
                { model | state = DealerDraw }

        DealerDraw ->
            if dealerWantsCard model then
                dealerGetCard model

            else
                { model | state = decideWinner model }

        PlayerWon ->
            { model | state = NotPlaying, playerWins = model.playerWins + 1 }

        DealerWon ->
            { model | state = NotPlaying, dealerWins = model.dealerWins + 1 }

        _ ->
            { model | state = NotPlaying }


decideWinner : Model -> GameState
decideWinner model =
    let
        p =
            model.player

        d =
            model.dealer

        state_ =
            if isBust p then
                DealerWon

            else if isBust d then
                PlayerWon

            else
                DealerWon
    in
    state_


deal : Model -> Model
deal model =
    let
        ( playerHand, dealerHand, rest ) =
            case model.deck of
                p1 :: d1 :: p2 :: d2 :: xs ->
                    ( [ p1, p2 ], [ d1, d2 ], xs )

                _ ->
                    ( [], [], model.deck )

        p =
            model.player

        d =
            model.dealer

        updatedPlayer =
            { p | hand = playerHand }

        updatedDealer =
            { d | hand = dealerHand }
    in
    { model | player = updatedPlayer, dealer = updatedDealer, state = NotPlaying, deck = rest }


checkBlackjack : Model -> Model
checkBlackjack model =
    let
        playerHasBlack =
            hasBlackjack model.player

        dealerHasBlack =
            hasBlackjack model.dealer

        state_ =
            if playerHasBlack then
                PlayerWon

            else if dealerHasBlack then
                DealerWon

            else
                PlayerDraw
    in
    { model | state = state_ }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }
