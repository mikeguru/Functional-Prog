{-Author: Wen Jiang-}
{-Email: wenjiang@uchicago.edu-}
{-Class: MPCS51400 Winter 2017-}

{-To compile-}
{-ghc Blackjack.hs-}

{-To run-}
{-./Blackjack-}

import Deck
import InitText
import FinalText
import Data.List
import System.Random
import Control.Monad

type CardDeck    = Deck
type PlayerCards = Deck
type DealerCards = Deck
type NumCards    = Int
type NumPlayers  = Int
type Score       = Int

-- main
main :: IO ()
main = do
    header
    --introduce randomeness
    gameState <- getStdGen
    --introduce new randomeness for the next game
    --st there will be new cards drawn from the deck
    gameState <- newStdGen
    gameLoop gameState
    footer
    restartLine <- getLine
    unless (restartLine == "q"||restartLine == "Q") $ do{
        -- restart if not q entered
        main
      }

--begin game loop with random generator
gameLoop :: StdGen -> IO ()
gameLoop gameState = do
    --at the beginning of the game you show the players two cards and the dealer's one card.
    let shuffledDeck                                          = shuffle gameState cardsDeck
        ((playerCards:(dealerCards@(dealerCard:_)):[]), deck) = deal 2 2 shuffledDeck
    putStrLn $ "Dealer's card: " ++ show dealerCard
    (playerCards', deck') <- playerGame playerCards deck
    let dealerCards' = dealerGame dealerCards deck'
        outcome      = gameOutcome playerCards' dealerCards'
    --the dealer keep drawing for card until hit isDealerStand ie 17<=points<=21
    putStrLn $ "Dealer's cards: " ++ showCards dealerCards'
    putStrLn outcome

gameOutcome :: PlayerCards -> DealerCards -> String
gameOutcome playerCards dealerCards
    --check for status
    | not playerValidScore || playerScore < dealerScore = "You lost. Wish you good luck next time~"
    | draw                                              = "Drawn! Have a good day!"
    | playerBlackjack                                   = "You got Blackjacked! Have a good day!"
    | playerScore > dealerScore                         = "You won! Have a good day!"
    where playerValidScore = isValidScore playerCards
          playerBlackjack  = isBlackjack playerCards
          dealerBlackjack  = isBlackjack dealerCards
          playerScore      = highestValidScore playerCards
          dealerScore      = highestValidScore dealerCards
          draw         = (playerBlackjack && dealerBlackjack) ||
                                (not playerBlackjack && not dealerBlackjack &&
                                   playerScore == dealerScore && playerValidScore)

playerGame :: PlayerCards -> CardDeck -> IO (Deck, Deck)
playerGame playerCards deck = do
    --if 21 or Blackjack just return
    putStrLn $ "Your cards: " ++ showCards playerCards
    if isBlackjack playerCards || is21 playerCards then
        return (playerCards, deck)
    else
        --else keep asking for user's input
        if isValidScore playerCards then do
            putStrLn "What would you like to do?"
            putStrLn "(1) Hit"
            putStrLn "(2) Stay"
            optStr <- getLine
            --(1) Hit then draw new card from the deck
            if optStr == "1" || optStr == "(1)"|| optStr == "Hit" || optStr == "(1) Hit"  then
                let (card:[], deck') = deal 1 1 deck
                in playerGame (card ++ playerCards) deck'
            else
                return (playerCards, deck)
        else return (playerCards, deck)

--joint the cards ie like in an array
showCards :: Deck -> String
showCards cards = intercalate ", " (map show cards)

--check for signals according to the BlackJack rule
dealerGame :: DealerCards -> CardDeck -> DealerCards
dealerGame dealerCards (card:cards) =
    let shouldStand = isDealerStand dealerCards
        validScore = isValidScore dealerCards
    in  if shouldStand || not validScore then dealerCards
        else dealerGame (card:dealerCards) cards

--check for signals according to the BlackJack rule
highestValidScore :: Deck -> Score
highestValidScore cards = maximum . filter (\x -> x <= 21) $ (0 : score cards)

isValidScore :: Deck -> Bool
isValidScore cards = (==) 1 $ length . find (\x -> x <= 21) $ score cards

isBlackjack :: Deck -> Bool
isBlackjack cards = length cards == 2 && is21 cards

is21 :: Deck -> Bool
is21 cards = (==) 1 $ length . find (\x -> x == 21) $ score cards

isDealerStand :: DealerCards -> Bool
isDealerStand cards = (==) 1 $ length $ find (\x -> 17<=x) $ filter (\x -> x <= 21 ) $ score cards

--compute card score
score :: Deck -> [Score]
score cards = let cardScores = foldl (\acc card -> (cardScore card):acc) [] cards
              in nub $ sum <$> sequence cardScores

--compute card score
cardScore :: Card -> [Score]
cardScore (NumCard _ value) =
    case value of
        Ace -> [1, 11]
        x   -> [(fromEnum x) + 1]
cardScore (FaceCard _ value) = [10]

--draw one card from deck
deal :: NumCards -> NumPlayers -> CardDeck -> ([PlayerCards], CardDeck)
deal numCards numPlayers cards = deal' numCards (map (\_ -> []) [1..numPlayers], cards)

--remove one from deck
deal' :: NumCards -> ([PlayerCards], CardDeck) -> ([PlayerCards], CardDeck)
deal' 0 (xxs, cards) = (xxs, cards)
deal' _ ([], cards) = ([], cards)
deal' numCards (xxs, cards) =
  let
      (xs, cards') = splitAt (length xxs) cards
      xxs' = zipWith (\x xs -> x:xs) xs xxs
  in
      deal' (numCards-1) (xxs', cards')
