{-Author: Wen Jiang-}
{-Email: wenjiang@uchicago.edu-}
{-Class: MPCS51400 Winter 2017-}

module Deck (Suit(..),
             Number(..),
             Face(..),
             Card(..),
             Deck,
             suits,
             faceCards,
             numCards,
             cardsDeck,
             shuffle) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import System.Random
import Data.List

--define variables
data Suit = Club | Diamond | Heart | Spade
            deriving (Eq, Ord, Read, Bounded, Enum)

data Number = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Face = Jack | Queen | King
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Card = NumCard Suit Number
          | FaceCard Suit Face
            deriving (Eq)

instance Show Card where
    show (NumCard suit pip) = (show pip) ++" "++ show(suit)
    show (FaceCard suit face) = (show face) ++" "++  show(suit)

instance Show Suit where
    show Spade = "♠"
    show Heart = "♥"
    show Diamond = "♦"
    show Club = "♣"

type Deck = [Card]

--create the card struct
suits :: [Suit]
suits = [(minBound :: Suit) ..]

--create the card struct
faceCards :: [Card]
faceCards = FaceCard <$> suits <*> [(minBound :: Face) ..]

--create the card struct
numCards :: [Card]
numCards = NumCard <$> suits <*> [(minBound :: Number) ..]

--create the card struct
cardsDeck :: [Card]
cardsDeck = faceCards ++ numCards

--shuffle all generated cards
shuffle :: (Eq a) => StdGen -> [a] -> [a]
shuffle _ [element] = [element]
shuffle gen elements =
        let (r, newGen) = random gen :: (Int, StdGen)
            element        = elements !! (mod r $ length elements)
            elements'      = delete element elements
        in element : shuffle newGen elements'
