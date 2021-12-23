module Blackjack.Deck where

import Control.Monad
import Control.Monad.Random
import Data.List (sort, intercalate)
import qualified System.Random as Rand
import System.Random.Shuffle (shuffleM)

data CardType =
  Two | Three | Four | Five |
  Six | Seven | Eight | Nine |
  Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum)
  
data CardSuit =
  Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum)
  
  
cardScore :: CardType -> Word
cardScore Two = 2
cardScore Three = 3
cardScore Four = 4
cardScore Five = 5
cardScore Six = 6
cardScore Seven = 7
cardScore Eight = 8
cardScore Nine = 9
cardScore Ten = 10
cardScore Jack = 10
cardScore Queen = 10
cardScore King = 10
cardScore Ace = 1

cardDeck :: [(CardType, CardSuit)]
cardDeck = [(x, y) | 
                x <- [ Two, Three, Four, Five
                    , Six, Seven, Eight, Nine
                    , Ten, Jack, Queen, King, Ace], 
               y <- [Hearts , Diamonds , Clubs , Spades]
            ]
            
nDeck :: Int -> [(CardType, CardSuit)]
nDeck 1 = cardDeck
nDeck n = if (n < 1) then [] else concat $ replicate n cardDeck

shuffledDeck :: ([(CardType, CardSuit)], Rand.StdGen)
shuffledDeck = (runRand (shuffleM (nDeck 4)) (Rand.mkStdGen 1000))
                     





