module Deck where

import Data.List (sort, intercalate)
import System.Random as Random
import System.Random.Shuffle (shuffle')

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

shuffleDeck :: [(CardType, CardSuit)] -> IO [(CardType, CardSuit)]
shuffleDeck cards = do 
            rng <- newStdGen
            let s = shuffle' cards (length cards) rng
            rng <- newStdGen
            return $ shuffle' s (length s) rng
        

remainingDeck :: [(CardType, CardSuit)] -> [(CardType, CardSuit)] -> [(CardType, CardSuit)]
remainingDeck _ [] = []
remainingDeck [] (y:ys) = y:ys
remainingDeck (x:xs) (y:ys) = remainingDeck xs (removeCard x (y:ys))

removeCard :: (CardType, CardSuit) -> [(CardType, CardSuit)] -> [(CardType, CardSuit)]
removeCard _ [] = []
removeCard x (y:ys) | x == y = ys 
                    | otherwise = y : removeCard x ys
                    
drawCard :: [(CardType, CardSuit)] -> IO (CardType, CardSuit)
drawCard drawn = do deck <- shuffleDeck (remainingDeck drawn (nDeck 4))
                    return $ head deck


