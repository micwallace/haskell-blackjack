{-# LANGUAGE DeriveGeneric #-}

module Deck where

import System.Random
import System.Random.Shuffle (shuffle')
import GHC.Generics

data CardType =
  Two | Three | Four | Five |
  Six | Seven | Eight | Nine |
  Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Generic)
  
data CardSuit =
  Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum, Generic)
  
  
cardScore :: CardType -> Int
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
cardScore Ace = 11


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


remainingDeck :: [(CardType, CardSuit)] -> [(CardType, CardSuit)] -> [(CardType, CardSuit)]
remainingDeck _ [] = []
remainingDeck [] (y:ys) = y:ys
remainingDeck (x:xs) (y:ys) = remainingDeck xs (removeCard x (y:ys))


removeCard :: (CardType, CardSuit) -> [(CardType, CardSuit)] -> [(CardType, CardSuit)]
removeCard _ [] = []
removeCard x (y:ys) | x == y = ys 
                    | otherwise = y : removeCard x ys
      
      
shuffleDeck :: [(CardType, CardSuit)] -> IO [(CardType, CardSuit)]
shuffleDeck cards = do 
            rng <- newStdGen
            let s = shuffle' cards (length cards) rng
            rng <- newStdGen
            return $ shuffle' s (length s) rng
        
  
drawCard :: [(CardType, CardSuit)] -> IO (CardType, CardSuit)
drawCard drawn = do deck <- shuffleDeck (remainingDeck drawn (nDeck 4))
                    return $ head deck
                    
testDrawCard :: [(CardType, CardSuit)] -> IO (CardType, CardSuit)
testDrawCard drawn = do return (Ace, Spades)


