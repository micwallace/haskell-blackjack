{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Commands where

import Text.Read
import System.IO
import System.Directory
import Data.Monoid ((<>))
import GHC.Generics
import Data.Aeson
import Data.Map
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Deck

data GameStatus = Waiting | Playing | Finished deriving (Show, Generic, Eq)

data PlayerTurn = Host | Guest deriving (Show, Generic, Eq)

data GameState = GameState {gameName :: String, 
                            hostPlayer :: String, 
                            hostHand :: [(CardType, CardSuit)],
                            hostScore :: Integer,
                            guestPlayer :: Maybe String,
                            guestHand :: [(CardType, CardSuit)],
                            guestScore :: Integer, 
                            turn :: PlayerTurn, 
                            status :: GameStatus} deriving (Show, Generic)
                            
instance ToJSON GameState
instance FromJSON GameState

instance ToJSON GameStatus
instance FromJSON GameStatus

instance ToJSON PlayerTurn
instance FromJSON PlayerTurn

instance ToJSON CardType
instance FromJSON CardType

instance ToJSON CardSuit
instance FromJSON CardSuit
    

initialGameState :: String -> String -> GameState
initialGameState n p = GameState {gameName=n, hostPlayer=p, hostHand=[], hostScore=0, guestPlayer=Nothing, guestHand=[], guestScore=0, turn=Host, status=Waiting}

loadGame :: String -> IO (Maybe GameState)
loadGame n = do
    exists <- doesFileExist (gameFilePath n)
    if exists then
        do
        gs <- readGameState n
        return $ Just gs
    else
        do
        putStrLn "The game does not exist"
        return Nothing
        
hasGuestPlayer :: GameState -> Bool
hasGuestPlayer gs = if (guestPlayer gs) == Nothing then False else True  

-- TODO: Can non-used record fields be removed?
setGuestPlayer :: GameState -> String -> GameState
setGuestPlayer g@(GameState {gameName=gn, hostPlayer=hp, hostHand=hh, hostScore=hs, 
                                guestPlayer=gp, guestHand=gh, guestScore=gs}) p = g { guestPlayer=Just p, status=Playing }

isPlayersTurn :: GameState -> String -> Bool
isPlayersTurn (GameState {hostPlayer=hp, guestPlayer=(Just gp), turn=t}) player = if (t) == Host then
                                                                    hp == player
                                                                else
                                                                    gp == player
                            
updateState :: GameState -> Integer -> [(CardType, CardSuit)] -> GameState
updateState g@(GameState {gameName=gn, hostPlayer=hp, hostHand=hh, hostScore=hs, 
                            guestPlayer=gp, guestHand=gh, guestScore=gs, turn=t}) s c 
                                    | ( t == Host) = g {hostScore = hs + s, hostHand = c ++ hh, turn=Guest} 
                                    | otherwise = g {guestScore = gs + s, guestHand = c ++ gh, turn=Host}
                            
doHit :: GameState -> String -> IO ()
doHit gs player = do 
                    if (not $ isPlayersTurn gs player) then
                        putStrLn "Not your turn!"
                    else
                        let t = turn gs
                            drawn = (hostHand gs) ++ (guestHand gs) in
                        do
                            card <- drawCard drawn
                            putStrLn $ "Card drawn: " ++ (show card) ++ " score: " ++ (show $ cardScore (fst card))
                            processDrawnCard gs card
                            

processDrawnCard :: GameState -> (CardType, CardSuit) -> IO ()
processDrawnCard gs card =  do 
                                let {score = cardScore (fst card)}
                                ngs <- writeGameState (updateState gs score [card])
                                putStrLn $ show ngs
                                putStrLn ""
                                printScore ngs
                                putStrLn ""
                                printStatus ngs
                    
                            
-- TODO: Prevent multiple stands ?
doStand :: GameState -> String -> IO ()
doStand gs player = do 
                    if (not $ isPlayersTurn gs player) then
                        putStrLn "Not your turn!"
                    else
                        do
                            ngs <- writeGameState (updateState gs 0 [])
                            writeGameState ngs
                            putStrLn $ show ngs
                            putStrLn ""
                            putStrLn $ player ++ "stood!"
                            putStrLn ""
                            printScore ngs
                            putStrLn ""
                            printStatus ngs
    
    
printScore :: GameState -> IO ()
printScore (GameState {hostPlayer=hp, hostScore=hs, hostHand=hh, 
                        guestPlayer=(Just gp), guestScore=gs, guestHand=gh, turn=t}) = 
                        do
                            putStrLn $ "--- score ---"
                            putStrLn $ "Host: " ++ hp ++ "\t" ++ (show hs) ++ "\t" ++ (show hh)
                            putStrLn $ "Host: " ++ gp ++ "\t" ++ (show gs) ++ "\t" ++ (show gh)

                            
printStatus :: GameState -> IO ()
printStatus (GameState {hostPlayer=hp, hostScore=hs, guestPlayer=(Just gp), guestScore=gs, turn=t}) 
                | hs > 21 = putStrLn $ hp ++ " is bust, " ++ gp ++ " has won the game!!"
                | gs > 21 = putStrLn $ gp ++ " is bust, " ++ hp ++ " has won the game!!"
                | hs == 21 = putStrLn $ hp ++ " has won the game"
                | gs == 21 = putStrLn $ gp ++ " has won the game"
                | t == Host = putStrLn $ hp ++ "'s turn"
                | t == Guest = putStrLn $ gp ++ "'s turn"
                
                
writeGameState :: GameState -> IO GameState
writeGameState game = do 
                        B.writeFile (gameFilePath $ gameName game) (encode game)
                        return game
    
readGameState :: String -> IO GameState
readGameState name = do
    d <- (eitherDecode <$> (getJSON name)) :: IO (Either String GameState)
    case d of
        Left err -> error err
        Right gs -> return gs
        
getJSON :: String -> IO B.ByteString
getJSON name = B.readFile $ gameFilePath name

gameFilePath :: String -> String
gameFilePath name = "./saved-game-" ++ name ++ ".json"






