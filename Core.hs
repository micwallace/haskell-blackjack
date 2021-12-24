{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Core where

import System.IO
import System.Directory
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Deck

data GameStatus = Waiting | Playing | Finished deriving (Show, Generic, Eq)

data PlayerTurn = Host | Guest deriving (Show, Generic, Eq)

data GameState = GameState {gameName :: String, 
                            hostPlayer :: String, 
                            hostHand :: [(CardType, CardSuit)],
                            hostScore :: Int,
                            guestPlayer :: Maybe String,
                            guestHand :: [(CardType, CardSuit)],
                            guestScore :: Int, 
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
                            
updateState :: GameState -> Int -> [(CardType, CardSuit)] -> GameState
updateState g@(GameState {hostHand=hh, guestHand=gh, turn=t}) s c 
                                    | ( t == Host) = g {hostScore = s, hostHand = c ++ hh, status=ns} 
                                    | otherwise = g {guestScore = s, guestHand = c ++ gh, status=ns}
                                    where ns = if s > 21 then Finished else Playing
                                    
guestTurn :: GameState -> GameState
guestTurn g@(GameState {turn=t}) = g { turn=Guest } 

gameFinished :: GameState -> GameState
gameFinished g@(GameState {status=s}) = g { status=Finished }
                            
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
processDrawnCard gs@(GameState {hostHand=hh, guestHand=gh, turn=t}) card =  do 
                                let {score = calculateScore (if t == Host then card:hh else card:gh)}
                                ngs <- writeGameState (updateState gs score [card])
                                putStrLn $ show ngs
                                putStrLn ""
                                printScore ngs
                                putStrLn ""
                                printStatus ngs
                                
calculateScore :: [(CardType, CardSuit)] -> Int
calculateScore c | s > 21 = reduceAces (countAces c) s
                 | otherwise = s
                    where s = calcScoreNum c
                   
calcScoreNum :: [(CardType, CardSuit)] -> Int
calcScoreNum [] = 0
calcScoreNum cs = sum $ map (\a -> cardScore (fst a)) cs
                    
countAces :: [(CardType, CardSuit)] -> Int
countAces cards = length [c | c <- cards, (fst c) == Ace] 

reduceAces :: Int -> Int -> Int
reduceAces a s | a == 0 = s
               | otherwise = if s - 10 < 21 then s - 10 else reduceAces (a - 1) (s-10)
                            
doStay :: GameState -> String -> IO ()
doStay gs player = do 
                    if (not $ isPlayersTurn gs player) then
                        putStrLn "Not your turn!"
                    else
                        if ((turn gs) == Host) then
                            hostStay gs
                        else
                            guestStay gs
                        
                            
hostStay :: GameState -> IO ()
hostStay gs = do
                ngs <- writeGameState $ guestTurn gs
                writeGameState ngs
                putStrLn $ show ngs
                putStrLn ""
                putStrLn $ (show (hostPlayer ngs)) ++ " has stayed!"
                putStrLn ""
                printScore ngs
                putStrLn ""
                printStatus ngs
            
            
guestStay :: GameState -> IO ()
guestStay gs = do
                ngs <- writeGameState $ gameFinished gs
                writeGameState ngs
                putStrLn $ show ngs
                putStrLn ""
                putStrLn $ "Game completed!"
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
printStatus (GameState {hostPlayer=hp, hostScore=hs, guestPlayer=(Just gp), guestScore=gs, turn=t, status=s}) 
                | hs > 21 = putStrLn $ hp ++ " is bust, " ++ gp ++ " has won the game!!"
                | gs > 21 = putStrLn $ gp ++ " is bust, " ++ hp ++ " has won the game!!"
                | s == Finished && hs > gs = putStrLn $ hp ++ " has won the game"
                | s == Finished && gs > hs = putStrLn $ gp ++ " has won the game"
                | (gs == hs) = putStrLn "The game is a tie"
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






