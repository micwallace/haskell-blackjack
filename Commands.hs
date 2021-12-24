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

data GameStatus = Waiting | Playing | Finished deriving (Show, Generic)

data PlayerTurn = Host | Guest deriving (Show, Generic)

data GameState = GameState {gameName :: String, 
                            hostPlayer :: String, 
                            hostHand :: [(String, String)],
                            hostScore :: Integer,
                            guestPlayer :: Maybe String,
                            guestHand :: [(String, String)],
                            guestScore :: Integer, 
                            turn :: PlayerTurn, 
                            status :: GameStatus} deriving (Show, Generic)
                            
instance ToJSON GameState
instance FromJSON GameState

instance ToJSON GameStatus
instance FromJSON GameStatus

instance ToJSON PlayerTurn
instance FromJSON PlayerTurn
    

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

setGuestPlayer :: GameState -> String -> GameState
setGuestPlayer g@(GameState {gameName=gn, hostPlayer=hp, hostHand=hh, hostScore=hs, 
                                guestPlayer=gp, guestHand=gh, guestScore=gs}) p = g { guestPlayer=Just p, status=Playing }

writeGameState :: GameState -> IO ()
writeGameState game = B.writeFile (gameFilePath $ gameName game) (encode game)
    
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






