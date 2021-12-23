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


data GameState = GameState {gameName :: String, 
                            hostPlayer :: String, 
                            hostHand :: [(String, String)],
                            hostScore :: Integer,
                            guestPlayer :: Maybe String,
                            guestHand :: [(String, String)],
                            guestScore :: Integer } deriving (Show, Generic)
                            
instance ToJSON GameState
instance FromJSON GameState
    

initialGameState :: String -> String -> GameState
initialGameState n p = GameState {gameName=n, hostPlayer=p, hostHand=[], hostScore=0, guestPlayer=Nothing, guestHand=[], guestScore=0}

setGuestPlayer :: GameState -> String -> GameState
setGuestPlayer g@(GameState {gameName=gn, hostPlayer=hp, hostHand=hh, hostScore=hs, 
                                guestPlayer=gp, guestHand=gh, guestScore=gs}) p = g { guestPlayer=Just p }

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






