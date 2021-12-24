{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Text.Read
import System.IO
import System.Directory
import Data.Monoid ((<>))
import GHC.Generics
import Data.Aeson
import Data.Map
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Commands

main :: IO ()
main = do
    putStrLn "Welcome to blackjack!"
    newlines 1
    putStr $ unlines helpTxt 
    newlines 1
    commandLoop
    
helpTxt :: [String]
helpTxt = [
    "Type command to continue:", 
    "help - view this cheetsheet",
    "new <name> <player> - create a new game",
    "delete <name> - delete game",
    "join <game> <player> - join game",
    "hit <game> <player> - take a card from the deck",
    "stay <game> <player> - move onto guest player or end the game",
    "show <game> - view game data and score"]
            
newlines :: Integer -> IO ()
newlines n = putStr $ unlines ["\n" | _ <- [1..n]]

commandLoop :: IO ()
commandLoop = do
    command <- getLine
    newlines 1
    runCommand $ parseCommand command
    newlines 1
    commandLoop
    
    
data CommandType = Help | New | Delete | Join | Hit | Stand | Show | Invalid deriving (Show)
    
data Command = Command {command :: CommandType, 
                        name :: Maybe String, 
                        player :: Maybe String} deriving (Show)
                    
                    
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 1 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs
                
parseCommand :: String -> Command
parseCommand c = let parts = (words c) in 
                    buildCommandData (nth 1 parts) parts
                
                      
buildCommandData :: Maybe String -> [String] -> Command
buildCommandData (Nothing) parts = Command {command = Invalid, name=Nothing, player=Nothing}
buildCommandData (Just c) parts = case (c) of
                                    "help" -> Command {command=Help, name=Nothing, player=Nothing}
                                    "new" -> Command {command=New, name=nth 2 parts, player=nth 3 parts}
                                    "delete" -> Command {command=Delete, name=nth 2 parts, player=Nothing}
                                    "join" -> Command {command=Join, name=nth 2 parts, player=nth 3 parts}
                                    "hit" -> Command {command=Hit, name=nth 2 parts, player=nth 3 parts}
                                    "stay" -> Command {command=Stand, name=nth 2 parts, player=nth 3 parts}
                                    "show" -> Command {command=Show, name=nth 2 parts, player=Nothing}
                                    _ -> Command {command = Invalid, name=Nothing, player=Nothing}
    
runCommand :: Command -> IO ()
runCommand c = case (command c) of 
                    New -> newGame c
                    Delete -> deleteGame c
                    Join -> joinGame c
                    Hit -> hit c
                    Stand -> stay c
                    Show -> showGame c
                    Help -> putStr $ unlines helpTxt
                    _ -> putStrLn "Invalid command, type help for available commands."
         
newGame :: Command -> IO ()
newGame (Command{name=Nothing}) = putStrLn "The game must have a name!"
newGame (Command{player=Nothing}) = putStrLn "The player must have a name!"
newGame (Command{name=Just n, player=Just p}) = do
    exists <- doesFileExist (gameFilePath n)
    if exists then
        putStrLn "The game save file already exists" 
    else 
        let gs = initialGameState n p in 
            do
            writeGameState gs
            putStrLn $ "Starting new game with name: " ++ n
            putStrLn $ show gs
        
deleteGame :: Command -> IO ()
deleteGame (Command{name=Nothing}) = putStrLn "Please specify game name!"
deleteGame (Command{name=Just n}) = do
    exists <- doesFileExist (gameFilePath n)
    if exists then
        do 
            removeFile (gameFilePath n)
            putStrLn $ "Game " ++ n ++ " deleted!"
    else 
        putStrLn "The game does not exist"

joinGame :: Command -> IO ()
joinGame (Command{name=Nothing}) = putStrLn "Please specify game name!"
joinGame (Command{player=Nothing}) = putStrLn "The player must have a name!"
joinGame (Command{name=Just n, player=Just p}) = do
        mgs <- loadGame n
        let gs (Just a) = a
        if ((guestPlayer $ gs mgs) == Nothing) then
            if (hostPlayer $ gs mgs) == p then
                putStrLn "The host player has the same name, choose a different one"
            else 
                do
                let ngs = setGuestPlayer (gs mgs) p 
                writeGameState ngs
                putStrLn $ "Player " ++ p ++ " has joined game " ++ n
                putStrLn $ show ngs
        else
            do
            putStrLn "The game has already started"
            putStrLn $ show (gs mgs)

hit :: Command -> IO ()
hit (Command{name=Nothing}) = putStrLn "Please specify game name!"
hit (Command{player=Nothing}) = putStrLn "Please specify player name!"
hit (Command{name=Just n, player=Just p}) = do
        mgs <- loadGame n
        let gs (Just a) = a
        if ((guestPlayer $ gs mgs) == Nothing) then
            putStrLn "Waiting for guest player"
        else
            if ((status $ gs mgs) == Finished) then
                putStr "Game has already finished!"
            else
                doHit (gs mgs) p
        
stay :: Command -> IO ()
stay (Command{name=Nothing}) = putStrLn "Please specify game name!"
stay (Command{player=Nothing}) = putStrLn "Please specify player name!"
stay (Command{name=Just n, player=Just p}) = do
        mgs <- loadGame n
        let gs (Just a) = a
        if ((guestPlayer $ gs mgs) == Nothing) then
            putStrLn "Waiting for guest player"
        else
            if ((status $ gs mgs) == Finished) then
                putStr "Game has already finished!"
            else
                doStay (gs mgs) p
            
showGame :: Command -> IO ()
showGame (Command{name=Nothing}) = putStrLn "Please specify game name!"
showGame (Command{name=Just n}) = do
        mgs <- loadGame n
        let gs (Just a) = a
        putStrLn ""
        putStrLn $ show (gs mgs)
        putStrLn ""
        printScore (gs mgs)
        putStrLn ""
        printStatus (gs mgs)

        
        
