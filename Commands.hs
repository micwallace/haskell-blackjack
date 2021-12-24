module Commands where

import System.Directory
import Core

data CommandType = Help | New | Delete | Join | Hit | Stand | Show | Invalid deriving (Show)
    
data Command = Command {command :: CommandType, 
                        name :: Maybe String, 
                        player :: Maybe String} deriving (Show)

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
        
stand :: Command -> IO ()
stand (Command{name=Nothing}) = putStrLn "Please specify game name!"
stand (Command{player=Nothing}) = putStrLn "Please specify player name!"
stand (Command{name=Just n, player=Just p}) = do
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
