{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
    "stand <game> <player> - move onto guest player or end the game",
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
                                    "stand" -> Command {command=Stand, name=nth 2 parts, player=nth 3 parts}
                                    "show" -> Command {command=Show, name=nth 2 parts, player=Nothing}
                                    _ -> Command {command = Invalid, name=Nothing, player=Nothing}
    
runCommand :: Command -> IO ()
runCommand c = case (command c) of 
                    New -> newGame c
                    Delete -> deleteGame c
                    Join -> joinGame c
                    Hit -> hit c
                    Stand -> stand c
                    Show -> showGame c
                    Help -> putStr $ unlines helpTxt
                    _ -> putStrLn "Invalid command, type help for available commands."
                    
                    
