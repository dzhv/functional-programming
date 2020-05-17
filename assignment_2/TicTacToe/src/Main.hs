module Main
where

import System.Environment
import Game

main :: IO()
main = do
	args <- getArgs
	case args of
		[gameId] -> do
			Game.startGame gameId
		_ -> putStrLn "wrong number of arguments. Please provide game id (only)"