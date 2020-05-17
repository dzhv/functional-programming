module Validation

where

import Decoding
import qualified Data.List

someMoves :: [Move]
someMoves = [Move 0 0 'x', Move 0 1 'x', Move 0 2 'x']
someMoves2 = [Move 0 0 'x', Move 1 0 'x', Move 2 0 'x']
someMoves3 = [Move 0 0 'x', Move 1 1 'x', Move 2 2 'x']
someMoves4 = [Move 0 2 'x', Move 1 1 'x', Move 2 0 'x']

validate :: String -> Bool
validate [] = True
validate string = 
	let 
		moves = decodeMessage string		
	in not (existsOverlapping moves)
		&& not (movesExistAfterAWin moves [])
		&& numberOfMovesIsCorrect moves

existsOverlapping :: [Move] -> Bool
existsOverlapping moves = any (moveSharesASpot moves) moves
	
isInTheSameSpot :: Move -> Move -> Bool
isInTheSameSpot move1 move2 = 
	((x move1) == (x move2) && (y move1) == (y move2))
	
moveSharesASpot :: [Move] -> Move -> Bool
moveSharesASpot moves move = any (isInTheSameSpot move) (Data.List.delete move moves)
	
	
movesExistAfterAWin :: [Move] -> [Move] -> Bool
movesExistAfterAWin uncheckedMoves [] = movesExistAfterAWin (drop 1 uncheckedMoves) [(Data.List.head uncheckedMoves)]
movesExistAfterAWin [] movesToCheck = False
movesExistAfterAWin uncheckedMoves movesToCheck = 
	(winnerExists movesToCheck) || (movesExistAfterAWin (drop 1 uncheckedMoves)) (movesToCheck ++ [(Data.List.head uncheckedMoves)])

winnerExists :: [Move] -> Bool
winnerExists moves = any (isAPartOfTheWin moves) moves

isAPartOfTheWin :: [Move] -> Move -> Bool
isAPartOfTheWin moves move = 
	(isInAWinningLine moves move) || (isInAWinningColumn moves move) || (isInAWinningDiagonal moves move)

isInAWinningLine :: [Move] -> Move -> Bool
isInAWinningLine moves move = movesHaveTheSameValue move (siblingLineMoves moves move)
		
siblingLineMoves :: [Move] -> Move -> (Maybe Move, Maybe Move)
siblingLineMoves moves move = 
	let
		yCoordinate = y move
		(siblingY1, siblingY2) = siblingCoordinates yCoordinate		
	in (findMove moves (x move) siblingY1, findMove moves (x move) siblingY2)


isInAWinningColumn :: [Move] -> Move -> Bool
isInAWinningColumn moves move = movesHaveTheSameValue move (siblingColumnMoves moves move)
	
siblingColumnMoves :: [Move] -> Move -> (Maybe Move, Maybe Move)
siblingColumnMoves moves move = 
	let
		xCoordinate = x move
		(siblingX1, siblingX2) = siblingCoordinates xCoordinate		
	in (findMove moves siblingX1 (y move), findMove moves siblingX2 (y move))


isInAWinningDiagonal :: [Move] -> Move -> Bool
isInAWinningDiagonal moves move = 
	movesHaveTheSameValue move (sibling1DiagonalMoves moves move) || movesHaveTheSameValue move (sibling2DiagonalMoves moves move) 
	
sibling1DiagonalMoves :: [Move] -> Move -> (Maybe Move, Maybe Move)
sibling1DiagonalMoves moves move = case (x move, y move) of
	(0, 0) -> (findMove moves 1 1, findMove moves 2 2)
	(1, 1) -> (findMove moves 0 0, findMove moves 2 2)
	(2, 2) -> (findMove moves 0 0, findMove moves 1 1)
	(a, b) -> (Nothing, Nothing)

sibling2DiagonalMoves :: [Move] -> Move -> (Maybe Move, Maybe Move)
sibling2DiagonalMoves moves move = case (x move, y move) of
	(0, 2) -> (findMove moves 1 1, findMove moves 2 0)
	(1, 1) -> (findMove moves 0 2, findMove moves 2 0)
	(2, 0) -> (findMove moves 0 2, findMove moves 1 1)
	(a, b) -> (Nothing, Nothing)

	
movesHaveTheSameValue :: Move -> (Maybe Move, Maybe Move) -> Bool
movesHaveTheSameValue move1 (Nothing, Nothing) = False
movesHaveTheSameValue move1 (Just move2, Nothing) = False
movesHaveTheSameValue move1 (Nothing, Just move3) = False
movesHaveTheSameValue move1 (Just move2, Just move3) = value move1 == value move2 && value move1 == value move3
	
siblingCoordinates :: Int -> (Int, Int)
siblingCoordinates 0 = (1, 2)
siblingCoordinates 1 = (0, 2)
siblingCoordinates 2 = (0, 1)
siblingCoordinates _ = error "probably not a coordinate was given"
	
findMove :: [Move] -> Int -> Int -> Maybe Move
findMove moves cordX cordY = Data.List.find (coordinatesAreEqual cordX cordY) moves

coordinatesAreEqual :: Int -> Int -> Move -> Bool
coordinatesAreEqual xCord yCord move = (x move == xCord) && (y move == yCord) 


numberOfMovesIsCorrect :: [Move] -> Bool
numberOfMovesIsCorrect moves = 
	let 
		xMoves = countMovesByValue moves 'x'
		oMoves = countMovesByValue moves 'o'
	in ((xMoves - oMoves) == 0) ||  ((xMoves - oMoves) == 1)

countMovesByValue :: [Move] -> Char -> Int
countMovesByValue moves val = length $ filter (isOfValue val) moves 

isOfValue :: Char -> Move -> Bool
isOfValue val move = value move == val
-- ar nera > 2 laimetoju		uzteks pazet, ar 
-- ejimu skaicius
-- ar nera toje pacioje vietoje
-- 


