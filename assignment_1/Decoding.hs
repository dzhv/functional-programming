module Decoding
where

import qualified Data.Char

msg :: String
msg = "d1:ad1:v1:x1:xi2e1:yi2ee1:bd1:v1:o1:xi2e1:yi0eee"
smap = "d1:v1:x1:xi2e1:yi2ee"
msg2 = "d1:v1:x1:xi2e1:yi2ee1:bd1:v1:o1:xi2e1:yi0ee"

type StringMap = [(String, String)]

data Move = Move {
	  x :: Int
	, y :: Int
	, value :: Char
} deriving (Eq, Show, Ord)


decodeMessage :: String -> [Move]
decodeMessage string = 
	let
		nakedDictionary = removeDictionaryBrackets string
		result = readExternalDictionaryContent nakedDictionary []
	in result
	
readExternalDictionaryContent :: String -> [Move] -> [Move]
readExternalDictionaryContent [] acc = acc
readExternalDictionaryContent string acc =
	let
		(key, left) = takeKeyName string
		(internalDictionary, left') = takeInternalDictionary left
		move = decodeSingleMoveDictionary internalDictionary		
	in readExternalDictionaryContent left' (move : acc)
		
takeInternalDictionary :: String -> (String, String)
takeInternalDictionary string = takeUntilEndOfDictionary string ""

takeUntilEndOfDictionary :: String -> String -> (String, String)
takeUntilEndOfDictionary string [] = takeUntilEndOfDictionary (drop 1 string) (take 1 string)
takeUntilEndOfDictionary string acc = 
	let 
		result = if (not (isANumber(head (reverse acc))) && ((head string) == 'e'))
					then (concat [acc, "e"], (drop 1 string)) 
					else takeUntilEndOfDictionary (drop 1 string) (concat [acc, take 1 string])
	in result
	

removeDictionaryBrackets :: String -> String
removeDictionaryBrackets ('d' : rest) = reverse (drop 1 (reverse rest))
removeDictionaryBrackets _ = error "expected 'd' at the start and 'e' at the end of the string"

isANumber :: Char -> Bool
isANumber c = Data.Char.ord c >= 48 && Data.Char.ord c <= 57

takeKeyName :: String -> (String, String)     -- String starting with key length(Key, RestOfString)
takeKeyName string = 
	let
		keyLengthString = takeWhile isANumber string 
		keyLength = read keyLengthString  :: Int
		key = take keyLength (drop (length keyLengthString + 1) string)
		rest = drop (length keyLengthString + 1 + keyLength) string
	in (key, rest)
	
takeStringValue :: String -> (String, String)
takeStringValue string = takeKeyName string

takeNumberValue :: String -> (String, String)
takeNumberValue string = 
	let
		value = take 1 (drop 1 string)
		left =  drop 3 string
	in (value, left)
	
decodeSingleMoveDictionary :: String -> Move
decodeSingleMoveDictionary string =
	let
		nakedDictionary = removeDictionaryBrackets string
		result = formASingleDictionaryFromString nakedDictionary []
		x = case lookup "x" result of
			Just a -> read a :: Int
			Nothing -> error "move message was somehow wrong"
		y = case lookup "y" result of
			Just a -> read a :: Int
			Nothing -> error "move message was somehow wrong"
		moveValue = case lookup "v" result of
			Just a -> readMoveValue a
			Nothing -> error "move message was somehow wrong"
			
	in Move x y moveValue
	
readMoveValue :: String -> Char
readMoveValue ("x") = 'x'
readMoveValue ("X") = 'x'
readMoveValue ("0") = 'o'
readMoveValue ("o") = 'o'
readMoveValue ("O") = 'o'
	
formASingleDictionaryFromString :: String -> StringMap -> StringMap
formASingleDictionaryFromString [] acc = acc
formASingleDictionaryFromString string acc = 
	let
		(key, stringWithoutKey) = takeKeyName string
		(val, left) = if (key == "v") then takeStringValue stringWithoutKey
			else takeNumberValue stringWithoutKey		
	in formASingleDictionaryFromString left ((key,val) : acc)
