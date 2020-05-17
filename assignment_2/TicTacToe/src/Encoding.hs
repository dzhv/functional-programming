module Encoding
where

import Decoding

summsg = "d1:ad1:v1:x1:xi2e1:yi2ee1:bd1:v1:o1:xi2e1:yi0eee"

encodeMoveToDictionary :: Move -> String
encodeMoveToDictionary move =
	concat ["d1:v1:", [(value move)], "1:xi", show ((x move)), "e1:yi", show ((y move)), "e"]

encodeMoveArrayToDictionary :: [Move] -> String
encodeMoveArrayToDictionary moves = 
	concat ["d", encodeMoveArrayRecursively moves 0 "", "e"] 

encodeMoveArrayRecursively :: [Move] -> Int -> String -> String
encodeMoveArrayRecursively [] counter acc = acc
encodeMoveArrayRecursively moves counter acc = 
	let
		str = concat [acc, "1:", show counter, encodeMoveToDictionary (head moves), "e"]
	in (encodeMoveArrayRecursively (drop 1 moves) (counter + 1) str)