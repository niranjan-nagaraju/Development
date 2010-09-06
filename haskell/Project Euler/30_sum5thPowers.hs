import IO
import Char

getDigits :: Int -> (Int, [Int])
getDigits num = 
	(num, digits) where
		digits = map (\x -> ((ord x) - (ord '0'))) (show num)

isCandidate :: Int -> (Int, [Int]) -> Bool
isCandidate order digit =
	(digitsPowerSum == fst(digit)) where
		digitsPowerSum = (sum [(product (take order (repeat x))) | x<- snd(digit)])


getCandidates order =
	[x | x <- [2 .. (startr *10*10)], (isCandidate order (getDigits x))] where
		startr = product (take (order - 1) (repeat (10::Int)))

main = do
	let cand4 = (getCandidates 4)
	print cand4
	print $ sum cand4 
	
	let cand5 = (getCandidates 5)
	print cand5
	print $ sum cand5
