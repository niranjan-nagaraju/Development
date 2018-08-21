import IO

fact :: Integer -> Integer
fact 0 = 0
fact n = product [1..n]

factorials :: [Integer]
factorials = map (\x -> fact x) [0..100]

getCandidate :: Int-> Int-> Integer
getCandidate n r = 
	let nfact = factorials !!  n
	in
	if nfact < 1000000
		then 0
		else 
			let ndivr = nfact `div` (factorials !! r)
			in
			if ndivr < 1000000
				then 0
				else
				let ndivnr = ndivr `div` (factorials !! (n-r))
				in
				if ndivnr < 1000000
					then 0
					else 1

main = do
	let ncList = [(n,r) | n<-[1..100], r<-[1..100], r<n]
	let count = foldl (\x y -> x + (getCandidate (fst(y)::Int) (snd(y)::Int))) 0  ncList
	print count
	

