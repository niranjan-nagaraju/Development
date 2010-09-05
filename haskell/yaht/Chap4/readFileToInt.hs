import IO

main = do
	s <- readFile "infileInt"
	let i = (read s)::Integer
	print i
