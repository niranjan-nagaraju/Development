import IO

triplets_array_sum lst sum = 
	[(x,y,z) | x <- lst, y <- lst, z <- lst, (x+y+z == sum) && (x<y) && (y<z)]

main = do
	print $ triplets_array_sum [1,2,3,4,5,6] 10 
