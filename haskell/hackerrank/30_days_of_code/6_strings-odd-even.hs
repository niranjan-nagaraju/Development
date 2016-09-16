{-
 - https://www.hackerrank.com/challenges/30-review-loop
 -}

-- Skip alternate characters in a string while printing
printAltChars [] _  = return ()
printAltChars (x:xs) skip = do
        if skip == False
            then do
                putChar x
                printAltChars xs True
            else printAltChars xs False	


-- Don't skip from idx 0, => print every even indexed character
printEvenChars s = printAltChars s False

-- skip from idx 0, => print every odd indexed character
printOddChars s = printAltChars s True


-- Process Current Line, print even chars first and then odd chars
processLine :: String -> IO ()
processLine s = do
	printEvenChars s 
	putChar ' '
	printOddChars s 
	putChar '\n'

-- Read n lines and process separately
getLinesAndProcess :: Int -> IO ()
getLinesAndProcess 0 = return ()
getLinesAndProcess n = do
	s <- getLine
	processLine s
	getLinesAndProcess (n-1)

main = do
	n <- readLn :: IO Int
	getLinesAndProcess n
