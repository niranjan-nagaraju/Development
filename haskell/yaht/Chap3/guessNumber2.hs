module Guess
	where

import IO
import Random
import Control.Monad

main = do
	hSetBuffering stdin LineBuffering
	num <- randomRIO (1::Int, 100)
	putStrLn "I thought of a number between 1 to 100.. Guess"
	startGuessing num

startGuessing num = do
	guess <- getLine
	let guessNum = read guess

	if guessNum > num
		then do 
			putStrLn "Too High";
			startGuessing num
		else if guessNum < num
			then do 
				putStrLn "Too Low"
				startGuessing num
			else do 
				putStrLn "You Win!"
