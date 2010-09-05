module Main
	where

import Control.Monad
import IO

main = do
	hSetBuffering stdin LineBuffering
	word <- getLine
	putStrLn ("Echo: " ++ word)
