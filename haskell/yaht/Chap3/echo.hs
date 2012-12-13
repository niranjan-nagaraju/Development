module Main
	where

import Control.Monad
import System.IO

main = do
	hSetBuffering stdin LineBuffering
	word <- getLine
	putStrLn ("Echo: " ++ word)
