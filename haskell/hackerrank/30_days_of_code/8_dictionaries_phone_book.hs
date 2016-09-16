import qualified Data.Map as Map
import Control.Monad

-- Add [name, number] to the map
addToMap :: [String] -> (Map.Map String String) -> (Map.Map String String)
addToMap nameNumberList m =
	let [name,number] = nameNumberList 
	in
	Map.insert name number m

-- Read n lines and process separately
getLinesAndProcess :: Int -> (Map.Map String String) -> IO (Map.Map String String)
getLinesAndProcess 0 phonebook = return phonebook 
getLinesAndProcess n phonebook = do
	name_number <- getLine
	getLinesAndProcess (n-1) (addToMap (words name_number) phonebook)


-- Print number if name found in phonebook, else print 'Not found'
processResult :: String -> Maybe String -> IO ()
processResult name (Just x) = do putStrLn (name ++ "=" ++ x)
processResult _ Nothing = do putStrLn "Not found"

lookupPhonebook phonebook name = do
	processResult name (Map.lookup name phonebook)
	

main = do
	n <- readLn :: IO Int
	phonebook <- getLinesAndProcess n Map.empty
	names <- getContents
	mapM_ (lookupPhonebook phonebook) (lines names) 
	
{- Test run -
 - $ runghc 8_dictionaries_phone_book.hs 
 - 3
 - sam 99912222
 - tom 11122222
 - harry 12299933
 - sam
 - 99912222
 - edward
 - Not found
 - harry
 - 12299933
 - $
 -} 
