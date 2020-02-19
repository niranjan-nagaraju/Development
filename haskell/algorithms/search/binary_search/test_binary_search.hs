import BinarySearch
import Control.Exception (assert)

main = do
	putStr $ assert ((binary_search [1,2,3,4,5] 6) == -1) ""
	putStr $ assert ((binary_search [1,2,3,4,5] 5) == 4) ""
	putStr $ assert ((binary_search [1,2,3,4,5] 0) == -1) ""
	putStr $ assert ((binary_search [1,2,3,4,5] 1) == 0) ""
	putStr $ assert ((binary_search [1,2,3,4,5] 2) == 1) ""

	putStr $ assert ((binary_search [1,3,5,7] 2) == -1) ""
	putStr $ assert ((binary_search [1,3,5,7] 1) == 0) ""
	putStr $ assert ((binary_search [1,3,5,7] 7) == 3) ""
	putStr $ assert ((binary_search [1,3,5,7] 4) == -1) ""
	putStr $ assert ((binary_search [1,3,5,7] 3) == 1) ""
	putStr $ assert ((binary_search [1,3,5,7] 5) == 2) ""
