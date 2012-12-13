{- Example of 'switch' case haskell -}

-- Version 1
casef x =
	case x of
		0 -> 1
		1 -> 5
		2 -> 3
		_ -> -1

-- Version 2
casef2 x = 
	case x of
		{0->1; 1->5; 2->3; _-> -1}


-- Version 3
f 0 = 1
f 1 = 5
f 2 = 3
f _ = -1
