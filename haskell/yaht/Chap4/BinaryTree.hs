data BinaryTree a
	= Leaf a
	| Branch (BinaryTree a) a (BinaryTree a)


treeSize :: (BinaryTree a) -> Integer
treeSize (Leaf x) = 1
treeSize (Branch left root right) = 1 + treeSize left + treeSize right

elements :: (BinaryTree a) -> [a]
elements (Leaf x) = [x]
elements (Branch left root right) = (elements left) ++ [root] ++ (elements right)

treeFold f y (Leaf x) = f x y
treeFold f y (Branch left root right) = treeFold f (f root (treeFold f y right)) left

treeFoldl f y (Leaf x) = f x y
treeFoldl f y (Branch left root right) = treeFoldl f (f root (treeFoldl f y left)) right

elementsF = treeFold (:) []

treeSizeF = treeFold (\a b -> b+1) 0

lbt = Branch (Leaf 1) 2 (Leaf 3)
rbt = Branch (Leaf 4) 5 (Leaf 6)
tree = Branch lbt 7 rbt

