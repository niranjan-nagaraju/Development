module List
	where

data List a = Nil
			| Cons a (List a)

listLength :: (List a) -> Int
listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead :: (List a) -> a
listHead (Cons x xs) = x

listTail :: (List a) -> (List a)
listTail (Cons x xs) = xs

listFoldl :: (a -> b -> a) -> a -> (List b) -> a
listFoldl f y Nil = y
listFoldl f y (cons x xs) = listFoldl f (f y x) xs

listFoldr :: (a->b->a) -> a -> (List b)	-> a
listFoldr f y Nil = y
listFoldr f y (cons x xs) = f x (listFoldr f y xs)

list1 = Cons 1 (Nil)
list2 = Cons 2 (list1)
list3 = Cons 3 (list2)
