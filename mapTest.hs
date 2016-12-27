mapper :: (a -> b) -> [a] -> [b]
mapper f [] = []
mapper f lst = [f x| x <- lst]


mapper2 :: (a-> b -> c) -> [a] -> [b] -> [c]
mapper2 _ _ _ = []
mapper2 f (x:xs) (y:ys) = (f x y) : (mapper2 f xs ys)

--figure out how to put in list notation
mapper3 f lst1 lst2 = [(f x y)| x <- lst1, y <-lst2] 

allSame _ [] = True -- if empty list return true
allSame f (x: []) = True -- if one element return true 
allSame f (x: y: []) = (f x == f y) -- if the first two elements are equal check them 
--allSame f (x: y: ys) = if (f x == f y) then allSame f ys else False 
allSame f (x:y:ys) = if (f x == f y) then allSame f (y:ys) else False

listCopy[] = []
listCopy (x:xs) = x : listCopy xs

map1 :: (t -> a) -> [t] -> [a]
map1 f [] = []
map1 f (x:xs) = (f x) : (map1 f xs)

take _ [] = []
take 0 (x:s) = []
take n (x:s) = x : take (n-1) s

addLists _ [] = []
addLists [] _ = []
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys