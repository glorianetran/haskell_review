map2 f [] = []
map2 f (x:[]) = [f x] 
map2 f (x:xs) = (f x) : map2 f xs

mapper :: (a -> b) -> [a] -> [b]
mapper f [] = []
mapper f lst = [f x| x <- lst]

mapper2 :: (a-> b -> c) -> [a] -> [b] -> [c]
mapper2 _ _ _ = []
mapper2 f (x:xs) (y:ys) = (f x y) : (mapper2 f xs ys)

mapper3 f lst1 lst2 = [(f x y)| x <- lst1,y <-lst2 ] 

allSame :: (Eq b) => (a -> b) -> [a] -> Bool
allSame _ [] = True 
allSame f (x: []) = True 
allSame f (x: y: []) = (f x == f y) 
allSame f (x:y:ys) = if (f x == f y) then allSame f (y:ys) else False

listCopy :: [t] -> [t]
listCopy [] = []
listCopy (x:xs) = x : listCopy xs

map1 :: (t -> a) -> [t] -> [a]
map1 f [] = []
map1 f (x:xs) = (f x) : (map1 f xs)

addLists :: (Num a) => [a] -> [a] -> [a]
addLists _ [] = []
addLists [] _ = []
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base [] = base 
myfoldl f base (x:xs) = myfoldl f (f base x) xs

myfoldr f base [] = base 
myfoldr f base (x:xs) = f x (myfoldr f base xs)

--three parameters
factors n = filter (\x -> (mod n x) == 0) [1..(n-1)]
isPerfect n = (foldl (+) 0 (factors n)) == n

factors2 :: (Integral a) => a -> [a] --need it cause of mod
factors2 n = [x | x <- [1 .. (n-1)] , (mod n x) == 0 ] 
perfectNum :: (Integral a) => a -> a -> [a]
perfectNum x y = [n | n <- [x .. y] , (foldl (+) 0 (factors2 n)) == n]

isPrime :: (Integral a) => a -> Bool
isPrime n = (length(factors2 n)) == 1
allPrimes :: (Integral a) => a -> a -> [a]
allPrimes x y = [n | n <- [x .. y], (isPrime n)]

isPrime2 :: (Integral a) => a -> Bool
isPrime2 0 = False
isPrime2 1 = False
isPrime2 n = (length(take 2 (factors2 n)))  <= 1 

allSame2 :: (Eq b) => (a -> b) -> [a] -> Bool
allSame2 _ [] = True
allSame2 _ (x:[]) = True
allSame2 funct lst = 
    let newlst = map (\x -> funct x) lst
    in all (== head newlst) (tail newlst)

foodChooser :: [Char] -> [Char]
foodChooser n
	| n == "tofu" = "Susana"
	| n == "yam" = "John"
	| n == "apple" = "fruity"
	| n == "cheese" = "cheesy"
	| otherwise = "watermelonnnnn"

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

doubleMe :: (Num a) => a -> a
doubleMe x = x + x  

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y   

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' :: (Num b) => [a] -> b
length' [] = 0  
length' (_:xs) = 1 + length' xs  

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2  

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  

whatMyNumber :: (Num a, Eq a) => a -> [Char]
whatMyNumber n = if n == 5 then "awesome" else "meh"

counterPositive [] = 0
counterPositive (x:[]) = if x > 0 then 1 else 0
counterPositive (x:xs) = if x > 0 then 0 + 1 else counterPositive xs
