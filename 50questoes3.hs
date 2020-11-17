import Data.Maybe
--1
enumfromto :: Int -> Int -> [Int]
enumfromto a b
    |a<=b = a:enumfromto (a+1) b
    |otherwise = []

--2
enumfromthento :: Int -> Int -> Int -> [Int]
enumfromthento a b c 
    | a < b && a > c || a > b && a < c = []
    |otherwise = a: enumfromthento b ((b-a)+ b) c
--3
plusplus :: [a] -> [a] -> [a]
plusplus [] [] = []
plusplus [] a = a
plusplus l m = head(l) : plusplus (tail(l)) m 

--4
finder :: [a] -> Int -> a
finder (h:t) a
    |a>=length((h:t)) = error ("List out of elements") 
    |a/=0 = finder (t) (a-1)
    |otherwise = h
--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = last(l) : reverse' (init(l))

--6
take' ::  Int -> [a]-> [a]
take' 0 a = []
take' a (h:t)  
    |a>length(h:t) = error ("List out of length")
    |otherwise = h:take' (a-1) (t) 

--7
drop' :: Int -> [a] -> [a]
drop' 0 a = a
drop' a (h:t)
    |a> length(h:t) = error ("list out of length")
    |otherwise = drop' (a-1) (t)

--8
zip' :: [a] -> [b] -> [(a,b)]
zip' (h:t) (a:as) = (h,a): zip' (t) (as)
zip' _ _ = []

--9 
elem' :: Eq a => a -> [a] -> Bool
elem' a (h:t)
    |a == h = True
    |otherwise = elem' a (t)
elem' a _ = False

--10 
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' a b = b:replicate' (a-1) b

--11
intersperce' :: a -> [a] -> [a]
intersperce' a [b] = [b]
intersperce' a (h:t) = h:a:intersperce' (a) (t)

--12
group' :: Eq a => [a] -> [[a]]
group' (h:t) = takeWhile' h (h:t) : group' (dropWhile' h (h:t))
group' [] = []
--dropWhile :: (a-> Bool)-> [a] -> [a]
dropWhile' :: Eq a => a -> [a] -> [a]
dropWhile' a [] = []
dropWhile' a (h:t)
    |a == h = dropWhile' a (t)
    |otherwise =  (h:t)
takeWhile' :: Eq a => a -> [a] -> [a]
takeWhile' a [] = []
takeWhile' a (h:t)
    |a == h = h:takeWhile' (a) (t)
    |otherwise = []
--13--duvida
--concat' :: [[a]] -> [a] 
--concat' [] = []
--concat' (h:t) = head(h):tail(h) : concat'(t)
--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits2' l (0)

inits2' :: [a] -> Int ->[[a]]
inits2' l a
    |a == length(l) = [l]
    |otherwise = (take'(a) l): (inits2' (l) (a+1))

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = tails2' l (length(l))

tails2' :: [a] -> Int -> [[a]]
tails2' l a 
    |a == 0 = [[]]
    |otherwise = (take' a l):(tails2' l (a-1))
--16
isPrefixOf' :: Eq a => [a] -> [a]-> Bool
isPrefixOf' [] a = True
isPrefixOf' a [] = False
isPrefixOf' (h:t) (a:as)
    |h==a = isPrefixOf' (t) (as)
    |otherwise = False

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:t) = catMaybes'(t)
catMaybes' (Just a:t) = a:catMaybes'(t)