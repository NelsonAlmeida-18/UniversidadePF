import Data.Either
import Data.Char

--partitionEithers'' :: [Either a b] -> ([a],[b])
--partitionEithers'' [] = ([], [])
--partitionEithers'' (h:t) 
--    case h of 
--    	(Left a) -> ([]++a,b):partitionEithers'' (t)
--    	(Right b) -> (a,[]++b):partitionEithers'' (t)

constroiMSet :: [(a,Int)] -> [a]
constroiMSet [] = []
constroiMSet ((a,b):t)
    |b == 0 = constroiMSet(t)
    |otherwise = a:constroiMSet((a,(b-1)):t)


delete2 :: Eq a=> a-> [a] -> [a]
delete2 a [] = []
delete2 a (h:t)
    |a == h = t
    |otherwise = h:(delete2 a (t))


menor :: String -> String -> Bool
menor [] b = True
menor a [] = False
menor (h:t) (x:xs)
    |ord(h)<=90 && ord(h)>= 65 && ord(x)<=90 && ord(x)>= 65 && a>b = False
    |ord(h)<=90 && ord(h)>= 65 && ord(x)<=90 && ord(x)>= 65 && a<b = True
    |ord(h)<=90 && ord(h)>= 65 && ord(x)<=90 && ord(x)>= 65  && a==b = menor(t) (xs)
    |a>b = False
    |a<b = True
    |otherwise = menor (t) (xs)
     where a = (ord(h)+32)
           b = (ord(x)+32)

group' :: Eq a=>[a] -> [[a]]
group' [] = []
group' (h:t) = (group2' h (t)) : group' (group3' h t 0)

group2' ::Eq a=> a -> [a]  -> [a]
group2' a [] = [a]
group2' a (h:t)
    |a == h = h:group2' a (t)
    |otherwise = [a]

group3' :: Eq a => a -> [a] -> Int -> [a]
group3' a [] c= []
group3' a (h:t) c
    |a == h && c==0 = group3' a (t) (c)
    |otherwise = h:group3'(a) (t) (c+1)


partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Right a):t) = (c,a:d)
    where (c,d) = partitionEithers' (t)
partitionEithers' ((Left a):t) = (a:c,d)
    where (c,d) = partitionEithers' (t)


insert :: Ord a =>a -> [a] -> [a]
insert a [] = [a]
insert a (b:c:t)
    |a>=b && a<c = b:a:c:t
    |a<b = a:b:c:t
    |a == b = a:b:c:t
    |otherwise = b:(insert a (c:t))
insert _ _ = []