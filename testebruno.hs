{-
reverse' :: [a] -> [a]
reverse' [] = []
reverse' a = last(a):reverse' (init(a))


insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t) 
    |a<=h  = a:h:t
    |otherwise = h:insert' a (t)

pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) = pMaior2 (h:t) (h) (0) (0)

pMaior2 :: Ord a => [a]->a -> Int -> Int -> Int
pMaior2 [] a b c= b
pMaior2 (h:t) a b c
    |h>= a = pMaior2 (t) h (c) (c+1)
    |otherwise = pMaior2 (t) a (b) (c+1)

concat' :: [[a]] -> [a]
concat' ((h:x):t)
    |length(x) == 0 = h:concat' t
    |otherwise = h:concat' (x:t)
concat' _ = []


data Movimento  = Norte | Sul |Este | Oeste deriving(Show)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d)
    |a == c && b == d = []
    |(a-c)<0 = Norte:caminho((a+1),b) (c,d)
    |(a-c)>0 = Sul:caminho((a-1),b) (c,d)
    |(b-d)>0 = Oeste:caminho(a,(b-1)) (c,d)
    |(b-d)<0= Este:caminho(a,(b+1)) (c,d)

--menmai :: Ord a => a -> [a] -> ([a],[a])
menmai x (a:as)
    |a > x = (p,a:q)
    |otherwise = (a:p,q)
    where (p,q) = menmai x as
menmai _ [] = ([],[])

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t)
    |b == 0 = converteMSet t
    |otherwise = a:converteMSet((a,(b-1)):t)
-}

nub :: Eq a=> [a] -> [a]
nub [] = []
nub (h:t)
    |nubaux (h) (t) = nub(t)
    |otherwise = h:nub(t)

nubaux :: Eq a => a -> [a] -> Bool
nubaux a (h:t) 
    |a == h = True
    |otherwise = nubaux a t
nubaux _ [] = False




intersect :: Eq a => [a] -> [a] -> [a]
intersect [] a= []
intersect (h:t) (x:xs) 
    |intersectelem h (x:xs) = (h:(intersect t (x:xs)))
    |otherwise = intersect t (x:xs)	

intersectelem :: Eq a => a -> [a] -> Bool
intersectelem a (h:t) 
    |a == h = True
    |otherwise =  intersectelem	a t	
intersectelem _ _ = False