import Data.List
--1
--(a)
any' :: (a->Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t)
    |f(h) = True
    |otherwise = any' f (t) 
--(b)
zipWith' :: (a->b->c) -> [a]-> [b]->[c]
zipWith' f (h:t) (x:xs) = (f h x):zipWith' f (t) (xs)
zipWith' f _ _ = []
--(c)
takeWhile' :: (a-> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t)
    |f h = h:takeWhile' f t
    |otherwise = []

--(d)
dropWhile' :: (a->Bool) -> [a]->[a]
dropWhile' f [] = []
dropWhile' f (h:t)
    |f h = dropWhile' f (t)
    |otherwise = h:t

--(e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (h:t)
    |f h = (h:x,y)
    |otherwise = ([],h:t)
    where (x,y) = span' f (t)

--(f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f a [] = []
deleteBy' f a (h:t)
    |f a h = t
    |otherwise = h:deleteBy' f a t

--(g)
sortOn' :: Ord b => (a->b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (x:xs) = insert' f x (sortOn' f xs)

insert' :: Ord b => (a->b) -> a -> [a] -> [a]
insert' _ y []   = [y]
insert' f y (h:t)
  |(f y)<=(f h) = y:(h:t) 
  |otherwise = h:(insert' f y t)
    
--(2)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--selGrau :: Int -> Polinomio -> Polinomio
--selGrau a b = filter (\x y -> a==snd(b)) b

type Mat a = [[a]]
dimOK :: Mat a -> Bool
dimOK (h:c:t)
    |length(h) == length (c) = dimOK t
    |otherwise = False
dimOK _ = True

dimMat :: Mat a -> (Int,Int)
dimMat [[]] = (0,0)
dimMat (h:t)
   |dimOK (h:t) = (length(h),length(h:t))
   |otherwise = error ("matriz mal definida")

--refazer para ficar mais sucinta
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [[]] a = a
addMat a [[]] = a
addMat [x] [y] = [zipWith (+) (x) (y)]
addMat a b 
    |x =zipWith (+) (head(a)) (head(b)) : addMat (tail(a)) (tail(b))
    |otherwise = error ("matrizes mal definidas") 
    where x = dimOK a && dimOK b && dimOK ([head(a)]++[head(b)]) && (dimMat a) == (dimMat b)

transpose' :: Mat a -> Mat a
transpose' a = zipWith (++) (a) []

multmat :: Num a => Mat a -> Mat a -> Mat a
multmat [[]] a = error ("impossivel multipicar as matrizes")
multmat	a [[]] = error ("impossivel multiplicar a matrizes")
multmat l m = [[sum(zipWith (*) l1 l2)|l2<-(transpose m), l1<-l]]

transpose2 :: Mat a -> Mat a
transpose2 [[]] = []
transpose2 [m] = [[x|x<-m]]
transpose2 m = map (head) m :transpose2 (map (tail) m) 
--acabar transpose


{-
normaliza :: Polinomio -> Polinomio
normaliza [a] = [a]
normaliza [] = []
normaliza (h:c:t)
    |snd(h) == snd(c) = normaliza (((fst(h)+fst(c)),snd(h)):t)
    |otherwise = h:normaliza (c:t) 

normaliza2 :: Polinomio -> Polinomio
normaliza2 [a] = [a]
normaliza2 [] = []
normaliza2 (h:t) = foldl (\((c,e):p) (c',e')-> if e==e'
	                                     then (c+c',e):t
	                                     else ((c',e')(c,e):t)) h t

-}