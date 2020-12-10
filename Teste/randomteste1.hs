--8
zip' :: [a]-> [b] -> [(a,b)]
zip' (h:t) (x:xs) = (h,x): zip'(t) (xs)
zip' _ _ =[]
--41
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' (h:t) = (h,a) : constroiMSet' (b)
    where (a,b) = constroiMSet2 h (t) (1) ([])
constroiMSet' _ = []

constroiMSet2 :: Eq a => a -> [a] -> Int -> [a] -> (Int,[a])
constroiMSet2 a (h:t) b c
    |a == h = constroiMSet2 a (t) (b+1) (c)
    |otherwise = constroiMSet2 (a) ([]) (b) (h:t)
constroiMSet2 a [] b c = (b,c)

constroiMSet'' :: Ord a => [a] -> [(a,Int)]
constroiMSet'' [] = []
constroiMSet'' l = aux 1 l
      where 
           aux i [x] = [(x,i)]
           aux i (h:s:t) = if h == s 
                              then aux (i+1) (s:t)
                           else (h,i):aux 1 (s:t) 

constroiMSet3 :: Ord a => [a] -> [(a,Int)]

constroiMSet3 [] = []
constroiMSet3 (x:xs) = (x, repete x (x:xs)):constroiMSet3 (remove x xs)
    where
        repete x [] = 0
        repete x (y:xs) = if x == y then repete x xs + 1 else 0

        remove x [] = []
        remove x (y:xs) = if x == y then remove x xs else (y:xs) 
--26
unwords' :: [String] -> String
unwords' [] = ""
unwords' [a] = a
unwords' (h:t) = h ++ " " ++ unwords' (t)

--45
data Movimento = Norte | Sul | Este | Oeste deriving (Show)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (c,d)
    |a == c && b == d= []
    |a<c = Este:caminho ((a+1),b) (c,d)
    |a>c = Oeste : caminho ((a-1),b) (c,d)
    |b<d = Norte : caminho (a,(b+1)) (c,d)
    |otherwise = Sul : caminho (a,(b-1)) (c,d)
--21
delete' :: Eq a => a -> [a]-> [a]
delete' a [] = []
delete' a (h:t)
    |a == h = t
    |otherwise = h:delete' a (t)
--28--fazer de novo
pMaior' :: Ord a => [a] -> Int
pMaior' [] = error "Lista sem elementos"
pMaior' (h:t) = pMaior2' h (t) (0) (0)

pMaior2' :: Ord a => a -> [a] -> Int -> Int -> Int 
pMaior2' a [] b c = b
pMaior2' a (h:t) (b) (c) 
    |a >= h = pMaior2' (a) (t) (c) (c+1)
    |otherwise = pMaior2' (h) (t) (b) (c+1)
--14
inits' :: [a] -> [[a]]
inits' l = inits2' (l) (0)

inits2' :: [a] -> Int -> [[a]]
inits2' l b 
    |length(l) == (b-1) = []
    |otherwise = take b l : inits2' l (b+1) 
--23
union' :: Eq a => [a] -> [a]-> [a]
union' l [] = l
union' (l) (x:xs)
    |aux x l = union' l (xs)
    |otherwise = union' (x:l) (xs)
    where aux a (h:t) |a == h = True |otherwise = aux a (t)
          aux a _ = False
--24
concat' :: [[a]] -> [a]
concat' [[a]] = [a] 
concat' ((h:t):xs)
    |length(t) == 0 = h:concat' xs
    |otherwise = h:concat' ((t):xs)
--2
enumFromThenToo :: Int -> Int -> Int -> [Int]
enumFromThenToo a b c 
    | a < b && a > c || a > b && a < c = []
    |otherwise = a: enumFromThenToo b ((b-a)+ b) c

--3
plusplus :: [a] -> [a] -> [a]
plusplus l [] = l
plusplus [] l = l
plusplus m l = plusplus (init(m)) ((last(m)):l)

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((_,b):t) = b+lengthMSet t

--3
drop' :: Int -> [a] -> [a]
drop' 0 a = a
drop' a (h:t)= drop' (a-1) (t)
drop' a [] = error ("not enough elements in list")

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a ((b,_):t)
    |a == b = True
    |otherwise = elemMSet a (t)

inits :: [a] -> [[a]]
inits [] = []
inits l = aux l (0)
    where aux a b|length(a) == b = [a] |otherwise = take b a : aux a (b+1)


caminho' :: (Int, Int) -> (Int,Int) -> [Movimento]
caminho' (a,b) (c,d)
    |a == c && b == d = []
    |a<c = Este: caminho' ((a+1),b) (c,d)
    |a >c = Oeste : caminho' ((a-1),b) (c,d)
    |b<d = Norte : caminho' (a,(b+1)) (c,d)
    |b>d = Sul : caminho' (a,(b-1)) (c,d)
    |otherwise = error("")

--42
partitionEithers' :: [Either a b]-> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of
                             (Left a) -> (a:x,y)
                             (Right b) -> (x,b:y)
                    where (x,y) = partitionEithers' t
--43
catMaybe :: [Maybe a] -> [a]
catMaybe [] = []
catMaybe ((Just a):t) = a:catMaybe t
catMaybe (_:t) = catMaybe t

--19
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a [] = []
elemIndices a (h:t) = elemIndicesaux a (h:t) (0)
    where elemIndicesaux a [] c = [] 
          elemIndicesaux a (h:t) c|a== h = c:elemIndicesaux a (t) (c+1) |otherwise = elemIndicesaux a t (c+1) 


--30
algarismos :: String -> String
algarismos [] = []
algarismos (h:t) 
    |h<='9' && h>='0' = h:algarismos(t)
    |otherwise = algarismos(t)

--15
tails :: [a] -> [[a]]
tails [] = []
tails l = tailsaux (l) (length(l))
    where tailsaux a 0 = [[]]
          tailsaux l a = take (a) l : tailsaux (l) (a-1)

--23
union :: Eq a =>[a] -> [a] -> [a]
union [] a = a
union a [] = a
union l (h:t)
    |unionaux l h = union l t
    |otherwise = union (l++[h]) t
    where unionaux :: Eq a => [a] -> a -> Bool
          unionaux [] a = False 
          unionaux (h:t) a|a == h = True |otherwise = unionaux t a

--16
isPrefixOf' :: Eq a => [a]-> [a] -> Bool
isPrefixOf' [] a = True
isPrefixOf' a [] = False
isPrefixOf' (h:t) (x:xs)
    |h == x = isPrefixOf' (t) (xs)
    |otherwise = isPrefixOf' (h:t) (xs)

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' a [] = False
isSuffixOf' [] a = True
isSuffixOf' l m
    |last(l) == last(m) = isSuffixOf' (init(l)) (init(m))
    |otherwise = isSuffixOf' (l) (init(m))


--10
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' a b = b:replicate' (a-1) b

--32
posPares :: [a] -> [a]
posPares [] = []
posPares [a] = [a]
posPares [a,c] = [a]
posPares (h:c:t) = h:posPares t
