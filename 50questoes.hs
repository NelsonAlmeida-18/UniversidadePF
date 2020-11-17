
-- 1
enumfromto :: Int -> Int->[Int]
enumfromto a b 
    |a == b = [b] 
    |a >b = [a] ++ enumfromto (a-1) b
    |otherwise = [a] ++ enumfromto (a+1) b 
 
-- 2
enumfromthento :: Int -> Int -> Int->[Int]
enumfromthento a b c 
    |a >= c = [c]  
    |otherwise = [a] ++ enumfromthento b (b+(b-a)) c

plusplus :: [a]-> [a]->[a]
plusplus a b 
    |length(a) == 0 = b
    |otherwise = plusplus c d
    where c = init(a)
          d = (last(a):b)
--4
getnum :: [a] -> Int ->a
getnum a b = do
    let c = (length a)
    let d = getnum e f
    if b==0
    then g
    else if (b/=c)
         then d
         else g
    where e = tail(a) 
          f = (b-1)
          g = head(a)


--5
reverse2 :: [a] -> [a]
reverse2 a 
    |(length a)==0 = a
    |otherwise = [last(a)] ++ reverse2(init(a))

--6
take2 :: Int -> [a] -> [a]
take2 n l 
    |(n)== length(l) = l
    |(n) == 0 = []
    |otherwise = [head(l)] ++ take2 (n-1) (tail(l))

--7 
drop2 :: Int -> [a] -> [a]
drop2 x y
    |x == length(y) = []
    |x == 0 = y
    |otherwise = drop2 c d
    where c = x-1
          d = tail(y)
--8
zip' :: [a] -> [b] -> [(a,b)]
zip' (a:as) (b:bs) = (a,b) : zip' as bs
zip' _      _      = []
--9 
elem' :: Eq a => a ->[a]->Bool
elem' a b 
    |length(b) == 0 = False
    |a == head(b) =True
    |otherwise = elem' a (tail(b))
--10
replicate' :: Int -> a -> [a]
replicate' a b 
    |a == 0 = []
    |otherwise = []++[b]++replicate' (a-1) b
--11 
intersperce' :: a -> [a] -> [a]
intersperce' a b 
    |length(b) == 0 = []
    |otherwise = [] ++ [head(b)] ++ [a] ++ intersperce' a (tail(b))
--12

group' :: Eq a => [a]-> [[a]] 
group' [] = []
group' a = [] ++ [takeWhile (==head(a)) a] ++ group'(dropWhile (==head(a)) a)

--13 
concat' ::Eq a =>[[a]] ->[a] 
concat' [] = []
concat' (h:t) = [] ++ takeWhile (==head(h)) h  ++ concat' (t)


--14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' a = inits' (init(a)) ++ [a]

--15
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' a = [] ++[a]++tails'(tail(a))
--16-
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] b = True
isPrefixOf' a b  
    |head(a) == head(b) = isPrefixOf' (tail(a)) (tail(b))
    |otherwise = False
--17 
isSuffixOf' :: Eq a => [a] ->[a]->Bool
isSuffixOf' [] b = True
isSuffixOf' a b 
    |last(a) == last(b) = isSuffixOf' (init(a)) (init(b))
    |otherwise = False
--18

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' a b 
    |length(a) == 0 = True
    |length(b) == 0 = False
    |head(a) == head(b) = isSubsequenceOf' (tail(a)) (tail(b))
    |otherwise = isSubsequenceOf' a (tail(b))

--19
elemIndices' :: Eq a => a -> [a] ->[Int]
elemIndices' _ [] = []
elemIndices' a b = elemIndicesCounter' a b 0

elemIndicesCounter' :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesCounter' a [] c = []
elemIndicesCounter' a b c
    |a == head(b) = [c] ++ elemIndicesCounter' a (tail(b)) (c+1)
    |otherwise = elemIndicesCounter' a (tail(b)) (c+1)

--20
nub2' :: Eq a=> [a] ->[a]
nub2' b  
    |length(b) == 0 = []
    |head(b) `elem` b = [head(b)] ++ nub2' (x)
    |otherwise = nub2' (tail(x))
    where x = [x | x<- b , x/=head(b)]
--21
delete' :: Eq a => a -> [a] ->[a]
delete' a b 
    |length(b) == 0 = []
    |a == head(b) = tail(b)
    |otherwise = [head(b)] ++ delete' a (tail(b)) 
 

--22 
myslashslash :: Eq a => [a] -> [a] -> [a]
myslashslash a b
    |length(a) == 0 = []
    |length(b) == 0 = a
    |head(a) == head(b) = myslashslash (tail(a)) (tail(b))  
    |otherwise = head(a) : myslashslash (tail(a)) (b)
--23
union :: Eq a => [a] -> [a] -> [a]
union a b 
    |length(a) == 0 =[]
    |length(b) == 0 =[]
    |head(a) == head(b) = union (tail(a)) (tail(b))
    |otherwise = [head(a)] ++ union (tail(a)) b

--24
intersect :: Eq a => [a] -> [a] -> [a]
intersect a b 
    |length(a) == 0 = []
    |length(b) == 0 = []
    |head(a) == head(b) = [] ++ [head(a)] ++ intersect (tail(a)) b
    |otherwise = intersect a (tail(b))
--25
insert :: Ord a => a -> [a] -> [a]
insert _ [] = []
insert a b 
    |a> head(b) && a<head(tail(b)) = [head(b)] ++ (a:tail(b))
    |otherwise = insert a (tail(b))

--26
unwords' :: [String] -> String
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

--27 
unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t

--28
pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior (h:x) = pMaior2 (h:x) h 0 0

pMaior2 :: Ord a => [a] -> a -> Int -> Int -> Int
pMaior2 [] _ b _ = b
pMaior2 (h:x) (a) (b) (c)
    |a>=h = pMaior2(x) a (c) (c+1)
    |otherwise = pMaior2(x) h (c+1) (c+1)
--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) 
    |t == [] = False
    |elemRep h t = True
    |otherwise = temRepetidos t

elemRep :: Eq a => a -> [a] -> Bool
elemRep _ [] = False
elemRep a (h:t)
    |a == h = True
    |otherwise = elemRep a t

--30
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t)
    |(h<='9') && (h>= '0')= h : (algarismos t)  
    |otherwise = algarismos t

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (h:t) = [head(t)] ++ posImpares(tail(t))

--32
posPares :: [a] -> [a]
posPares [] = []
posPares (h:t) = [h] ++ posPares(tail(t))

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (h:t:x) 
    |h<=t=isSorted (t:x)
    |otherwise = False 

--34
--again later
iSort :: Ord a => [a] -> [a]
iSort [xs] = [xs]
iSort a = iSort2 a a
iSort2 :: Ord a => [a] -> [a] -> [a]
iSort2 (h:t:xs) b
    |(h:t:xs) == [] && isSorted b == True = b
    |h>= t = iSort2(h:xs) (t:h:xs)
    |otherwise = iSort2(t:xs) (h:t:xs)
iSort2 _ b = b

--35
menor :: String -> String -> Bool
menor a [] = False
menor [] b = True
menor (h:t) (x:xs)
    |h>x = False
    | h<x = True
    |otherwise = menor (t) (xs)
--36--duvida, a == Int ou só a?
elemMSet :: Eq a => a-> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet a (h:t)
    |a == fst(h) = True
    |otherwise = elemMSet a (t)

--37 
lengthMSet :: [(a,Int)] -> Int
lengthMSet (h:t) = lengthMSet2 (h:t) 0
lengthMSet _ = 0

lengthMSet2 :: [(a,Int)] -> Int -> Int
lengthMSet2 [] b = b
lengthMSet2 (h:t) b = lengthMSet2 (t) (b+(snd(h)))

--38--Posso trocar String por [a]???????
--
--- 
--
converMSet :: [(a,Int)] -> [a]
converMSet [] = []
converMSet (h:t) =  (multiplier (fst(h)) (snd(h)) [])  ++ converMSet(t)

multiplier :: a -> Int -> [a] -> [a]
multiplier _ 0 c = c
multiplier a b c =  multiplier a (b-1) (a:c)

--39 
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = []
insereMSet a (h:t) 
    |a == fst(h) = [(a,(snd(h)+1))] ++ insereMSet a (t)
    |otherwise = [h] ++ insereMSet a (t)

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [] = []
removeMSet a (h:t) 
    |a == fst(h) = [(a,snd(h)-1)] ++ removeMSet a t 
    |otherwise = [h] ++ removeMSet a t

--41 --com uso de dropwhile
constroiSet :: Ord (a) =>  [a] -> [(a,Int)] 
constroiSet (h:t:xs) 
    |h == t = [(h,length(constroiSet2 (h:t:xs)))] ++ constroiSet (dropWhile(==h) (h:t:xs))
    |otherwise = [(h,1)] ++ constroiSet (t:xs)

constroiSet [] = []

constroiSet2 :: Eq a => [a] -> [a]
constroiSet2 [] = []
constroiSet2 (h:xs) = takeWhile (==h) (h:xs)

--41 sem uso de dropwhile

--42--?? Either??--
--partitionEithers :: [Either a b] -> ([a],[b])
--partitionEithers ((Left a):t) = (a:partitionEithers(t),[])
--partitionEithers ((Right b):t) = ([],[]++[b]) ++ partitionEithers(t)

--43--como executo??
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):t) = a : catMaybes(t)
catMaybes ((Nothing):t) = catMaybes (t) 

--44
data Movimento = Norte | Sul | Este | Oeste deriving(Show)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (Norte:t) = posicao(a,b+5) (t)
posicao (a,b) (Sul:t) = posicao(a,b-5) (t)
posicao (a,b) (Este:t) = posicao(a+5,b) (t)
posicao (a,b) (Oeste:t) = posicao(a-5,b) (t)
--45
caminho :: (Int, Int) -> (Int, Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    |xi == xf && yi == yf = [] 
    |xi>xf = Oeste : caminho(xi,yi) (xf+1,yf)
    |xi>xf = Este : caminho(xi,yi) (xf-1,yf)
    |yi>yf = Sul : caminho(xi,yi) (xf,yf+1)
    |yi<yf = Norte : caminho(xi,yi) (xf,yf-1)
    |otherwise = caminho(xi,yi) (xf,yf)
--46
vertical :: [Movimento] -> Bool
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (Oeste:t) = False
vertical (Este:t) = False
vertical _ = True


--47
data Posicao = Pos Int Int deriving(Show)
maisCentral :: [Posicao] -> Posicao
maisCentral ((Pos a b):t) = maisCentral2 ((Pos a b):t) (Pos a b)

maisCentral2 :: [Posicao] -> Posicao -> Posicao
maisCentral2 [] (Pos e f) = (Pos e f)
maisCentral2 ((Pos a b):t) (Pos e f)
    |abs(a) >= abs(e) && abs(b)>=abs(f) = maisCentral2(t) (Pos e f)
    |abs(a) <= abs(e) && abs(b)<=abs(f) = maisCentral2(t) (Pos a b)
    |otherwise = maisCentral2(t) (Pos a b)
--48-- É assim??
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos a b) ((Pos c d):t)
    |b==d || b==(d+1) || b == (d-1) || a==c || a==(c-1) || a==(c+1)= (Pos c d) : vizinhos(Pos a b) (t)
    |otherwise =  vizinhos (Pos a b) (t)

--49
mesmaOrdenada ::[Posicao] -> Bool
mesmaOrdenada ((Pos a b):(Pos c d):t) 
    |b == d = mesmaOrdenada ((Pos c d):t)
    |otherwise = False

mesmaOrdenada _ = True

-- 50
data Semaforo = Verde | Amarelo | Vermelho  deriving (Show)
intersecaoOK :: [Semaforo] -> Bool
intersecaoOK (Verde:t) = intersecaoOK(t)
intersecaoOK (Vermelho:t) = False
intersecaoOK (Amarelo:t) = False
intersecaoOK _ = True