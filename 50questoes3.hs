import Data.Maybe
import Data.Char
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

--17
isSuffixOf'::Eq a => [a] -> [a] -> Bool
isSuffixOf' [] x = True
isSuffixOf' a [] = False
isSuffixOf' l x
    |last(l) == last(x) = isSuffixOf' (init(l)) (init(x))
    |otherwise = False

--18
isSubsequenceOf' :: Eq a =>[a] -> [a] -> Bool
isSubsequenceOf' [] a = True
isSubsequenceOf' a [] = False
isSubsequenceOf'(h:t) (x:xs)
    |h == x = isSubsequenceOf' (t) (xs)
    |otherwise = isSubsequenceOf' (h:t) (xs)
--19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' l x = elemIndices2' l x (0)

elemIndices2' :: Eq a => a -> [a] -> Int -> [Int]
elemIndices2' l [] c = []
elemIndices2' l (h:t) c
    |l ==h = c:elemIndices2' l (t) (c+1)
    |otherwise = elemIndices2' l t (c+1) 

--20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) 
    |nub2' h (t) = nub' t
    |otherwise = h:nub'(t) 
nub2' :: Eq a=>a -> [a] -> Bool
nub2' a [] = False
nub2' a (h:t)
    |a == h = True
    |otherwise = nub2' a (t)
--21
delete' :: Eq a => a -> [a] -> [a]
delete' a [] = []
delete' a (h:t)
    |a == h = t
    |otherwise = h:delete' (a) t  

--22
multipledelete :: Eq a => [a] -> [a]-> [a]
multipledelete a [] = a
multipledelete (h:t) (x:xs)
    |h==x = multipledelete (t) (xs)
    |otherwise = h : multipledelete (t) (x:xs)
--23
union' :: Eq a=> [a] -> [a] -> [a]
union' l [] = l
union' l (x:xs)
    |x `elem` l = union' (l) (xs)
    |otherwise = union' (l++[x]) (xs)
--24
intersect' :: Eq a => [a] ->[a] -> [a] 
intersect' [] l = []
intersect' (h:t) l
    |intersectelm' (h) (l) = h:intersect' (t) l
    |otherwise = intersect' t l

intersectelm' :: Eq a => a -> [a] -> Bool
intersectelm' a [] = False
intersectelm' a (h:t)
    |a == h = True
    |otherwise = intersectelm' a (t)

--25--perguntar se é recursiva
insert' :: Ord a => a -> [a]-> [a]
insert' a [] = [a]
insert' a [c]
    |a>c = [c,a]
    |otherwise = [a,c]
insert' a (h:c:t)
    |a>h && a<c= h:a:c:t
    |a<h = a:h:c:t
    |otherwise = h:insert' a (c:t)
--26
unwords' :: [String] -> String
unwords' [t] = t
unwords' (h:t) = h++ " "++ unwords' (t)

--27
unlines' :: [String] -> String
unlines' [t] = t
unlines' (h:t) = h ++ "\n" ++ unlines' (t)

--28
pMaior' :: Ord a =>[a] -> Int
pMaior' l = pMaior2' l (0)

pMaior2' :: Ord a => [a] -> Int -> Int
pMaior2' [t] a = a
pMaior2' (h:c:t) a 
    |h>=c = pMaior2' (h:t) (a)
    |otherwise = pMaior2' (c:t) (a+1)

--29
temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (h:t) 
    |temRepetidoselem' (h) (t) = True
    |otherwise = temRepetidos' (t)

temRepetidoselem' :: Eq a=> a -> [a] -> Bool
temRepetidoselem' a [] = False
temRepetidoselem' a (h:t)
    |a == h = True
    |otherwise = temRepetidoselem' a (t)

--30
algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t)
    |h>='0' && h<='9' = h:algarismos' (t)
    |otherwise = algarismos' (t) 

--31
posImpares' :: [a] -> [a]
posImpares' [h,c] = [c]
posImpares' [h,c,t] = [c]
posImpares' (h:c:t) = c:posImpares' (t)

--32
posPares' :: [a] -> [a]
posPares' [h,c] = [h]
posPares' [h,c,t] = [h,t]
posPares' (h:t) = h:posPares' (tail(t))

--33
isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' [h,c,d] 
    |h<=c && c<=d ||h>=c && c>=d  = True
    |otherwise =  False

isSorted' (h:c:d:t)
    |h<=c && c<=d = isSorted' (c:d:t)
    |h>=c && c>=d = isSorted' (c:d:t)
    |otherwise = False

--34
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = iSort2' (h:t) ([])

iSort2' :: Ord a => [a] -> [a] -> [a]
iSort2' [] b = b
iSort2' [x] b = insert' x b
iSort2' (h:t) b = iSort2' (t) (x)
    where x = insert' h b

--35 --perguntar se é necessário converter maíuscula para minuscula
menor' :: String -> String -> Bool
menor' [] a = True
menor' (h:t) (x:xs)
    |ord(h)<ord(x) = True
    |ord(h)==ord(x) = menor' (t) (xs)
    |otherwise = False 
menor' _ _ = False

--36
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' a [] = False
elemMSet' a ((b,_):t)
    |a == b = True
    |otherwise = elemMSet' (a) t

--37
lengthMSet' :: [(a,Int)] -> Int 
lengthMSet' [] = 0
lengthMSet' ((_,a):t) = a+lengthMSet' t

--38
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((a,b):t)
    |b/=0 = a:converteMSet' ((a,(b-1)):t)
    |otherwise = converteMSet' (t)
--39
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' a [] = [(a,1)]
insereMSet' a ((b,c):t)
    |a == b = (b,(c+1)):t
    |otherwise = (b,c):insereMSet' a (t)

--40
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' a [] = [(a,1)]
removeMSet' a ((b,c):t)
    |a==b = ((b,(c-1)):t)
    |otherwise = (b,c):removeMSet' a (t)
--41
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (h:t) = constroiMSet2' (h) (t) (1) : constroiMSet' (dropWhile (==h) (h:t))

constroiMSet2' :: Ord a => a -> [a] -> Int->(a,Int)
constroiMSet2' a [] b = (a,b)
constroiMSet2' a (h:t) b 
   |a == h = constroiMSet2' (a) (t) (b+1)
   |otherwise = constroiMSet2' (a) (t) (b)

--42
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:x,y)
    where (x,y) = partitionEithers' (t)
partitionEithers' ((Right a):t) = (x,a:y)
    where (x,y) = partitionEithers' (t)
--43
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Nothing):t) = catMaybes' (t)
catMaybes' ((Just a):t) = a:catMaybes' (t)

--44
data Movimento = Norte | Sul | Este | Oeste deriving (Show)

posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (a,b) [] = (a,b)
posicao' (a,b) (Norte:t)=posicao' ((a+1),b) (t)
posicao' (a,b) (Sul:t)=posicao' ((a-1),b) (t)
posicao' (a,b) (Este:t)=posicao' (a,(b+1)) (t)
posicao' (a,b) (Oeste:t)=posicao' (a,(b-1)) (t)

--45

caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (a,b) (c,d) 
    |c>a = Norte : caminho' ((a+1),b) (c,d)
    |c<a = Sul : caminho' ((a-1),b) (c,d)
    |d>b = Este : caminho' (a,(b+1)) (c,d)
    |d<b = Oeste : caminho' (a,(b-1)) (c,d)
    |otherwise = []
--46
vertical' :: [Movimento] -> Bool
vertical' [] = True
vertical' (Norte:t) = vertical' (t)
vertical' (Sul:t) = vertical' (t)
vertical' (_:t) = False

--47
data Posicao = Pos Int Int deriving (Show)
maisCentral' :: [Posicao] -> Posicao
maisCentral' [a] = a
maisCentral' ((Pos a b):(Pos c d):t)
    |distCentral a b  > distCentral c d = maisCentral' ((Pos c d):t)
    |otherwise = maisCentral' ((Pos a b):t)

distCentral :: Int -> Int -> Float
distCentral a b = sqrt(fromIntegral(a^2+b^2))

--48
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos a [] = []
vizinhos (Pos a b) ((Pos c d):t)
    |a == (c+1) || a == (c-1) && b == (c+1) || b == (c-1) = (Pos c d):(vizinhos (Pos a b) (t))
    |otherwise = vizinhos (Pos a b) (t)

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos a b),(Pos c d)] 
    |a == c = True
    |otherwise = False
mesmaOrdenada ((Pos a b):(Pos c d):t)
    |a==b = mesmaOrdenada ((Pos c d):t)
    |otherwise = False

--50
data Semaforo = Verde | Amarelo | Vermelho deriving (Show)

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK (Verde:t) = interseccaoOK (t)
interseccaoOK (_:t) = False