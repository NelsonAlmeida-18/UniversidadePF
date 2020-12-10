import Data.List
import Data.Either

enumFromTo' :: Int -> Int->[Int]
enumFromTo' a b = (a):enumFromTo(a+1) b

myenumfromthento :: Int -> Int -> Int -> [Int]
myenumfromthento a b c
    |a<c && a<b && (a+(b-a))<=c = a:myenumfromthento b (b+(b-a)) c
    |a>c && a>b && (b-(b-a))>=c= a:myenumfromthento b (b-(b-a)) c
    |otherwise = [a]

maismais :: [a] -> [a] -> [a]
maismais [] a = a
maismais (h:t) a = h:maismais(t) a 

exclamacao :: [a] -> Int -> a
exclamacao (h:t) a 
     |a == 0 = h
     |otherwise = exclamacao(t) (a-1)

reverse' :: [a] -> [a] 
reverse' [] = []
reverse' a = (last(a)):reverse'(init(a)) 

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' a (h:t)=h:take'(a-1) (t)

drop' :: Int -> [a] -> [a]
drop' 0 b = b
drop' a (h:t) = drop' (a-1) (t) 

zip' :: [a] -> [b] -> [(a,b)]
zip' [] b = []
zip' a [] = []
zip' (h:t) (x:xs) = (h,x): zip' (t) (xs)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (h:t) 
    |a == h = True
    |otherwise = elem' a (t)

replicate' :: Int -> a -> [a]
replicate' 0 b = []
replicate' a b = b :replicate' (a-1) b

intersperce' :: a -> [a] -> [a]
intersperce' a [] = []
intersperce' a (h:t) = [h]++[a] ++ intersperce' a t

--está comprida porque não usei dropwhile
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

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = [head(h)]++tail(h)++concat' t  

inits' :: [a] -> [[a]]
inits' a = inits2' a 0

inits2' [] _ = []
inits2' a b 
    |b == length(a)+1 = []
    |otherwise = [] ++ [take' b a] ++ inits2' (a) (b+1)

tails' :: [a] -> [[a]]
tails' a = tails2' a (0)

--tails2' :: [a] -> Int -> [a]
tails2' a b 
    |b == length(a)+1 = []
    |otherwise = [drop' b a] ++ tails2' (a) (b+1)

isprefixof' :: Eq a => [a] -> [a] -> Bool
isprefixof' a b 
    |a == x = True
    |otherwise = False
    where x = take' (length(a)) (b)

issuffixof' :: Eq a => [a] -> [a] -> Bool
issuffixof' a b 
    |a == x = True
    |otherwise = False
    where x = drop' (length(b)-length(a)) (b)

issubsequenceof' :: Eq a => [a] -> [a] -> Bool
issubsequenceof' [] _ = True
issubsequenceof' _ [] = False
issubsequenceof' (h:t) (x:xs) 
    |h == x = issubsequenceof' t (xs)
    |otherwise = issubsequenceof' (h:t) (xs)

elemindices' :: Eq a => a-> [a] -> [Int] 
elemindices' a b = elemindices2' a b 0 0

elemindices2' _ [] _ _ = []
elemindices2' a (h:t) b c
    |a == h = []++[c] ++ elemindices2' a (t) c (c+1)
    |otherwise = elemindices2' a (t) b (c+1)


nub' :: Eq a => [a] -> [a] 
nub' [] = []
nub' (h:t) 
    |nubdetec' h t = nub' (t)
    |otherwise = h:nub' (t)

nubdetec' :: Eq a => a -> [a] -> Bool
nubdetec' _ [] = False
nubdetec' a (h:t)
    |a == h = True
    |otherwise = nubdetec' a t
delete' :: Eq a => a -> [a] -> [a]
delete' a (h:t) = delete2' a (h:t) []

delete2' :: Eq a => a -> [a] -> [a] ->[a]
delete2' a [] b = []
delete2' a (h:t) b
    |a == h = b++t
    |otherwise = delete2' (a) t (b++[h])

--fazer again, estou a complicar
slash' :: Eq a => [a] -> [a] -> [a]
slash' [] _ = []
slash' a [] = a
slash' (h:t) (x:xs) 
    |h==x = slash' t xs 
    |otherwise = h:slash' t xs

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) (x:xs)
    |intersectdetect' (h) (x:xs) = h:intersect' t (x:xs)
    |otherwise = intersect' (t) (x:xs)


intersectdetect' :: Eq a => a -> [a] -> Bool
intersectdetect' a [] = False
intersectdetect' a (h:t)
    |a == h = True
    |otherwise = intersectdetect' a t 

insert :: Ord a =>a -> [a] -> [a]
insert a [] = [a]
insert a (b:c:t)
    |a>=b && a<c = b:a:c:t
    |a<b = a:b:c:t
    |a == b = a:b:c:t
    |otherwise = b:(insert a (c:t))
insert _ _ = []



unwords' :: [String] -> String
unwords' [] = []
unwords' (h:t) = h ++ " " ++ unwords' t

unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t


pmaior' :: Ord a => [a] -> Int
pmaior' a = pmaior2' a (head(a)) 0

pmaior2' :: Ord a => [a] -> a -> Int-> Int
pmaior2' [] b c = c
pmaior2' (h:t) b c
    |h>b = pmaior2' t h (c+1)
    |otherwise = pmaior2' t b (c)

temrepetidos' :: Eq a => [a] -> Bool
temrepetidos' [] = False
temrepetidos' (h:t) 
    |temrepetidoscheck' (h) (t) = True
    |otherwise = temrepetidos' (t)

temrepetidoscheck' :: Eq a => a -> [a] -> Bool
temrepetidoscheck' a [] = False
temrepetidoscheck' a (h:t)
    |a == h = True
    |otherwise = temrepetidoscheck' a (t)

algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t)
    |h>= '0' && h<= '9' = h:algarismos' (t)
    |otherwise = algarismos' (t) 

--fazer de novo sem Eq
posImpares' :: Eq a => [a] -> [a]
posImpares' [] = []
posImpares' (h:t) 
    |length(t) == 2 = [] ++ [head(t)] 
    |otherwise = [head(t)] ++ posImpares' (tail(t))

posPares' ::  [a] -> [a]
posPares' [] = []
posPares' [a] = [a]
posPares' (h:t) = h:posPares' (tail(t))

isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' (h:t) 
    |isSorted2' (t) (h) = isSorted'(t)
    |otherwise = False

isSorted2' :: Ord a => [a] -> a -> Bool
isSorted2' [] a = True
isSorted2' (h:t) a 
    |a <= h = isSorted2' (t) a
    |otherwise = False


iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = iSort2' (h:t) []

iSort2' :: Ord a => [a] -> [a] -> [a]
iSort2' [] b = b
iSort2' (h:t) b = iSort2' t (insert h b)

menor' :: [a] -> [a] -> Bool
menor' [] [] = error "Não há listas menores se ambas são vazias"
menor' [] b = True
menor' a [] = False
menor' (h:t) (x:xs) = menor' (t) (xs)

elemMSet :: Eq a => a -> [(a, Int)] -> Bool
elemMSet c [] = True
elemMSet c ((a,b):t)
    |c == a = elemMSet c t
    |otherwise = False

lengthMSet :: Eq a => [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((a,b):t) = lengthMSet2 ((a,b):t) 0

lengthMSet2 :: Eq a => [(a,Int)] -> Int -> Int
lengthMSet2 [] b = b
lengthMSet2 ((a,b):t) c = lengthMSet2 (t) (c+b)

convertMSet :: [(a,Int)] -> [a]
convertMSet [] = []
convertMSet ((a,b):t)
    |b/=0 = a:convertMSet((a,(b-1)):t)  
    |otherwise = convertMSet(t)

insereMSet :: Eq a => a ->[(a,Int)] -> [(a,Int)]
insereMSet _ [] = []
insereMSet c ((a,b):t)
    |c == a = ((a,(b+1)):t)
    |otherwise = (a,b):insereMSet c (t)

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet c ((a,b):t)
    |c == a = ((a,(b-1)):t)
    |otherwise = (a,b) : removeMSet c (t)

--42
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Right a):t) = (c,a:d)
    where (c,d) = partitionEithers' (t)
partitionEithers' ((Left a):t) = (a:c,d)
    where (c,d) = partitionEithers' (t)

--43


--44
data Movimento = Norte | Sul | Este | Oeste

posicao :: (Int, Int) -> [Movimento] -> (Int,Int)
posicao a [] = a
posicao (x,y) (Norte:t) = posicao (x,(y+1)) t
posicao (x,y) (Sul:t) = posicao (x,(y-1)) t
posicao (x,y) (Oeste:t) = posicao ((x-1),y) t
posicao (x,y) (Este:t) = posicao ((x+1),y) t

vertical :: [Movimento] -> Bool
vertical [] = False
vertical (Norte:t) = True
vertical (Sul:t) = True
vertical(Este:t) = vertical(t)
vertical(Oeste:t) = vertical(t)

data Posicao = Pos Int Int deriving(Show)

maisCentral :: [Posicao] -> Posicao
maisCentral [] = error "Por favor insere uma Posicao"
maisCentral ((Pos 0 0):t) = (Pos 0 0)
maisCentral a = maisCentral2 a (head(a)) 

maisCentral2 :: [Posicao] -> Posicao -> Posicao
maisCentral2 [] b = b
maisCentral2 ((Pos a b):t) (Pos e f)
    |(dist' a b)>=(dist' e f) = maisCentral2 t (Pos e f)
    |otherwise = maisCentral2 t (Pos a b)

dist' :: Int -> Int -> Float
dist' a b = sqrt(fromIntegral((a^2)+(b^2)))

vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos a b) [] = []
vizinhos (Pos a b) ((Pos c d):t) 
    |(abs(c-a)) == 1 || (abs(d-b)) == 1 = [Pos c d] ++ (vizinhos (Pos a b) (t))
    |otherwise = vizinhos (Pos a b) (t)

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada ((Pos a b):(Pos c d):t)
    |(b==d) = mesmaOrdenada ((Pos a b):t)
    |otherwise = False
mesmaOrdenada _ = True

data Semaforo = Verde | Vermelho | Amarelo deriving(Show)
intersecaoOk :: [Semaforo] -> Bool
intersecaoOk (h:t) = case h of 
	                    (Vermelho) -> False
	                    (Verde) -> intersecaoOk (t)
	                    (Amarelo) -> intersecaoOk (t)
intersecaoOk _ = True
