import Data.Char
--------------------------------------------------------------------------
dobros :: [Float]-> [Float]
dobros [] = []
dobros (h:t) = (h*2): dobros (t)
dobros' l = map (*2) l
--------------------------------------------------------------------------

numOcorre :: Char -> String -> Int
numOcorre a [] = 0
numOcorre a (h:t)
    |a == h = 1+numOcorre a (t)
    |otherwise = numOcorre a t
--------------------------------------------------------------------------
positivos :: [Int] -> Bool
positivos (h:t)
    |h>=0 = positivos t
    |otherwise = False
positivos _ = True
--------------------------------------------------------------------------
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
    |h<= 0 = soPos t
    |otherwise = h:soPos(t)
--------------------------------------------------------------------------
somaNeg :: [Int]-> Int
somaNeg [] = 0
somaNeg (h:t)
    |h<=0 = h+somaNeg (t)
    |otherwise = somaNeg (t)
--------------------------------------------------------------------------
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l
    |length(l) <=3 = l
    |otherwise = tresUlt (tail(l))
--------------------------------------------------------------------------
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b:segundos(t)
--------------------------------------------------------------------------
nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros a [] = False
nosPrimeiros a ((b,c):t)
    |a == b = True
    |otherwise = nosPrimeiros a (t)
--------------------------------------------------------------------------
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (x+a,y+b,z+c)
    where (x,y,z) = sumTriplos (t)
--------------------------------------------------------------------------
--3
--------------------------------------------------------------------------
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos l = filter (isDigit) l
--------------------------------------------------------------------------
minusculas :: [Char] -> [Char]
minusculas [] = []
minusculas l = filter (isAsciiLower) l
--------------------------------------------------------------------------
nums :: String -> [Int]
nums [] = []
nums (h:t)
    |ord(h)>=48 && ord(h)<=57 = (ord(h)-48):nums(t)
    |otherwise = nums(t)  
--------------------------------------------------------------------------
--(4)
--------------------------------------------------------------------------
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--------------------------------------------------------------------------
conta :: Int -> Polinomio -> Int 
conta a [] = 0
conta a ((b,c):t)
    |a ==c = 1+conta a t
    |otherwise = conta a t
--------------------------------------------------------------------------
grau :: Polinomio -> Int
grau [] = 0
grau (l) = maximum([snd(x)|x<-l])

grauRecursivo :: Polinomio -> Int
grauRecursivo [] = 0
grauRecursivo [(a,b)] = b
grauRecursivo ((a,b):(c,d):t)
    |b>= d = grauRecursivo ((a,b):t)
    |otherwise = grauRecursivo ((c,d):t)
--------------------------------------------------------------------------
selGrau :: Int -> Polinomio -> Polinomio
selGrau a [] = []
selGrau a l = filter (\x-> snd(x)==a) l 

selGrauRecursivo :: Int -> Polinomio -> Polinomio
selGrauRecursivo a [] = []
selGrauRecursivo a ((b,c):t)
    |a ==c = (b,c): selGrauRecursivo a (t)
    |otherwise = selGrauRecursivo a (t)
--------------------------------------------------------------------------
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((a,b):t) = (c,b-1) : deriv (t)
    where c = fromIntegral(b)*a
--------------------------------------------------------------------------
calcula :: Float -> Polinomio -> Float
calcula a [] = 0
calcula a ((b,c):t) = d + calcula a (t)
    where d = b*(a^c)
--------------------------------------------------------------------------
simpRecursiva :: Polinomio -> Polinomio
simpRecursiva [] = []
simpRecursiva ((a,b):t)
    |a == 0 = simpRecursiva (t)
    |otherwise = (a,b):t

simp' :: Polinomio -> Polinomio
simp' [] = []
simp' l = filter (\x -> fst(x) /= 0) l
--------------------------------------------------------------------------
multRecursiva :: Monomio -> Polinomio -> Polinomio
multRecursiva a [] = []
multRecursiva a ((b,c):t) = (d,e):multRecursiva a t
    where d = fst(a)*b
          e = snd(a)*c
--------------------------------------------------------------------------
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = a:normaliza (b)
    where (a,b) =(normalizaAux (h) t  [])

normalizaAux :: Monomio -> Polinomio -> Polinomio -> (Monomio,Polinomio)
normalizaAux (a,b) [] l = ((a,b),l)
normalizaAux (a,b) ((c,d):t) l
    |b == d = normalizaAux (a+c,b) (t) l
    |otherwise = normalizaAux (a,b) t ((c,d):l)
--------------------------------------------------------------------------
soma :: Polinomio -> Polinomio ->Polinomio
soma [] a = a
soma a [] = a
soma a b = (x,y) : soma (tail(a)) (tail(b))
    where (x,y) = (fst(head(a))+fst(head(b)),snd(head(a))+snd(head(b)))
--------------------------------------------------------------------------
produto :: Polinomio-> Polinomio -> Polinomio
produto ((a,b):t) c = somaXPTO (x) (produto (t) c)
    where x = (produtoAux(a,b) c)
produto _ _ = []

produtoAux :: Monomio -> Polinomio -> Polinomio
produtoAux (a,b) ((c,d):t)= (a*c,b+d) : produtoAux (a,b) (t) 
produtoAux _ _ =[]

somaXPTO :: Polinomio -> Polinomio -> Polinomio
somaXPTO [] a = a
somaXPTO (h:t) a = somaXPTO t (h:a)
--------------------------------------------------------------------------
ordena :: Polinomio -> Polinomio
ordena l = insereXPTO l []

insereXPTO :: Ord b => [(a,b)] -> [(a,b)] -> [(a,b)]
insereXPTO [(a,b)] [] = [(a,b)]
insereXPTO ((a,b):t) [] = insereXPTO (t) [(a,b)]
insereXPTO ((a,b):t) ((c,d):l)
    |b<=d = insereXPTO t ((a,b):(c,d):l)
    |otherwise = (c,d):insereXPTO ((a,b):t) (l)
insereXPTO _ a = a
--------------------------------------------------------------------------
equiv :: Polinomio -> Polinomio -> Bool
equiv a b = normaliza (a) == normaliza b
