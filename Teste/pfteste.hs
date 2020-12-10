import Data.List


constroimset' :: Ord a => [a] -> [(a,Int)]
constroimset' [] =[]
constroimset' (h:t) = (h,a) : constroimset' (b)
    where (a,b) = aux h t (1)
          aux :: Ord a => a -> [a] -> Int-> (Int,[a])
          aux a [] c = (c,[])
          aux a (h:t) c 
              |a == h = aux a (t) (c+1)
              |otherwise = (c,(h:t))

data Posicao = Pos Int Int deriving(Show)
maisCentral :: [Posicao] -> Posicao
maisCentral [] = error "lista vazia"
maisCentral [a] = a
maisCentral ((Pos a b): (Pos c d):t)
    |(a*a+b*b)<=(c*c+d*d) = maisCentral ((Pos a b):t)
    |otherwise = maisCentral ((Pos c d):t) 

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = error ("Tem de existir uma ou mais posicoes")
mesmaOrdenada [a] = True
mesmaOrdenada ((Pos a b):(Pos _ d):t)
    |b ==d = mesmaOrdenada ((Pos a b):t)
    |otherwise = False

zip' :: [a] -> [b] -> [(a,b)]
zip' (h:t) (x:xs) = (h,x) : zip' (t) (xs)
zip' _ _ = []

posPares :: [a]-> [a]
posPares [] = []
posPares [a] = [a]
posPares [a,b] = [a]
posPares [a,b,c] = [a,c]
posPares (h:c:t) = h:posPares (t)

posImpares :: [a] -> [a]
posImpares [a,c] = [c]
posImpares (h:c:t) = c:posImpares(t)
posImpares _ = []


take' :: Int -> [a] -> [a]
take' 0 l = []
take' a (h:t) = h: take'(a-1) (t)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort l = aux (l) ([])
    where aux :: Ord a => [a] -> [a] -> [a]
          aux [] l = l
          aux (h:t) l = aux t (insert h l)


iSort2 :: Ord a => [a] -> [a]
iSort2 [] = []
iSort2 l = aux l ([])
    where aux :: Ord a => [a] -> [a] -> [a]
          aux [] l = l
          aux (h:t) l = aux t (insere h l)

insere :: Ord a => a -> [a] -> [a]
insere a [] = [a]
insere a (h:t)
    |a<=h = a:h:t
    |otherwise = h:insere a (t)