import System.IO

--(1)
puntopunto :: [a] -> Int -> a
puntopunto [] a = error("Fora da lista")
puntopunto (h:t) 0 = h
puntopunto (h:t) a = puntopunto (t) (a-1)

--(2)
data Movimento = Norte | Sul | Este | Oeste deriving(Show)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t)= case h of 
                         Norte -> posicao (x,y+1) (t)
                         Sul -> posicao (x,y-1) (t)
                         Este -> posicao (x+1,y) (t)
                         Oeste -> posicao (x-1,y) (t)
--(3)
any' :: (a -> Bool) -> [a] -> Bool
any' f (h:t)
    |f h = any' f t
    |otherwise = False
any' _ _ = True 

--(4)
type Mat a = [[a]]
triSup :: (Num a,Eq a) => Mat a -> Bool 
triSup (h:t) = triSupAux (h:t) (0)

triSupAux :: (Num a, Eq a) => Mat a -> Int -> Bool
triSupAux (h:t) n 
    |aux h (0) == n = triSupAux t (n+1)
    |otherwise = False
triSupAux _ _ = True
aux :: (Num a, Eq a) => [a] -> Int -> Int
aux [] p = p
aux (h:t) (p)
    |h == 0 = aux(t) (p+1)
    |otherwise = aux (t) (p)
--(5)

movimenta :: IO (Int,Int)
movimenta = movimenta' (0,0)
    where movimenta' (x,y) =do
              a<-getChar
              if a == 'N' then (x,y+1) else (x,y)

--(6)
data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]
ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5),Quadrado 4,Mover (4,3) (Quadrado 2)])
--(a)
vazia :: Imagem -> Bool
vazia (Quadrado b) =False
vazia (Mover a b) = vazia b
--vazia (Juntar a) = vazia a
--(b)
maior :: Imagem -> Maybe Int
maior (Quadrado a) = Just a
maior (Mover a b) = maior b
--maior (Juntar a) = maior a