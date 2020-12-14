--(1)
--(a)
unlines' :: [String] -> String
unlines' [] = []
unlines' [a] = a
unlines' (h:t) = h ++ "\n" ++ unlines' (t)

--(b)
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) a [] = a
(\\) [] a = []
(\\) (h:t) (a:b)
    |h == a = (\\) t b 
    |otherwise = (\\) t (a:b)

--(2)
data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a
--(a)
primeiro :: Seq a -> a
primeiro (Inicio a _) =a
primeiro (Fim _ a) = a
--(b)
semUltimo :: Seq a -> Seq a 
semUltimo (Inicio a (Nil)) = Nil
semUltimo (Fim (Nil) a) = Nil
semUltimo (Inicio a b) = Inicio a (semUltimo b)
semUltimo (Fim (a) b) = (Fim (semUltimo a) b) 
--(3)
data BTree a = Empty | Node a (BTree a) (BTree a)
--(a)
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune a (Node b e d) = Node b (prune (a-1) e) (prune (a-1) d)
--(b)
semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo (Empty) = Empty
semMinimo (Node a Empty d) = Node a Empty Empty
semMinimo (Node a e d) = Node a (semMinimo e) (semMinimo d)
--(4)
type Tabuleiro = [String]
exemplo :: Tabuleiro
exemplo = ["R...","R...","R...","R..."]
--(a)
posicoes :: Tabuleiro -> [(Int,Int)]
posicoes [] = []
posicoes (h:t) = (aux(h),length(h)-length(h:t)):posicoes t
    where
        aux :: String -> Int
        aux (h:t)
              |h == 'R' = 0
              |otherwise = 1 + aux (t)
--(b)-- refazer valido
valido :: Tabuleiro -> Bool 
valido x = validoAux x
valido _ = False

validoAux [] = False
validoAux ([]:t) = validoAux t
validoAux (h:t)
    |(y == (y'+1) && (x == (x'+1) || x == (x'+(-1)))) || (y == (y'+(-1)) && (x == (x'+1) || x == (x'+(-1)))) = False
    |otherwise = validoAux (t)
    where p = posicoes (h:t) 
          x = fst(head(p))
          y = snd(head(p))
          x' = fst(head(tail(p)))
          y' = snd(head(tail(p)))

--(c)
bemFormado :: Int -> Tabuleiro -> Bool
bemFormado a b
    |a == length(b) && a == length(head(b)) && a == fst(aux b) = True
    |otherwise = False
    where aux :: Tabuleiro -> (Int,Int)
          aux [] = (0,0)
          aux ([]:t) = (x,y)
              where (x,y) = aux(t)
          aux (h:t)
              |head(h)== 'R' =  (x+1,y)
              |head(h) == '.' = (x,y+1)
              |head(h) /= '.' || head(h) /= 'R' = ((-1),(-1))
              |otherwise = (x,y)
              where (x,y) = aux(tail(h):t)
