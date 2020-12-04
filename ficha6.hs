data BTree a = Empty | Node a (BTree a) (BTree a) deriving(Show)

btreeteste1 :: BTree Int
btreeteste1 = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 3 Empty Empty))(Node 20 (Node 30  Empty Empty) (Node 30 Empty Empty)))
--------------------------------------------------------------------------
altura :: BTree a -> Int
altura (Empty) = 0
altura (Node a b c) = 1 + max (altura b) (altura c)
--------------------------------------------------------------------------
nodos :: BTree a -> Int
nodos (Empty) = 0
nodos (Node a b c) = 1 + nodos(b) + nodos(c)
--------------------------------------------------------------------------
--como assim sem descentes?
folhas :: BTree a -> Int
folhas (Empty) = 0
folhas (Node a Empty b) = 1 + folhas b
folhas (Node a b Empty) = 1 + folhas b
--------------------------------------------------------------------------
prune :: Int -> BTree a -> BTree a
prune 1 (Node a e d) = (Node a Empty Empty)
prune _ Empty = Empty
prune a (Node b c d) = (Node b (prune (a-1) c) (prune(a-1) d)) 
--------------------------------------------------------------------------
path :: [Bool] -> BTree a -> [a]
path [] a = []
path (_:t) (Empty) =[]
path ((True):t) (Node a b c) = a : path (t) (c) 
path ((False):t) (Node a b c) = a : path (t) (b)
--------------------------------------------------------------------------
mirror :: BTree a -> BTree a
mirror (Empty) = Empty
mirror (Node a b c) = Node a c b
--------------------------------------------------------------------------
zipWithBT :: (a->b->c) -> BTree a -> BTree b-> BTree c
zipWithBT g l Empty = Empty
zipWithBT g Empty l = Empty
zipWithBT g (Node a b c) (Node d e f) = (Node (g a d) (zipWithBT g b e) (zipWithBT g c f))
--------------------------------------------------------------------------
--btree para testar a função unzipBT
btreeteste2 :: BTree (Char,Int,Float)
btreeteste2 = (Node ('a',1,1.0) (Node ('b',2,2.0) (Node ('c',3,3.0) Empty Empty) (Node ('c',3,3.0) Empty Empty))(Node ('x',20,20.0) (Node ('y',30,30.0)  Empty Empty) (Node ('y',30,30.0) Empty Empty)))

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b, BTree c)
unzipBT (Empty) = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e f) = (Node a x k,Node b y l,Node c z m)
    where (x,y,z) = unzipBT e
          (k,l,m) = unzipBT f
--------------------------------------------------------------------------
--(2)

minimo :: Ord a => BTree a -> BTree a
minimo (Node a Empty Empty) = (Node a Empty Empty)
minimo (Node a b c) = minimo (b)
--------------------------------------------------------------------------
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node a Empty Empty) = (Empty)
semMinimo (Node a e Empty) = (Node a (semMinimo e) Empty)
semMinimo (Node a e d) = (Node a (semMinimo e) d)
--------------------------------------------------------------------------
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node a Empty Empty) = (a, Empty)
minSmin (Node a e d) = (c, Node a b Empty)
    where (c,b) = minSmin (e)
--------------------------------------------------------------------------
--não usei a função anterior, como faço com a anterior?
remove :: Ord a => a -> BTree a -> BTree a
remove a (Empty) = Empty
remove a (Node n e d)
    |a <=n = b
    |a>= n = c
    |otherwise =(Node n (remove a e) (remove a d))
    where b = (remove a e) 
          c = (remove a d)
--------------------------------------------------------------------------
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving(Show)
data Classificacao = Aprov Int | Rep | Faltou deriving(Show)
type Turma = BTree Aluno
--------------------------------------------------------------------------

inscNum :: Numero -> Turma -> Bool
inscNum a Empty = False
inscNum a (Node (b,_,_,_) e d)
    |a == b = True
    |otherwise = (inscNum a e) || (inscNum a d)
--------------------------------------------------------------------------
trabEst :: Turma -> [(Numero,Nome)]
trabEst (Empty) = []
trabEst (Node (a,b,TE,_) e d) = (a,b) : trabEst (e) ++ trabEst (d)
trabEst (Node a b c) = trabEst (b) ++ trabEst(c)
--------------------------------------------------------------------------
nota :: Numero -> Turma -> Maybe Classificacao
nota a Empty = Nothing
nota a (Node (b,_,_,c) e d)
    |a<b = nota a e
    |a>b = nota a d
    |a == b = Just c 
--------------------------------------------------------------------------
percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas (Node (_,_,_,Faltou) e d) = (1+percFaltas e + percFaltas d)/100
percFaltas (Node a e d) = (percFaltas e) + (percFaltas d)

--------------------------------------------------------------------------
mediaAprov :: Turma -> Float
mediaAprov Empty = 0
mediaAprov (Node a e d) = fst(x)/snd(x)
    where x= mediaAprovaux (Node a e d)

mediaAprovaux ::Turma -> (Float,Float)
mediaAprovaux (Empty)= (0,0)
mediaAprovaux (Node (_,_,_, Aprov l) e d)= (a+p,b+1)
    where p = fromIntegral(l)
          (a,b) = somaXPTO (mediaAprovaux (e)) (aprovAvaux (d))
mediaAprovaux (Node _ e d) = (a,b+1)
    where (a,b) = somaXPTO (mediaAprovaux (e)) (aprovAvaux (d))

somaXPTO ::(Num a, Num b) =>(a,b) -> (a,b) -> (a,b)
somaXPTO (a,b) (c,d) = ((a+c),(b+d))

--------------------------------------------------------------------------
--btree para testar algumas funçoes
btreeTeste ::  Turma
btreeTeste = (Node (1,"teste1",TE, Aprov 1) (Node (2, "teste2", TE, Rep) (Node (3,"Teste3", TE, Aprov 3) Empty Empty) Empty)Empty)
--------------------------------------------------------------------------
aprovAv :: Turma -> Float 
aprovAv (Empty) = 0
aprovAv (Node a e d) = (fst(g) /snd(g))
    where g = aprovAvaux (Node a e d)

aprovAvaux :: Turma -> (Float,Float)
aprovAvaux Empty = (0,0)
aprovAvaux (Node (_,_,_,Aprov _) e d) = ((p+1),(r+1))
    where (p,r) = somal (aprovAvaux (e)) (aprovAvaux (d))
aprovAvaux (Node (_,_,_,Rep) e d) = (p,(r+1))
    where (p,r) = somal (aprovAvaux (e)) (aprovAvaux (d))
aprovAvaux (Node a e d) = (p,r)
    where (p,r) = somal (aprovAvaux (e)) (aprovAvaux (d))

somal :: (Float,Float) -> (Float,Float) -> (Float,Float)
somal (a,b) (c,d)= (a+c,b+d) 
--------------------------------------------------------------------------