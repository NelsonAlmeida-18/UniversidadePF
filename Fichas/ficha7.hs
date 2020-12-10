data ExpInt = Const Int | Simetrico ExpInt | Mais ExpInt ExpInt | Menos ExpInt ExpInt | Mult ExpInt ExpInt

--(1)
--(a)
calcula :: ExpInt -> Int
calcula a = case a of
    Const a -> a
    Simetrico a -> -(calcula a)
    Mais a b -> calcula a + calcula b
    Menos a b -> calcula a - calcula b
    Mult a b -> calcula a * calcula b

--(b)
infixa :: ExpInt -> String
infixa a = case a of
    Const b -> show(b)
    Mais b c -> infixa b ++ "+" ++ infixa c
    Menos b c -> infixa b ++ "-" ++ infixa c
    Simetrico b -> "-(" ++ infixa b ++ ")"
    Mult b c -> infixa b ++ "*" ++ infixa c

--(c)
posfixa :: ExpInt -> String
posfixa a = case a of
    Const b -> show(b)
    Mais b c-> posfixa b ++ posfixa c ++ "+"
    Menos b c-> posfixa b ++ posfixa c ++ "-"
    Simetrico b-> "(" ++ posfixa b  ++ ")-"
    Mult b c-> posfixa b ++ posfixa c ++ "*"

--2
data RTree a = R a [RTree a]

--(a)
soma :: Num a => RTree a -> a
soma (R a []) = a
--soma (R a b) = a + map soma b

--(b)
altura :: RTree a -> Int
altura (R a []) = 1
altura (R a b) = 1 + maximum(map altura b)

--(c)
prune :: Int -> RTree a -> RTree a 
prune 1 (R a _) = R a []
--prune a (R n b) = R n [prune (a-1) b]
--(d)
mirror :: RTree a -> RTree a
mirror (R a []) = R a []

--(e)
--postorder :: RTree a -> [a]

--3
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--(a)
ltSum :: Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork a b) = ltSum a + ltSum b

--(b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork a b) = listaLT a ++ listaLT b

--(c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 1
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

--(4)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--(a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b) = (Empty, Tip b)
splitFTree (No a b c) = (Node a x y,Fork x' y')
    where (x,x') = splitFTree b
          (y,y') = splitFTree c

--(b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees  _ _ = Nothing