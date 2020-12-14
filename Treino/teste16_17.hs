--(1)
type MSet a = [(a,Int)]
--(a)
cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet ((a,b):t) = b+cardMSet t
--(b)
--moda :: MSet a -> [a]
--(c)
converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet (t)
converteMSet ((a,b):t) = a:converteMSet ((a,(b-1)):t)
--(d)
--addNcopies :: Eq a => MSet a -> a -> Int -> MSet a 

--(2)
data SReais = AA Double Double | FF Double Double | AF Double Double | FA Double Double | Uniao SReais SReais
--(a)

--(b)
pertence :: Double -> SReais -> Bool
pertence a (Uniao b c) = pertence a b || pertence a c
pertence a (FF b c)  |a<=c && a>=b = True |otherwise = False
pertence a (FA b c)  |a>=b && a<c = True |otherwise = False
pertence a (AF b c)  |a>b && a<= c = True |otherwise = False
pertence a (AA b c)  |a>b && a <c = True |otherwise = False

--(c)--testar estas funcoes
tira :: Double -> SReais -> SReais
tira a (FF b c) |a<=c && a>=b = Uniao (FF b a) (FF a c) |otherwise = error ("o numero nao pertence ao intervalo")
tira a (FA b c) |a == c = error ("o numero nao pertence ao conjunto")|a<c && a>=b = Uniao (FF b a) (FA a c) |otherwise = error (" o numero nao pertence ao intervalo")   
tira a (AF b c) |a == b = error ("o numero nao pertence ao conjunto") | a <=c && a>b = Uniao (AF b a) (FF a c) |otherwise = error ("o numero nao pertence ao conjunto") 
tira a (AA b c) |a == b || a == c = error ("o numero nao pertence ao conjunto") | a<c && a >b = Uniao (AF b a) (FA a c) |otherwise = error("o numero nao pertence ao conjunto")

--(3)
data RTree a = R a [RTree a]
--(a)--acabar
percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] a = Nothing
percorre (h:t)(R a b)

