{-
--1
(a)
[x | x<- [1..20], mod x 2 ==0, mod x 3 == 0]
--[6,12,18]
(b)
[x | x<- [y|y<-[1..20], mod y 2 ==0, mod x 3 ==0]]
--se for mod y 3 então é igual à anterior, caso contrário não sei
(c)
[(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)
(d)
--pensar de novo
[sum [y | y <- [1..x], odd y] | x <- [1..10]]
sum(1,3,5,7,9) = 25
-}

--2
--(a)
--rever listas de compreensaão

--3
digitAlpha :: String -> (String,String)
digitAlpha (h:t)
    |h>='0' && h<='9' = (a,h:b)
    |otherwise = (h:a,b)
    where (a,b) = digitAlpha(t)
digitAlpha _ = ("","")

--percorre apenas uma vez?
nzp :: [Int] -> (Int,Int,Int)
nzp (h:t)
    |h>0 = (a,b,c+1)
    |h<0 = (a+1,b,c)
    |otherwise = (a,b+1,c)
    where (a,b,c) = nzp (t)
nzp _ = (0,0,0)


--resto = if  (a-b)<=0 
--divisão = 
divMod' :: Integral a => a -> a -> (a,a)
divMod' a b
    |a<0 = (0,0)
    |(a-b)<=0 = (x,(y+a))
    |otherwise = ((x+1),y)
    where (x,y) = divMod' (a-b) (b) 

fromDigits :: [Int] -> Int
fromDigits l = fromDigits' (l) (length(l)-1)

fromDigits' [] a = 0
fromDigits' (h:t) a = h*(10^a) + fromDigits' (t) (a-1)

--7 não percebo/demasiado cansado para pensar

--8 não percebo