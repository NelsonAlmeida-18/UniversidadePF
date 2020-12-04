{-
--1
--(a)
[x | x<- [1..20], mod x 2 ==0, mod x 3 == 0]
--[6,12,18]
--(b)
[x | x<- [y|y<-[1..20], mod y 2 ==0, mod x 3 ==0]]
--se for mod y 3 então é igual à anterior, caso contrário não sei
--(c)
[(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
--(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)

--2
--(a)
[2^x|x<-[0..10]]
--(b)
[(x,y)|x<-[1..5],y<-[1..5], x+y==6]
[(x,6-x)|x<-[1..5]]
--(c)
[[x|x<-[1..y]] |y<-[1..5]]
--d
[[1|x<-[1..y]] | y<-[1..5]]
--e
[fact(x)|x<-[1..6]]
fact 0 = 1
fact x = x*(fact(x-1))
--}
--3
digitAlpha :: String -> (String,String)
digitAlpha (h:t)
    |h>='0' && h<='9' = (a,h:b)
    |h<='z' && h>='a' || h<='Z' && h>= 'A' = (h:a,b)
    |otherwise = (a,b)
    where (a,b) = digitAlpha(t)
digitAlpha _ = ("","")

nzp :: [Int] -> (Int,Int,Int)
nzp (h:t)
    |h>0 = (a,b,c+1)
    |h<0 = (a+1,b,c)
    |otherwise = (a,b+1,c)
    where (a,b,c) = nzp (t)
nzp _ = (0,0,0)


divMod' :: Integral a => a -> a -> (a,a)
divMod' a 0 = error "Divisão por 0"
divMod' a b
    |a<0 && b>0 || a>0 && b<0 = (-res,resp)
    |(a-b)<=0 = (x,(y+a))
    |otherwise = ((x+1),y)
    where (x,y) = (divMod' (a-b) (b) )
          (res,resp) = (divMod' (abs a) (abs b))


fromDigits' :: [Int] -> Int
fromDigits' [] = 0
fromDigits' l = last(l)+10* fromDigits' (init(l))

--7 
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (h:t) = maxSumInit' (t) (h) (h)

maxSumInit' [] _ max = max
maxSumInit' (h:t) total max
    |max>(total+h) = maxSumInit' (t) (total+h) (max)
    |otherwise = maxSumInit' (t) (total+h) (total+h)

--8 
fibss :: Int -> Int
fibss n = acc n (0,1)
    where 
        acc 0 (a,c) = a
        acc 1 (a,c) = c
        acc n (a,c) = acc (n-1) (c,a+c)