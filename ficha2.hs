type Polinomio = [Monomio]
type Monomio = (Float,Int)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (h:t) = a:normaliza (b)
    where (a,b) =(normalizaAux (h) t  [])

normalizaAux :: Monomio -> Polinomio -> Polinomio -> (Monomio,Polinomio)
normalizaAux (a,b) [] l = ((a,b),l)
normalizaAux (a,b) ((c,d):t) l
    |b == d = normalizaAux (a+c,b) (t) l
    |otherwise = normalizaAux (a,b) t ((c,d):l)