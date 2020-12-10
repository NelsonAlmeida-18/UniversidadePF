data Hora = H Int Int deriving(Show)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

testeetapa :: Etapa -> Bool
testeetapa ((H a b),(H c d))
    |a>24 || a<0 || c>24 || c<0 || b>60 || b<0 = False 
    |a<c = True
    |a==c && b<d = True
    |otherwise = False

testeViagem :: Viagem -> Bool
testeViagem ((a,b):(c,d):t)
    |testeetapa(a,b) && testeetapa(b,c) = testeViagem((c,d):t)
    |otherwise = False
testeViagem _ =True

--2
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
type Poligonal = [Ponto]

mover :: Poligonal -> Ponto -> Poligonal
mover [] a = a
mover [b] a = a
--mover [(Cartesiano a b),(Cartesiano c d)] (Cartesiano e f) = [(Cartesiano e f),(Cartesiano (c+e-a) (d+f-a))]
--mover ((Cartesiano a b):(Cartesiano e f):t) (Cartesiano c d) = (Cartesiano (c) (d)) : mover ((Cartesiano e f):t) (Cartesiano (e+(c-a)) (f+(d-b)))
polar2cart (Cartesiano a b) = (a,b)
polar2cart (Polar a b) = ((cos(b)*a),sin(b)*a)


data Contacto = Casa Int | Trab Int | Tlm Int | Email String deriving(Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]


--duvida se posso fazer assim
type Contactos = [Contacto]
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e [] = [(n,[Email e])]
acrescEmail n e ((x,l):t)
  | n == x && acrescEmail' e l = error "Esse email já existe na lista de contactos"
  |n == x = (x, (Email e):l) : t
  | otherwise = (x,l) : acrescEmail n e t

acrescEmail' :: String -> Contactos -> Bool
acrescEmail' a ((Email b):t)
    |a == b = True
    |otherwise = acrescEmail' a (t)
acrescEmail' a ((b):t) = acrescEmail' a (t)
acrescEmail' a [] = False 

--Output duvida com Maybe [String]
--verEmails :: Nome -> Agenda -> Maybe [String]
verEmails a ((x,l):t)
    | a == x = verEmails' a l
    |otherwise = verEmails a t

verEmails' :: String -> Contactos -> [String]
verEmails' a ((Email b):t) = b: (verEmails' a t)
verEmails' a ((_):t) = verEmails' a t
verEmails' _ _ = []


consTelef :: Contactos -> [Integer]
consTelef [] = []
consTelef ((Tlm b):t) = fromIntegral(b):consTelef(t)
consTelef ((Trab b):t) = fromIntegral(b):consTelef(t)
consTelef ((Casa b):t) = fromIntegral(b):consTelef(t)
consTelef ((Email b):t) = consTelef(t)

--casa :: Nome -> Agenda -> Maybe Integer
casa :: Nome -> Agenda -> Integer
casa a [] = error "Ñ existem contactos"
casa a ((n,b):t)
    |a == n = casa' b
    |otherwise = casa a (t)

casa' ((Casa b):cs) = fromIntegral(b)
casa' ((_):cs) = casa' (cs)

type Dia = Int
type Mes = Int
type Ano = Int
type Nome' = String

data Data = D Dia Mes Ano deriving(Show)

type TabDN = [(Nome',Data)]


--procura :: Nome -> TabDN -> Maybe Data
procura :: Nome' -> TabDN-> Data
procura a [] = error "ñ existem datas"
procura a ((b,c):t)
    |a == b = c
    |otherwise = procura a t

--idade :: Data -> Nome -> TabDN -> Maybe Int
idade :: Data -> Nome -> TabDN -> Int
idade a b [] = 0
idade a b ((c,d):t)
    |b == c = idadeCalc a d
    |otherwise = idade a b (t)

idadeCalc :: Data -> Data -> Int
idadeCalc (D a b c) (D d e f)
    |b> e || b== e && a>=d= c-f
    |otherwise = (c-f-1) 

anterior :: Data -> Data -> Bool
anterior (D a b c) (D d e f)
    |c<f = True
    |c==f && b<e || c==f && a<d = True
    |otherwise = False 
--fazer later
--ordena :: TabDN -> TabDN 
--ordena ((x,(D a b c)):(y,(D d e f):t)

--later
--porIdade :: Data ->TabDN -> [(Nome,Int)] 


data Movimento = Credito Float | Debito Float deriving(Show)
data Extrato = Ext Float [(Data, String, Movimento)]  deriving(Show)

extValor :: Extrato -> Float -> [Movimento]
extValor (Ext a []) e = []
extValor (Ext a ((b,c,(Credito d)):t)) e
    |d>=e = (Credito d):((extValor (Ext a (t))) e)
    |otherwise = extValor (Ext a (t)) e
extValor (Ext a ((b,c,(Debito d)):t)) e
    |d>=e = (Debito d):((extValor (Ext a (t))) e)
    |otherwise = extValor (Ext a (t)) e

filtro :: Extrato -> String -> [(Data,Movimento)]
filtro (Ext b []) a = []
filtro (Ext a ((b,c,d):t)) e
    |c==e =  (b,d):(filtro (Ext a (t)) e)
    |otherwise =  (filtro (Ext a (t)) e)

myunwords :: [String] -> String
myunwords [x] = x
myunwords (h:t) = h ++ " " ++ myunwords (t)

creDeb :: Extrato -> (Float,Float)
creDeb (Ext a ((_,_,(Credito b)):t)) = (b+x,y)
    where (x,y) = creDeb (Ext a (t))
creDeb (Ext a ((_,_,(Debito b)):t)) = (x,b+y)
   where (x,y) = creDeb (Ext a (t))
creDeb _ = (0,0)

--x = credito
--y = debito
saldo :: Extrato -> Float
saldo (Ext a b) = (a-x+y)
    where (x,y) = creDeb (Ext a b)

enumFromThenToo :: Int -> Int -> Int -> [Int]
enumFromThenToo a b c 
    | a < b && a > c || a > b && a < c = []
    |otherwise = a: enumFromThenToo b ((b-a)+ b) c

    