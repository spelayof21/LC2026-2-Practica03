module Practica03 where


--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn (Not (Cons a)) = Cons (not a)
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Not p)) =  fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = And (fnn p) (fnn (Not q))
fnn (Not (Syss p q)) = Or (And (fnn p) (fnn (Not q))) (And (fnn q) (fnn (Not p)))
fnn (Cons a) = Cons a
fnn (Var p) = Var p
fnn (Not p) = Not (fnn p)
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
fnn (Impl p q) = Or (fnn (Not p)) (fnn q)
fnn (Syss p q) = And (Or (fnn (Not p)) (fnn q)) (Or (fnn (Not q)) (fnn p))

--Ejercicio 2
fnc :: Prop -> Prop
fnc (Not (Var p)) = Not (Var p)
fnc (Not (Cons a)) = Cons (not a)
fnc (Cons a) = Cons a
fnc (Var p) = Var p
fnc (Not p) = fnc (fnn (Not p))
fnc (And p q) = And (fnc p) (fnc q)
fnc (Or p q) = distributividad (fnc p) (fnc q)
fnc (Impl p q) = fnc (fnn (Impl p q))
fnc (Syss p q) = fnc (fnn (Syss p q))


--Función auxiliar que nos ayuda a la distributividad
distributividad :: Prop -> Prop -> Prop
distributividad p (And q r) = And (distributividad p q) (distributividad p r)
distributividad (And p q) r = And (distributividad p r) (distributividad q r)
distributividad p q = Or p q



{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas (Or p q) =  [literales (Or p q)]
clausulas p = [literales p]

--Función auxiliar que saca los literales
literales :: Prop -> Clausula
literales (Or p q) = sinDuplicados (literales p ++ literales q)
literales p = [p]


--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion cl1 cl2 =
  case compl cl1 cl2 of
    Nothing     -> sinDuplicados (cl1 ++ cl2)
    Just (l, m) -> sinDuplicados (eliminaUno l cl1 ++ eliminaUno m cl2)


eliminaUno :: Eq a => a -> [a] -> [a]
eliminaUno _ [] = []
eliminaUno x (y:ys)
  | x == y    = ys
  | otherwise = y : eliminaUno x ys

--Funcion auxiliar que busca el primer par
compl :: Clausula -> Clausula -> Maybe (Literal, Literal)
compl cl1 cl2 =
  let pares = [(l, complemento l) | l <- cl1, complemento l `elem` cl2]
      puros = [(l, m) | (l, m) <- pares, complemento l `notElem` cl1]
  in case puros of
       (x : _) -> Just x
       []      -> case pares of
         []      -> Nothing
         (x : _) -> Just x



sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados [] = []
sinDuplicados (x:xs) = x : sinDuplicados (filter (/= x) xs)

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente cl1 cl2 =
  any (\l -> complemento l `elem` cl2) cl1

--Funcion para el complemento de una literal
complemento :: Literal -> Literal
complemento (Not p) = p
complemento p = Not p

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion p =
  let cls = filter (not . esTautologia) (clausulas (fnc (Not p)))
  in if null cls then False else saturar cls

saturar :: [Clausula] -> Bool
saturar cl
  | [] `elem` cl = True
  | null nuevas  = False
  | otherwise    = saturar (cl ++ nuevas)
  where
    posibles = [ resolucion cl1 cl2 | cl1 <- cl, cl2 <- cl, hayResolvente cl1 cl2 ]
    nuevas   = filter (`notElem` cl) posibles

esTautologia :: Clausula -> Bool
esTautologia cl = any (\l -> complemento l `elem` cl) cl
