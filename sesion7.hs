existe :: (Eq a) => a -> [a] -> Bool
existe x lista = foldl1 (||) (map (==x) lista)

dignum :: [Int] -> Int
dignum lista = foldl f 0 lista --lista folfl simplemente aplica esa operacion entre la lista
    where f a lis = a*10+lis --3125=((((0*10+5)*10+2)*10+1)*10+3)

maxlis :: [Int] -> Int
maxlis = foldr1 max

  veces :: (Eq a) => [a] -> a -> Int
  veces lista x = length (filter (==x) lista)

veces2 :: (Eq a) => [a] -> [Bool] -> Int
veces2 lista x = foldl f 0 (map (==x) lista)
	where f a lis = if lis == True then a+1 else a+0

dividePares :: [Int] -> [Int]
dividePares lista = [x `div` 2 | x <- lista, x `mod` 2 == 0] --Similar a las condiciones matematicas

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b lista = [x | x <- lista, a <= x, x <= b]

cuentaPositivos :: [Int] -> Int
cuentaPositivos lista = foldl f 0 (map (>0) lista)
	where f a lis = if lis == True then a+1 else a+0

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue x lista = filter (>x) lista

cuentaPositivos2 :: [Int] -> Int
cuentaPositivos2 lista = length([x | x <- lista, x > 0])

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [ (a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]


--Dividir-->Ordenar-->Fusionar
dividir :: [a] -> ([a],[a])
dividir [] = ([],[])
dividir [x] = ([x],[])
dividir (x:y:zs) = (x:xs, y:ys) where (xs,ys) = dividir zs

fusion :: (Ord a) => [a] -> [a] -> [a]
fusion [] ys = ys
fusion xs [] = xs
fusion (x:xs) (y:ys)
	| x < y = x : fusion xs (y:ys)
	| y < x = y : fusion (x:xs) ys
	| x == y = x : y : fusion xs ys

ordenar :: (Ord  a) => [a] -> [a]
ordenar [] = []
ordenar [x] = [x]
ordenar lis = fusion (ordenar xs) (ordenar ys)
	where
		(xs, ys) = dividir lis


-- Representar polinomios como listas
test :: [Int]
test = [-4,3,0,-1,2] -- 2*x^4 - x^3 + 3*x -4
-- "2*x^2 + -1*x^2 + 0*x^2 + 3*x^2 + -4*x^2"
-- "2*x^2 + -1*x^2 + 3*x^2 + -4*x^2"  Tras aplicar el filter eliminamos el 0*10


ter2cad :: (Int,Int) -> String
ter2cad (c,0) = show c
ter2cad (1,1) = show "x"
ter2cad (-1,1) = show "-x"
ter2cad (c,1) = (show c) ++ "*x"
ter2cad (-1,e) = "-x^" ++ (show e)
ter2cad (1,e) = "x^" ++ (show e)
ter2cad (c,e) = (show c) ++ "*x^" ++ (show 2) -- Show traduce a cadena

pol2cad :: [Int] -> String
pol2cad lis =
	foldl1 unir $
	map ter2cad $ -- Traduce
	reverse $
	filter (\(c,e) -> c /= 0) $
-- Los $ nos ahorran la cantidad de parentesis que hay que añadir
-- Gracias a que $ tiene un bajo orden de precedencia podemos
-- escribir es misma expresión como sum $ map sqrt [1..130]
	zip lis [0..] -- Exponente
	where
		unir res ('-':ts) = res ++ " - " ++ ts
		-- Mira a ver si comienza con un menos, y ts es la cadena sin el menos
		-- "2*x^2 - x^3 + 3*x - 4"
		unir res ter = res ++ " + " ++ ter

	--            0       1      3
	------------------------------------
	--    -1  -  -1      -x    -x^e
	--    +1  -   1       x     x^e
	--     c  -   c      c-x    c-x^e


-- Ahora vamos a sumar polinomios
sumpol :: [Int] -> [Int] -> [Int]
sumpol [] ys = ys
sumpol xs [] = xs
sumpol (x:xs) (y:ys) = (x+y) : sumpol xs ys

-- Producto de polinomios
mulpol :: [Int] -> [Int] -> [Int]
mulpol _ [] = []
mulpol xs (y:ys) = sumpol (map (*y) xs)
						  (mulpol (0:xs) ys)


data Pol = P [Int]

instance Show Pol where
	show (P lis) = pol2cad lis

-- Le dotamos de las diferentes operaciones que puede hacer el tipo de datos Pol
instance Num Pol where
	(P l1) + (P l2) = P (sumpol l1 l2)
	(P l1) * (P l2) = P (mulpol l1 l2)
	negate (P lis)  = P (map negate lis) --les da valor negativo a todos los valores
	abs (P lis)     = undefined
	signum (P lis)  = undefined
	fromInteger n   = P [fromInteger n]

-- *Main> (P [1,1])*(P [1,2])
-- 2*x^2 + 3*x + 1

x :: Pol
x = P [0,1] -- Ahora podemos declarar cualquier polinomio

-- *Main> (x+1)*(x-1)
-- x^2 - 1
-- *Main> (x+1)^8
-- x^8 + 8*x^2 + 28*x^2 + 56*x^2 + 70*x^2 + 56*x^2 + 28*x^2 + 8*x + 1


-- Van a caer bastantes preguntas con codigo haskell, que tecnicas se estan utilizando en este codigo?,
-- secciones, concordancia de patrones, evaluacion diferida, subexpresiones, listas infinitas...
-- Codigo Haskell y decir que hace el programa, o hacer alguna linea de Haskell
-- Preguntas usando map, filter, fold (no diferenciar entre l, r), zip, zipwith(te evita el map)
-- \n -> n+n
-- O resolverlas en python con map filter reduce(fold) y filter
-- LAMBDA n : n + n

paridad :: Int -> Int
paridad n = (paridad2 n 0) where
  paridad2 n k = if n `mod` 2 /= 0 then k else paridad2 (n `div` 2) (k+1)

--paridad2 :: Int -> Int -> Int
--paridad2 n k = if n `mod` 2 /= 0 then k else paridad2 (n `div` 2) (k+1)

iter :: (a -> a) -> a ->[a]
iter f x = x : iter f (f x)

misterio :: [Int] -> Int
misterio lis =
               foldr1 (+) $
               map (\(a,b) -> a*b) $
               zip lis $
               map (10^)[0..]
