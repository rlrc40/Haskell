-- Problema 1: Ultimo elemento de una lista
myLast :: [a] -> a
myLast lista = (lista) !! (length(lista)-1)

ultimo :: [a] -> a
ultimo [] = error "No hay ultimo elemento en una lista vaciaaa"
ultimo [x] = x
ultimo (_:xs) = ultimo xs

-- Problema 2: Penultimo elemento de una lista
penultimo :: [a] -> a
penultimo = last . init -- init te da la lista sin el ultimo elemento

myButLast :: [a] -> a
myButLast lista = reverse lista !! 1

-- Problema 3: K-esimo elmeneto
kEsimo :: [a] -> Int -> a
kEsimo lista k = lista !! (k-1)

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Indice fuera de rango"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1          = error "Indice fuera de rango"
  | otherwise      = elementAt xs (k-1)


-- Problema 4: Encontrar el numero de elementos que contiene una lista
-- Forma facil length

myLength :: [a] -> Int
myLength = sum . map (\_->1)

-- Problema 5: Invertir una lista
-- Forma facil: revert
invertir :: [a] -> [a]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

-- Problemas 5: Palindromo
esPalindromo :: (Eq a) => [a] -> Bool
esPalindromo [x] = True
esPalindromo (x:xs) = if x == last xs then esPalindromo (take (length xs -1) xs) else False

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = foldr (&&) True $ zipWith (==) xs (reverse xs)

-- Problema 7: Unir listas


-- Problema 8: eliminar los elementos duplicado
compress :: Eq a => [a] -> [a]
compress = map head . group
