existe :: (Eq a) => a -> [a] -> Bool
existe x lista = foldl1 (||) $ map (==x) lista

dignum :: [Int] -> Int
dignum lista = foldl f 0 lista
  where f a lista = a*10+lista

dignum2 :: [Int] -> Int
dignum2 lista =  foldr (+) 0 $ zipWith (*) lista (map (10^) [(length(lista)-1),(length(lista)-2)..0])
-- foldr (+) 0 $ zipWith (*) [1,2,3,4] (map (10^) [3,2,1,0])

veces :: (Eq a) => [a] -> a -> Int
veces lista x = foldl f 0 (map (==x) lista)
	where f a lis = if lis == True then a+1 else a+0

dividePares :: [Int] -> [Int]
dividePares lista = [x `div` 2 | x <- lista, x `mod` 2 == 0]

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b lista = [x | x <- lista, x >= a, x <= b]

enRangoFacil :: Int -> Int -> [Int]
enRangoFacil a b = [a..b]

cuentaNegativos :: [Int] -> Int
cuentaNegativos lista = length $ filter (<0) lista

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase cadena = [ c | c <- cadena, c `elem` ['A'..'Z']]

capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]
