busqueda :: (Eq a) => a -> [a] -> Int
busqueda _ [] = 0
busqueda x (y:ys) = if x == y then length z else do 
	busqueda x ys 
	where z = length (y:ys) (-) (y:ys) !! x
