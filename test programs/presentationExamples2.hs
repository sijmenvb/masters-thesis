plus:: Int -> Int -> Int

invertNum :: Bool -> Int -> Int

iterate :: (Int -> Int) -> Int -> (Int -> Int)

trice f x = f (f (f x))


fun = plus (plus 4 6 7

--fun2 = plus plus 4 4)

--fun3 = plus plus plus 4 5 plus plus plus 2 3 4 5 6

--fun4 = trice \x -> trice \y -> plus y y x 5

--fun5 = iterate \x invertNum x True 8 2








--fun6 = \x y -> plus x y) 5

--fun7 = \x -> (\y -> plus x y 5