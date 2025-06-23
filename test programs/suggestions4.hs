trice f x = f (f (f x))

invertNum :: Bool -> Int -> Int

iterate :: (Int -> Int) -> Int -> (Int -> Int)



invertNum :: Bool -> Int -> Int

plus :: Int -> Int -> Int

fun2 x y =
    let
        var3 = 
            let
                fun3 = plus
            in
                fun4 5 (fun3 5 5)
        fun4 a b = plus a b 
    in 
        var3)



fun = plus plus 4 5 6