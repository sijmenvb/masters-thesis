trice f x = f (f (f x))

iterate :: (Int -> Int) -> Int -> (Int -> Int)

invertNum :: Bool -> Int -> Int

plus :: Int -> Int -> Int

fun x y =
    let
        var3 = 
            let
                fun3 = plus
            in
                fun4 5 (fun3 5 5)
        fun4 a b = plus a b 
    in 
        var3)



fun2 = let 
        x = 4 
    in x