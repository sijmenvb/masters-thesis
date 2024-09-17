plus :: Int -> Int -> Int


val = 42 

fun x val = plus x val

fun2 = (\x -> plus x True)

fun3 x =
  let plzWork = plus x val
   in plzWork 