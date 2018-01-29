
triangles xs = [(x, y, z) | x <- xs, y <- xs, z <- xs, x + y > z, y + z > x, x + z > y]

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

