
triangles xs = [(x, y, z) | x <- xs, y <- xs, z <- xs, x + y > z, y + z > x, x + z > y]

