{-
 -- this only works for convex polygons.
triangleArea :: [Double] -> [Double] -> [Double] -> Double
triangleArea p1 p2 p3 =
  let x1 = head p1
      x2 = head p2
      x3 = head p3
      y1 = last p1
      y2 = last p2
      y3 = last p3
  in
  0.5 * abs ( (x1 - x3) * (y2 - y1) - (x1 - x2) * (y3 - y1) )

-- anchor point, point list -> the area
polygonArea :: [Double] -> [[Double]] -> Double
polygonArea anchor [] = 0.0
polygonArea anchor [_] = 0.0
polygonArea anchor (p1:otherVertices) = 
  triangleArea anchor p1 (head otherVertices) + polygonArea anchor otherVertices 
  

main = do
  raw <- getContents
  let points =  map (\s -> (map (\f -> read f::Double) (words s))) $ tail $ lines raw
  let result = polygonArea (head points) (tail points)
  print result
-}

polygonAreaGreenTheorem :: [[Double]] -> Double
polygonAreaGreenTheorem [] = 0.0
polygonAreaGreenTheorem [_] = 0.0
polygonAreaGreenTheorem (p1 : points) =
  let p2 = head points
      x1 = head p1
      x2 = head p2
      y1 = last p1
      y2 = last p2
  in
  x1 * y2 - y1 * x2 + (polygonAreaGreenTheorem points)



main = do
  raw <- getContents
  let points =  map (\s -> (map (\f -> read f::Double) (words s))) $ tail $ lines raw
  let result = 0.5 * (polygonAreaGreenTheorem $ points ++ [head points])
  print result
