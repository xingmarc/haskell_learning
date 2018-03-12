
edge :: [Double] -> [Double] -> Double
edge p1 p2 =
  let x1 = head p1
      y1 = last p1
      x2 = head p2
      y2 = last p2
  in
  sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

calc :: [[Double]] -> Double
calc [] = 0.0
calc [_] = 0.0
calc points =
  let p1 = head points
      p2 = head $ tail points
  in
  (edge p1 p2) + (calc $ tail points)

main = do
  raw <- getContents
  let points =  map (\s -> (map (\f -> read f::Double) (words s))) $ tail $ lines raw
  let result = calc points + edge (head points) (last points)
  print result

