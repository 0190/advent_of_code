module Day6_1
type Coord = (int * int)
type Segment = (Coord * Coord)

let mergeSegments ((x,y),(x',y'),(u,v),(u',v')) =
  if x = u && x' = u' then [((x, min y v),(x', max y' v'))]
  elif y = v && y' = v' then [((min x u, y),(max x' u', y'))]
  else [(x,y),(x,y)] // else!