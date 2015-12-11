module Shared

  let file = "day3.txt"
  let explode (s : string) = [for c in s -> c]
  let readToList filePath = filePath |> System.IO.File.ReadAllText |> explode

  let houses = [(0,0)]

  let getNextHouse direction house =
    match direction with
    | '<' -> (fst house - 1, snd house)
    | '^' -> (fst house, snd house + 1)
    | '>' -> (fst house + 1, snd house)
    | _ -> (fst house, snd house - 1)

  let rec visitNext ds hs =
    if ds = []
    then hs
    else
      let currentHouse = List.head hs
      let currentDirection = List.head ds
      let nextHouse = getNextHouse currentDirection currentHouse
      visitNext (List.tail ds) (nextHouse :: hs)
