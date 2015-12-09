open System

let file = "day3.txt"
let readText filePath = System.IO.File.ReadAllText(filePath)

let houses = [(0,0)]

let getNextHouse direction house =
  match direction with
  | '<' -> (fst house - 1, snd house)
  | '^' -> (fst house, snd house + 1)
  | '>' -> (fst house + 1, snd house)
  | 'v' -> (fst house, snd house - 1)
  | _   -> house

let houseVisited visited nh = visited |> List.exists (fun h -> h = nh)

let rec visitNext ds hs =
  if ds = ""
  then hs
  else
    let currentHouse = hs |> List.head
    let visited = hs |> List.tail
    let proceed = visitNext (ds |> Seq.tail |> String.concat)
    //let furtherDirections = ds |> Seq.tail
    let nextHouse = getNextHouse nextDirection currentHouse
    if not (houseVisited visited nextHouse)
    then proceed (nextHouse :: hs)
    else proceed hs

let directions = file |> readText

let visited = visitNext directions houses
