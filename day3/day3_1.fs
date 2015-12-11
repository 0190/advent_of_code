open System

let file = "day3.txt"
let readText filePath = System.IO.File.ReadAllText(filePath)

let houses = [(0,0)]

let getNextHouse direction house =
  match direction with
  | '<' -> (fst house - 1, snd house)
  | '^' -> (fst house, snd house + 1)
  | '>' -> (fst house + 1, snd house)
  | _ -> (fst house, snd house - 1)

let houseVisited visited nh = visited |> List.exists (fun h -> h = nh)

// TODO: rewrite to use tail recursion
let rec visitNext ds hs =
  if ds = ""
  then hs
  else
    let currentHouse = hs |> List.head
    let visited = hs |> List.tail
    let proceed = visitNext (ds |> Seq.tail |> System.String.Concat)
    let nextDirection = ds |> Seq.head
    let nextHouse = getNextHouse nextDirection currentHouse
    proceed (nextHouse :: hs)

let directions = file |> readText
let visited = (visitNext directions houses) |> List.distinct
let numberOfLuckyHouses = visited |> List.length
