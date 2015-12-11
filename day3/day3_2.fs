#load "day3_shared.fs"
open Shared

let rec dividePaths ds dsanta drobot =
  match ds with
  | []               -> (dsanta, drobot)
  | [e]              -> (dsanta @ [e], drobot)
  | e1 :: e2 :: list -> dividePaths list (dsanta @ [e1]) (drobot @ [e2])

let directions = file |> readToList
let (santaDirections, robotDirections) =
  dividePaths directions [] []
let visited = (visitNext santaDirections houses) @ (visitNext robotDirections houses) |> List.distinct
let numberOfLuckyHouses = visited |> List.length
