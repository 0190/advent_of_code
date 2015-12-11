#load "day3_shared.fs"
open Shared
let directions = file |> readToList
let visited = (visitNext directions houses) |> List.distinct
let numberOfLuckyHouses = visited |> List.length
