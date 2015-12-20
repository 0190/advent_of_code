module CommandParser
open System

let fileName = "day6.txt"
let separators = [|"turn"; "through"; " "|]
let readLines filePath = filePath |> System.IO.File.ReadLines

type coord = (int * int)
type state = seq<coord>
type command = state -> (coord * coord) -> state
type commandEntry = command * coord * coord

let parseCoords (s : string) =
  let list = s.Split(',') |> Array.toList |> List.map (fun x -> int(x))
  (list.[0], list.[1])

let parseCoordsTuple (s1: string, s2: string) =
  (parseCoords s1, parseCoords s2)

let parseCommand (on : command) (off : command) (toggle : command) (line : string) =
  let arr = line.Split(separators, StringSplitOptions.RemoveEmptyEntries)
  match arr with
  | [|"on"; c1; c2|] -> (on, parseCoordsTuple(c1, c2))
  | [|"off"; c1; c2|] -> (off, parseCoordsTuple(c1, c2))
  | [|_; c1; c2|] -> (toggle, parseCoordsTuple(c1, c2))

let lightsInRectangle (c1 : coord) (c2 : coord) =
  // assume (fst c1 <= fst c2) && (snd c1 <= snd c2)
  let x' = fst c2 - fst c1 + 1
  let num = x' * (snd c2 - snd c1 + 1) //'
  Seq.init num (fun i -> (fst c1 + i % x', snd c1 + int(i / x')))