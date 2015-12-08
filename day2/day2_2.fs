#load "functions_service.fs"
open BasicFunctions

let file = "day2_1.txt"
let lines = readLines file
let splitLineByX s = splitStringByChar(s, 'x')
let strSeqToIntArr list = (System.Int32.Parse |> Array.map) list |> Seq.toArray
let dimensions = lines |> (splitLineByX |> Seq.map) |> (strSeqToIntArr |> Seq.map)

let calculateRibbonForOnePresent (dim : array<int>) =
  let h = dim.[0]
  let w = dim.[1]
  let d = dim.[2]
  let sorted = dim |> Array.sort
  let sorted_tail = sorted |> Array.tail
  (rectangularParapVolume h w d) + 2 * ((sorted |> Array.head) + (sorted_tail |> Array.head))

let sum_ribbon = dimensions |> (calculateRibbonForOnePresent |> Seq.map) |> Seq.sum