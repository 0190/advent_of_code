#load "functions_service.fs"
open BasicFunctions

let file = "day2_1.txt"
let lines = readLines file
let splitLineByX s = splitStringByChar(s, 'x')
let strSeqToIntArr list = (System.Int32.Parse |> Array.map) list |> Seq.toArray
let dimensions = lines |> (splitLineByX |> Seq.map) |> (strSeqToIntArr |> Seq.map)

let calculatePaperForOnePresent (d : array<int>) =
  let a = d.[0] * d.[1]
  let b = d.[1] * d.[2]
  let c = d.[2] * d.[0]
  (rectangularParapSquare a b c) + ([|a; b; c|] |> Seq.min)

let sum = dimensions |> (calculatePaperForOnePresent |> Seq.map) |> Seq.sum