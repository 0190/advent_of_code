#load "shared.fs"
open Shared

let startsWithFiveZeros (s : string) =
  s.[0..4] = "00000"

let hashStartsWithFiveZeros (v : int, hash : string) =
  startsWithFiveZeros hash

let answerPair =
  strNumbers
  |> Seq.map keyHashPair
  |> Seq.filter hashStartsWithFiveZeros
  |> Seq.head

let decodedInt = fst answerPair
