#load "shared.fs"
open Shared

// Optimization needed ;_;
let zeroNum = 6

let startsWithNumberOfZeros (s : string, n : int) =
  s.[0..(n - 1)] = String.concat "" (Seq.init n (fun i -> "0"))

let hashStartsWithZeros (v : int, hash : string) =
  startsWithNumberOfZeros (hash, zeroNum)

let answerPair =
  strNumbers
  |> Seq.map keyHashPair
  |> Seq.filter hashStartsWithZeros
  |> Seq.head

let decodedInt = fst answerPair
