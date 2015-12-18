module Shared
let fileName = "day5.txt"
let contains x = List.exists ((=) x)
let explode (s : string) = [for c in s -> c]
let implode (cs : seq<char>) = System.String.Concat cs
let readLines filePath = filePath |> System.IO.File.ReadLines |> Seq.map explode

let lines = readLines fileName