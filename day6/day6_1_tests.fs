#load "day6_1.fsx"
open Day6_1

let assertResult (f: 'a -> 'b) (dict: Map<'a, 'b>) =
  dict |> Map.iter (fun key value ->
                      if f key = value then printf "."
                      else printf "F\n%A expected result %A\n" key value)

let segmentsToMerge = Map [((0,0),(1,1),(2,0),(3,1)), [((0,0),(3,1))];
                           ((0,0),(1,1),(0,3),(1,3)), [((0,0),(1,3))]]

assertResult mergeSegments segmentsToMerge