#load "day5_2.fs"
open Day5_2

let assertResult (f: 'a -> 'b -> bool) (dict: Map<'a, 'b>) =
  dict |> Map.iter (fun key value ->
                      if f key value then printf "."
                      else printf "F\n%A expected result %A\n" key value)

let strings = Map.ofList [("aaa", false);
                          ("aaaa", true);
                          ("a", false);
                          ("aa", false);
                          ("abab", true);
                          ("aba", false);
                          ("bab", false);
                          ("ab00ab", false);
                          ("ab000ab", true);
                          ("asdf4545hjkl", true)]

let isNiceTest s b =
  isNice (explode s) = b

assertResult isNiceTest strings