let fileName = "day5.txt"
let contains x = List.exists ((=) x)
let explode (s : string) = [for c in s -> c]
let implode (cs : seq<char>) = System.String.Concat cs
let readLines filePath = filePath |> System.IO.File.ReadLines |> Seq.map explode
let tail s = s |> Seq.tail |> System.String.Concat

let rec pairList cs acc =
  let h = cs |> List.head
  let t = cs |> List.tail
  if t = [] then acc
  else pairList t ((h, List.head t) :: acc)

let rec hasNonConsecutiveDuplicates ls acc =
  not (ls = []) &&
  let hd = ls |> List.head
  let tl = ls |> List.tail
  (contains hd acc && (hd <> (acc |> List.head) ||
    // the following condition is for when, for example, ls = [x;x;x]
    // in this case all the duplicated are consecutive,
    // but the pair of ones on the outside is, however, nonconsecutive
    let acctl = acc |> List.tail
    not (acctl = []) && hd = (acctl |> List.head)) ||
    hasNonConsecutiveDuplicates tl (hd :: acc))

let rec hasChainedPairs prs =
  let pr = List.head prs
  let tl = List.tail prs
  not (tl = []) &&
  let nx = List.head tl
  snd pr = fst nx || hasChainedPairs tl

let containsSomePairTwice s =
  let pairs = pairList s []
  hasNonConsecutiveDuplicates pairs []

let containsPairDividedBySomeLetter s =
  let pairs = pairList s []
  hasChainedPairs pairs

let isNice s =
  containsSomePairTwice s && (containsPairDividedBySomeLetter s)

let niceLines = fileName |> readLines |> Seq.filter isNice
let answer = niceLines |> Seq.length
