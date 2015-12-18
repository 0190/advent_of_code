#load "shared.fs"
open Shared

let vowels = ['a'; 'e'; 'i'; 'o'; 'u']
let forbiddenStrings = ["ab"; "cd"; "pq"; "xy"]
let isVowel c = contains c vowels
let isPairForbidden (cs : List<char>) = contains (System.String.Concat cs) forbiddenStrings
let tail s = s |> Seq.tail |> System.String.Concat

let rec atLeastThreeVowelsWithInit n s =
  n >= 3 || (not (s = []) && (atLeastThreeVowelsWithInit (if s |> List.head |> isVowel then n + 1 else n) (List.tail s)))

let atLeastThreeVowels = atLeastThreeVowelsWithInit 0

let rec hasLetterTwiceInARow s =
  let tail = s |> List.tail
  let head = s |> List.head
  if tail = [] then false
  else head = (tail |> List.head) || hasLetterTwiceInARow tail

let rec noForbiddenStrings s =
  List.length s < 2 || (not (isPairForbidden s.[0..1]) && noForbiddenStrings (s |> List.tail))

let isNice s =
  atLeastThreeVowels s && hasLetterTwiceInARow s && noForbiddenStrings s

let niceLines = lines |> Seq.filter isNice
let answer = niceLines |> Seq.length
