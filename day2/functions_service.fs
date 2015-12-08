module BasicFunctions
// collection of generic functions in f#

// function that returns content of the file as seq
// string -> System.Collections.Generic.IEnumerable<string>
let readLines filePath = System.IO.File.ReadLines(filePath)

//
// string * char -> seq<string>
let splitStringByChar (s : string, c : char) = s.Split [|c|]

// square
let rectangularParapSquare a b c = 2 * (a + b + c)

// volume
let rectangularParapVolume h w d = h * w * d