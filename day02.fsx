type Range = {
    Start: uint64
    End: uint64
}

let parseRange (input: string) =
    let parts = input.Split("-")
    {
        Start = parts.[0] |> uint64
        End = parts.[1] |> uint64
    }

let parseRanges (input: string) =
    input.Split(",")
    |> Array.map parseRange

let values range =
    [| for i in range.Start .. range.End do yield i |]

let isInvalid value =
    let str = value.ToString()
    let halfLen = str.Length / 2
    str[..halfLen - 1] = str[halfLen..]

let invalidValues range =
    range
    |> values
    |> Array.Parallel.filter isInvalid

let part1 filename =
    filename
    |> System.IO.File.ReadAllText
    |> parseRanges
    |> Array.Parallel.map invalidValues
    |> Array.concat
    |> Array.sum

// Part 2

let isInvalid2 value =
    let str = value.ToString()
    let chunks = [|1..str.Length / 2|]

    // Check all possible chunk sizes
    chunks
    |> Array.Parallel.exists (fun size ->
        str
        |> Seq.chunkBySize size
        |> Seq.map System.String
        |> Seq.distinct
        |> Seq.length
        |> (=) 1
    )

let invalidValues2 range =
    range
    |> values
    |> Array.Parallel.filter isInvalid2

let part2 filename =
    filename
    |> System.IO.File.ReadAllText
    |> parseRanges
    |> Array.Parallel.map invalidValues2
    |> Array.concat
    |> Array.sum