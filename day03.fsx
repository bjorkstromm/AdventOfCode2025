let batteries (bank : string) =
    let charToInt c = int c - int '0'

    bank
    |> Seq.map charToInt
    |> Seq.toList

let largestJoltage batteries =   
    let rec loop state xs =
        match xs with
        | [_] | [] -> state
        | head :: tail ->
            let max = tail |> List.max
            let value = head * 10 + max
            loop (state @ [value]) tail

    loop [] batteries
    |> List.max

let part1 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.Parallel.map (batteries >> largestJoltage)
    |> Array.sum

// Part 2
let largestJoltage2 batteries =
    let batteryLength = List.length batteries
    let rec loop startIndex length acc =
        if length = 0 then acc
        else
            let stopIndex = batteryLength - length
            // find index of max in batteries[startIndex..stopIndex], leftmost on ties
            let idx =
                seq { startIndex .. stopIndex }
                |> Seq.maxBy (fun i -> batteries.[i])
            loop (idx + 1) (length - 1) (batteries.[idx] :: acc)
    loop 0 12 []
    |> List.map (uint64)
    |> List.mapi (fun i v -> v * pown 10UL i)
    |> List.sum

let part2 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.Parallel.map (batteries >> largestJoltage2)
    |> Array.sum