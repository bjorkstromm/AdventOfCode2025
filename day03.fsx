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

part1 "test.txt"