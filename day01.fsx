type Direction =
    | Left
    | Right

type Rotation = {
    Direction: Direction
    Clicks: int
}

let parseRotation (input: string) =
    let direction =
        match input.[0] with
        | 'L' -> Left
        | 'R' -> Right
        | _ -> failwithf "Invalid direction: %c" input.[0]

    let clicks = input.[1..] |> int

    { Direction = direction; Clicks = clicks }


let rotate pos rotation =
    let direction =
        match rotation.Direction with
        | Left -> -1
        | Right -> 1
    let clicks = rotation.Clicks % 100 * direction

    // We overflow the circle, so we add 100 before modulo
    (pos + clicks + 100) % 100

let part1 filename =
    let init = { Direction = Left; Clicks = 0 } , 50

    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map parseRotation
    |> Seq.scan (fun (_, pos) rot -> rot, rotate pos rot) init
    |> Seq.filter (fun (_, pos) -> pos = 0)
    |> Seq.length


// Part 2
let rotate2 pos rotation =
    let direction =
        match rotation.Direction with
        | Left -> -1
        | Right -> 1
    let clicks = rotation.Clicks % 100 * direction
    let overflows = rotation.Clicks / 100
    let newPos = pos + clicks

    if newPos > 0 && newPos < 100 then
        newPos, overflows
    else
        // If initial position was at 0, don't count the initial overflow
        let additionalOverflow =
            if pos = 0 then 0 else 1
        (newPos + 100) % 100, overflows + additionalOverflow


let part2 filename =
    let init = { Direction = Left; Clicks = 0 } , (50, 0)

    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map parseRotation
    |> Seq.scan (fun (_, (pos, _)) rot -> rot, rotate2 pos rot) init
    |> Seq.sumBy (fun (_, (_, overflows)) -> overflows)