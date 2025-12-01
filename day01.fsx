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
    let init = ({ Direction = Left; Clicks = 0 } , 50)

    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map parseRotation
    |> Seq.scan (fun (_, pos) rot -> rot, rotate pos rot) init
    |> Seq.filter (fun (_, pos) -> pos = 0)
    |> Seq.length

part1 "test.txt"