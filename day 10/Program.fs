open System
open System.IO

type Point2D = { X : int; Y: int }
type TileType = Invalid = '\x00' | Earth = '.' | Start = 'S' | NorthToSouth = '│' | WestToEast = '―' | NorthToEast = '└' | NorthToWest = '┘' | SouthToWest = '┐' | SouthToEast = '┌'
type Direction = North = 0 | East = 1 | South = 2 | West = 3

let data = "input.txt" |> File.ReadAllLines
let map = data |> array2D |> Array2D.map (fun c -> match c with | '.' -> TileType.Earth | '|' -> TileType.NorthToSouth  | '-' -> TileType.WestToEast | 'S' -> TileType.Start | '7' -> TileType.SouthToWest | 'J' -> TileType.NorthToWest | 'F' -> TileType.SouthToEast | 'L' -> TileType.NorthToEast | _ -> failwith $"invalid char {c}") 
let PossibleConnections = Map[
    (TileType.NorthToSouth, Map [(Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.South, [TileType.NorthToSouth; TileType.NorthToEast; TileType.NorthToWest; TileType.Start])]);
    (TileType.WestToEast, Map [(Direction.West, [TileType.WestToEast; TileType.NorthToEast; TileType.SouthToEast; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
    (TileType.NorthToEast, Map [(Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
    (TileType.NorthToWest, Map [(Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.West, [TileType.WestToEast; TileType.SouthToEast; TileType.NorthToEast; TileType.Start])]);
    (TileType.SouthToWest, Map [(Direction.South, [TileType.NorthToSouth; TileType.NorthToEast; TileType.NorthToWest; TileType.Start]); (Direction.West, [TileType.WestToEast; TileType.SouthToEast; TileType.NorthToEast; TileType.Start])]);
    (TileType.SouthToEast, Map [(Direction.South, [TileType.NorthToSouth; TileType.NorthToWest; TileType.NorthToEast; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
    (TileType.Start, Map [
        (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest]); (Direction.West, [TileType.WestToEast; TileType.SouthToEast; TileType.NorthToEast]);
        (Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest]); (Direction.South, [TileType.NorthToSouth; TileType.NorthToWest; TileType.NorthToEast]);
    ]);
]
let find2D needle (arr: 'a [,]) = 
    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr[x,y] = needle   then Some { X = x; Y = y}
          else go (x+1) y
    go 0 0
let connections (data: TileType [,]) (p: Point2D) =
    let connection thisTileType direction =
        let nextTile = match direction with | Direction.North -> { X = p.X - 1; Y = p.Y } | Direction.South -> { X = p.X + 1; Y = p.Y } | Direction.West -> { X = p.X; Y = p.Y - 1 } | Direction.East -> { X = p.X; Y = p.Y + 1 } | _ ->failwith $"unknown direction {direction}"
        if nextTile.X < 0 || nextTile.X >= data.GetLength 0 || nextTile.Y < 0 || nextTile.Y >= data.GetLength 1 then None
        else PossibleConnections[thisTileType].[direction] |> List.tryFind (fun t -> t = data[nextTile.X, nextTile.Y]) |> (fun o -> match o with | None -> None | _ -> Some nextTile)
    let tileType = data[p.X, p.Y]
    match tileType with
    | TileType.NorthToSouth -> [Direction.North; Direction.South] |> List.map (connection tileType)
    | TileType.WestToEast -> [Direction.West; Direction.East] |> List.map (connection tileType)
    | TileType.NorthToEast -> [Direction.North; Direction.East] |> List.map (connection tileType)
    | TileType.NorthToWest -> [Direction.North; Direction.West] |> List.map (connection tileType)
    | TileType.SouthToWest -> [Direction.South; Direction.West] |> List.map (connection tileType)
    | TileType.SouthToEast -> [Direction.South; Direction.East] |> List.map (connection tileType)
    | TileType.Start -> Enum.GetValues<Direction>() |> Array.fold (fun acc elem -> match connection tileType elem with | None -> acc | Some v -> Some v::acc) []
    | _ -> failwith $"No connections can be found for type {data[p.X, p.Y]}"
    |> List.choose id
let printMap (m: TileType array2d) =
    for x in 0 .. (m |> Array2D.length1) - 1 do
        for y in 0 .. (m |> Array2D.length2) - 1 do
            printf $"%c{LanguagePrimitives.EnumToValue m[x,y]}"
        printfn ""
let start = map |> find2D TileType.Start |> (fun p -> match p with | Some v -> v | _ -> failwith "start not found")
let firstConnectedTile = connections map start |> List.head
let rec createLoop tile loop = connections map tile |> List.filter (fun p ->  not (List.contains p loop)) |> (fun l -> match l with | [] -> tile::loop | _ -> l |> List.head |> (fun p -> createLoop p (tile::loop)))
let loop = createLoop firstConnectedTile [start]

let part1 = loop |> List.length |> (fun x -> x / 2)

let countInsideLoop (m: TileType array2d) (loop: Point2D array) =
    let insideLoop p =
        let mutable inside = false
        let mutable p1 = loop[0]
        for i in 1..loop.Length do
            let p2 = loop[i%loop.Length]
            if p.X > int (Math.Min (float p1.X, float p2.X)) then
                if p.X <= int (Math.Max(float p1.X, float p2.X)) then
                    if p.Y <= int (Math.Max(float p1.Y, float p2.Y)) then
                        let intersection = (p.X - p1.X) * (p2.Y - p1.Y) / (p2.X - p1.X) + p1.Y
                        if p1.Y = p2.Y || p.Y <= intersection then inside <- not inside
            p1 <- p2
        inside
    seq {for x in 0..(m.GetLength 0) - 1 do for y in 0..(m.GetLength 1) - 1 do yield {X =x; Y = y }} |> Seq.fold (fun acc elem  -> match loop |> Array.contains elem with | false -> (match insideLoop elem with | true -> acc + 1 | false -> acc) | _ -> acc) 0

let part2 = countInsideLoop map (loop |> List.toArray)

printMap map
printfn $"day 10, part 1: %d{part1}"
printfn $"day 10, part 2: %d{part2}"