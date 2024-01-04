
open System.IO

type Point2D = { X : int; Y: int }
type TileType = Invalid = '\x00' | Earth = '.' | Start = 'S' | NorthToSouth = '│' | WestToEast = '―' | NorthToEast = '└' | NorthToWest = '┘' | SouthToWest = '┐' | SouthToEast = '┌'
type Direction = North = 0 | East = 1 | South = 2 | West = 3
let directionOffsets = [| (-1, 0); (0, 1); (1, 0); (-1, 0) |]

let data = "input.txt" |> File.ReadAllLines
let map = data |> array2D

let z = map |> Array2D.map (fun c -> match c with | '.' -> TileType.Earth | '|' -> TileType.NorthToSouth  | '-' -> TileType.WestToEast | 'S' -> TileType.Start | '7' -> TileType.SouthToWest | 'J' -> TileType.NorthToWest | 'F' -> TileType.SouthToEast | 'L' -> TileType.NorthToEast | _ -> failwith $"invalid char {c}")

let y = Map[
    (TileType.NorthToSouth, Map [(Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.South, [TileType.NorthToSouth; TileType.NorthToEast; TileType.NorthToWest; TileType.Start])]);
    (TileType.WestToEast, Map [(Direction.West, [TileType.WestToEast; TileType.NorthToEast; TileType.SouthToEast; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
    (TileType.NorthToEast, Map [(Direction.North, [TileType.NorthToSouth; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
    (TileType.NorthToWest, Map [(Direction.North, [TileType.WestToEast; TileType.SouthToEast; TileType.SouthToWest; TileType.Start]); (Direction.West, [TileType.WestToEast; TileType.SouthToEast; TileType.NorthToEast; TileType.Start])]);
    (TileType.SouthToWest, Map [(Direction.South, [TileType.NorthToSouth; TileType.NorthToEast; TileType.NorthToWest; TileType.Start]); (Direction.West, [TileType.WestToEast; TileType.SouthToEast; TileType.NorthToEast; TileType.Start])]);
    (TileType.SouthToEast, Map [(Direction.South, [TileType.NorthToSouth; TileType.NorthToWest; TileType.SouthToWest; TileType.Start]); (Direction.East, [TileType.WestToEast; TileType.NorthToWest; TileType.SouthToWest; TileType.Start])]);
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

let start = z |> find2D TileType.Start |> (fun p -> match p with | Some v -> v | _ -> failwith "start not found")

// let nextConnection (data: TileType [,]) (p: Point2D) =
//     let testIfConnected (directions: Direction list) =
//         if directions = [] then None else
//         let direction = directions.Head
//         let testLocation = directionOffsets[int direction] |> (fun t -> { X = p.X + fst t; Y = p.Y + snd t })
//         let isConnected p1 p2 =
//             match p1 with
//             | NorthToSouth -> match p2 with | Start |  
//             | _ -> false
//         let thisData = data[p.X, p.Y]
//         if (testLocation.Y = 0 && direction = Direction.West) || ( testLocation.Y + 1 = data.GetLength 1 && direction = Direction.East) then None
//         elif (testLocation.X = 0 && direction = Direction.North) || (testLocation.X + 1 = data.GetLength 2 && direction = Direction.South) then None
//         else None
//     match data[x,y] with
//     | TileType.Start -> 0
//     | TileType.NorthToSouth
//     | _ -> 0
    // for i in Direction.North .. Direction.West do 

let printMap (m: TileType array2d) =
    for x in 0 .. (m |> Array2D.length1) - 1 do
        for y in 0 .. (m |> Array2D.length2) - 1 do
            printf $"%c{LanguagePrimitives.EnumToValue m[x,y]}"
        printfn ""
    
printMap z
