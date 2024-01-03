
open System.IO

type TileType = Invalid = '\x00' | Earth = '.' | Start = 'S' | Vertical = '│' | Horizontal = '―' | NorthToEast = '└' | NorthToWest = '┘' | SouthToWest = '┐' | SouthToEast = '┌'

let data = "input.txt" |> File.ReadAllLines
let map = data |> array2D

let z = map |> Array2D.map (fun c -> match c with | '.' -> TileType.Earth | '|' -> TileType.Vertical  | '-' -> TileType.Horizontal | 'S' -> TileType.Start | '7' -> TileType.SouthToWest | 'J' -> TileType.NorthToWest | 'F' -> TileType.SouthToEast | 'L' -> TileType.NorthToEast | _ -> failwith $"invalid char {c}")

let printMap (m: TileType array2d) =
    for x in 0 .. (m |> Array2D.length1) - 1 do
        for y in 0 .. (m |> Array2D.length2) - 1 do
            printf $"%c{LanguagePrimitives.EnumToValue m[x,y]}"
        printfn ""
    
printMap z
