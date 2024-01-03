
open System
open System.IO

type TileType = Invalid = '\x00' | Earth = '.' | Start = 'S' | Vertical = '।' | Horizontal = '―' | NorthToEast = '⎣' | NorthToWest = '⎦' | SouthToWest = '⎤' | SouthToEast = '⎡'
let data = "input.txt" |> File.ReadAllLines

let map = data |> array2D

let z = map |> Array2D.map (fun c -> match c with | '.' -> TileType.Earth | '|' -> TileType.Vertical  | '-' -> TileType.Horizontal | 'S' -> TileType.Start | '7' -> TileType.SouthToWest | 'J' -> TileType.NorthToWest | 'F' -> TileType.SouthToEast | 'L' -> TileType.NorthToWest | _ -> failwith $"invalid char {c}")

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"