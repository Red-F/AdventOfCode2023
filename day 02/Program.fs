open System.IO
open Common.Regex

type Color = Red | Green | Blue

let parseGames lines =
    let parseGame index (line : string) =
        let parseHand (s : string) =
            let parseCubes (cubes: string) =
                match cubes with
                | Regex @"([0-9]+) blue" [count] -> Blue, int count
                | Regex @"([0-9]+) green" [count] -> Green, int count
                | Regex @"([0-9]+) red" [count] -> Red, int count
                | _ -> failwith "can not parse"
            s.Split(',') |> Array.map parseCubes
        line.Split(';') |> Array.map parseHand |> (fun x -> (index+1, x))
    lines |> Seq.mapi parseGame

let lines = "input.txt" |> File.ReadLines

let bag = Map [(Red, 12); (Green, 13); (Blue, 14)]

let part1 =
    lines |> parseGames |> Seq.filter (fun (_, game) -> game |> Array.fold (Array.fold (fun acc (color, count) -> acc && bag[color] >= count)) true) |> Seq.sumBy fst

let adjustColor (acc : Map<Color,int>) (color : Color) (count : int) =
    if acc[color] >= count then acc else acc.Change (color, (fun x -> match x with | Some _ -> Some count | _ -> None))

let part2 =
    lines |> parseGames |>
    Seq.map (fun (_, x) -> Array.fold (Array.fold (fun acc (color, count) -> adjustColor acc color count )) (Map [(Red, 0); (Green, 0); (Blue, 0)]) x ) |>
    Seq.map (fun x -> (1L, x) ||> Map.fold (fun acc _ count -> acc * int64 count)) |> Seq.sum
    
printfn $"day 2, part 1: %d{part1}"

printfn $"day 2, part 2: %d{part2}"