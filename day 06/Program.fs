open System
open System.IO

let data = "input.txt" |> File.ReadLines |> Seq.toArray
let times = data[0].Split(':')[1] |> (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> Array.map int64
let distances = data[1].Split(':')[1] |> (fun x -> x.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> Array.map int64
let discriminant a b c = (b * b) - 4.0 * a * c
let quadraticSolutions a b c = if discriminant a b c >= 0 then ((-b - Math.Sqrt (discriminant a b c)) / 2.0 * a, (-b + Math.Sqrt (discriminant a b c)) / 2.0 * a) |> (fun t -> if fst t < snd t then t else snd t, fst t) else failwith "Discriminant less than zero"
let part1 = (times, distances) ||> Array.map2 (fun b c -> quadraticSolutions -1 (float b) (float -c) |> (fun t -> int64 (Math.Ceiling ((snd t) - 1.0) - Math.Floor ((fst t) + 1.0)) + 1L)) |> Array.reduce (*)

let time =  data[0].Split(':')[1] |> (fun x -> x.Replace(" ", "")) |> int64
let distance =  data[1].Split(':')[1] |> (fun x -> x.Replace(" ", "")) |> int64
let part2 = quadraticSolutions -1 (float time) (float -distance) |> (fun t -> int64 (Math.Ceiling ((snd t) - 1.0) - Math.Floor ((fst t) + 1.0)) + 1L)

printfn $"day 6, part 1: %d{part1}"
printfn $"day 6, part 2: %d{part2}"