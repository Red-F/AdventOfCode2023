open System
open System.IO

let nextFromRepeatingArray (a : 'T array) =
    let mutable index = -1
    (fun () -> index <- (index + 1) % a.Length; a[index])
let instructions = "input.txt" |> File.ReadLines |> Seq.head
let nextInstruction = instructions |> Array.ofSeq |> nextFromRepeatingArray
let nodes =
    "input.txt" |> File.ReadAllLines |> Seq.skip 2 |>
    Seq.map (fun s -> s |> Seq.filter (fun c -> not ("=,()".Contains c)) |> String.Concat |> (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries)) |> (fun a -> (a[0], (a[1], a[2])))) |> Map
let nextElement instruction currentElement = (if instruction = 'L' then fst nodes[currentElement] else snd nodes[currentElement])
let rec tracePeople steps currentElement = if currentElement = "ZZZ" then steps else nextElement (nextInstruction ()) currentElement |> tracePeople (steps + 1)
    
let part1 = tracePeople 0 "AAA"

let startElements = nodes.Keys |> Seq.filter (fun s -> s[2] = 'A')
let rec traceGhost steps (currentElement: string) = if currentElement[2] = 'Z' then steps else nextElement (nextInstruction ()) currentElement |> traceGhost (steps + 1L)
let rec greatestCommonDenominator a b = if b = 0L then a else greatestCommonDenominator b (a % b)
let leastCommonMultiple a b = if a = 0L || b = 0L then 0L else abs (a * b) / greatestCommonDenominator a b
let rec leastCommonMultipleOfList (numbers: int64 list) = match numbers with | [] -> 0L | [x] -> x | x::xs -> List.fold leastCommonMultiple x xs |> abs
    
let part2 = startElements |> Seq.map (traceGhost 0L) |> Seq.toList |> leastCommonMultipleOfList

printfn $"day 7, part 1: %d{part1}"
printfn $"day 7, part 2: %d{part2}"