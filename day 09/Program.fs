open System
open System.IO

let data = "input.txt" |> File.ReadAllLines |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int64 |> Array.toList)

let rec differences l a = if a |> List.forall (fun x -> x = 0L) then (0L::(0L::a))::l else differences (a::l) (a |> List.pairwise |> List.map (fun (a, b) -> b - a))

let rec projections (ll: int64 list list) =
    match ll with
    | [y] -> y.Head, y |> List.last
    | _ -> ll.Head |> (fun l -> l.Head, l |> List.last) |> (fun (first, last) -> (ll.Tail.Head.Head - first) :: (List.append ll.Tail.Head [(last + (ll.Tail.Head |> List.last))])) |> (fun n -> n::ll.Tail.Tail) |> projections

let part1 = data |> Seq.map (differences []) |> Seq.map (fun x -> projections x |> snd) |> Seq.sum
let part2 = data |> Seq.map (differences []) |> Seq.map (fun x -> projections x |> fst) |> Seq.sum


printfn $"day 7, part 1: %d{part1}"
printfn $"day 7, part 2: %d{part2}"
