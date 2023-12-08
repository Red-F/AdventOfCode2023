open System.IO
open Common.Regex

type ScratchCard = {CardNumber: int; WinningNumbers: int list; MyNumbers: int list; CountOfWinningNumbers : int; CountOfCards : int}
let parseScratchCards =
    let score c = c.MyNumbers |> List.fold (fun acc elem -> match List.contains elem c.WinningNumbers with | false -> acc | true -> acc + 1) 0
    let step (s: string) =
        let intListFromString (s : string) = s.Replace("  ", " ").Trim(' ').Split(' ') |> Array.toList |> List.map int
        match s with
        | Regex @"Card +([0-9]+): +(([0-9]+ +)+)\|(( +[0-9]+)+)" [ cardNumber; winningNumbers; _; myNumbers; _ ] ->
            { CardNumber = int cardNumber; WinningNumbers = intListFromString winningNumbers; MyNumbers = intListFromString myNumbers; CountOfWinningNumbers = 0; CountOfCards = 1 }
        | _ -> failwith "mismatch"
    "input.txt" |> File.ReadLines |> Seq.map step |> Seq.map (fun card -> { card with CountOfWinningNumbers = score card })

let part1 = parseScratchCards |> Seq.sumBy (fun c -> match c.CountOfWinningNumbers with | 0 -> 0 | _ -> int (2.0 ** float (c.CountOfWinningNumbers - 1)))

let scratchCardsPile (a : ScratchCard array) =
    let adjustPile index winCount = for i in index+1..index+winCount do a[i] <- {a[i] with CountOfCards = a[i].CountOfCards + a[index].CountOfCards}
    a |> Array.iteri (fun i c -> if c.CountOfWinningNumbers <= 0 then () else adjustPile i c.CountOfWinningNumbers)
    (0, a) ||> Array.fold (fun acc elem -> acc + elem.CountOfCards)
    
let part2 = parseScratchCards |> Seq.toArray |> scratchCardsPile

printfn $"day 4, part 1: %d{part1}"
printfn $"day 4, part 2: %d{part2}"