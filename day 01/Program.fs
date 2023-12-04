open System
open System.IO

let parseDigitCalibrationValues lines =
    let parseCalibrationValue (line : string) =
        (line |> Seq.find Char.IsDigit |> Char.ToString) + (line |> Seq.findBack Char.IsDigit |> Char.ToString) |> int 
    Seq.map parseCalibrationValue lines
    
let words = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
let digits = [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9" |]

let parseDigitAndWordCalibrationValues lines =
    let firstWordOrDigit (line : string) =
        let firstWord = words |> Array.map (fun w -> (line.IndexOf(w), w)) |> Array.filter (fun (i, _) -> i >= 0) |> (fun a -> match a with [||] -> [| (100, "") |] | _ -> a)  |> Array.minBy fst |> (fun (i, w) -> (i, Array.IndexOf(words, w) + 1))
        let firstDigit = digits |> Array.map (fun w -> (line.IndexOf(w), w)) |> Array.filter (fun (i, _) -> i >= 0) |> (fun a -> match a with [||] -> [| (100, "") |] | _ -> a) |> Array.minBy fst |> (fun (i, w) -> (i, Array.IndexOf(digits, w) ))
        seq { firstWord; firstDigit } |> Seq.minBy fst |> snd
    let lastWordOrDigit (line : string) =
        let firstWord = words |> Array.map (fun w -> (line.LastIndexOf(w), w)) |> Array.filter (fun (i, _) -> i >= 0) |> (fun a -> match a with [||] -> [| (-1, "") |] | _ -> a) |> Array.maxBy fst |> (fun (i, w) -> (i, Array.IndexOf(words, w) + 1))
        let firstDigit = digits |> Array.map (fun w -> (line.LastIndexOf(w), w)) |> Array.filter (fun (i, _) -> i >= 0) |> (fun a -> match a with [||] -> [| (-1, "") |] | _ -> a) |> Array.maxBy fst |> (fun (i, w) -> (i, Array.IndexOf(digits, w) ))
        seq { firstWord; firstDigit } |> Seq.maxBy fst |> snd
    let parseDigitAndWordCalibrationValue (line : string) =
        ((firstWordOrDigit line) * 10) + (lastWordOrDigit line)
    Seq.map parseDigitAndWordCalibrationValue lines
    
let calibrationDocument = "input.txt" |> File.ReadLines

printfn $"day 1, part 1: %d{calibrationDocument |> parseDigitCalibrationValues |> Seq.sum}"

printfn $"day 1, part 2: %d{calibrationDocument |> parseDigitAndWordCalibrationValues |> Seq.sum}"