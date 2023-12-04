open System
open System.IO

type Point2D = { X : int; Y: int }

let lines = "input.txt" |> File.ReadLines

let map = lines |> array2D

let potentialNumbersInRow (a : char array) row =
    let rec parseNumbers (data : char array) index partialNumber listOfNumbers =
        if index = data.Length then
            if partialNumber = "" then listOfNumbers |> List.rev else ({X = row; Y = index - partialNumber.Length}, partialNumber) :: listOfNumbers |> List.rev
        else
            if (data[index] |> Char.IsDigit) then parseNumbers data (index + 1)  $"{partialNumber}{data[index]}"  listOfNumbers
            else if partialNumber <> "" then parseNumbers data (index + 1) "" (({X = row; Y = index - partialNumber.Length}, partialNumber) :: listOfNumbers) else parseNumbers data (index + 1) "" listOfNumbers
    parseNumbers a 0 "" []

let potentialNumbers = seq { for i in 0 .. (Array2D.length1 map) - 1 do potentialNumbersInRow map[i,*] i } |> Seq.fold (fun acc elem -> elem |> List.append acc) []

let isNextToSymbol c = not (Char.IsDigit c) && c <> '.' 
let isNextToStar c = (c = '*') 
    
let testChar (p: Point2D) (a : char array2d) (f: char -> bool) =
    seq { for x in p.X - 1 .. p.X + 1 do for y in p.Y - 1 .. p.Y + 1 do if x < 0 || x >= Array2D.length1 a || y < 0 || y >= Array2D.length2 a then (false, {X = -1; Y = -1}) else (f a[x, y], {X = x; Y = y}) }
    |> Seq.tryFind fst |> (fun x -> match x with | None -> (false, {X = -1; Y = -1}) | Some s -> s)
    
let testString (p:Point2D, s:string) (a:char array2d) (f: char -> bool) = seq { for y in p.Y .. p.Y + s.Length - 1 -> y } |> Seq.fold (fun acc y -> if fst acc then acc else testChar { X = p.X; Y = y } a f) (false, {X = -1; Y = -1})

let part1 = potentialNumbers |> List.filter (fun t -> (testString t map isNextToSymbol) |> fst) |> List.map (fun t -> snd t |> int64 ) |> List.sum
let numbersNextToStar = potentialNumbers |> List.map (fun t -> (t, (testString t map isNextToStar))) |> List.filter (fun x -> fst (snd x))

let rec matchFirst (a : ((Point2D * string) * (bool * Point2D)) list) sum =
    if a = [] then sum else
        let firstNumber = a.Head
        let secondNumber = firstNumber |> (fun (_, operand) -> a.Tail |> List.tryFind (fun (_, otherOperand) -> otherOperand = operand))
        match secondNumber with
        | None -> matchFirst (a |> List.filter (fun (number, _) -> number <> fst firstNumber)) sum
        | Some v -> matchFirst (a |> List.filter (fun (number, _) -> number <> fst firstNumber && number <> fst v)) (sum + (int64 (snd (fst firstNumber)) * (int64 (snd (fst v)))))
    
let part2 = matchFirst numbersNextToStar 0L

printfn $"day 3, part 1 : %d{part1}"
printfn $"day 3, part 2 : %d{part2}"