open System.IO

let data = "input.txt" |> File.ReadAllLines
let image = data |> array2D

let insertRowAfterIndex (matrix: 'a[,]) (row: 'a[]) (index: int) =
    let numRows, numCols = matrix.GetLength(0), matrix.GetLength(1)
    Array2D.init (numRows+1) numCols (fun i j -> if i <= index then matrix[i, j] elif i = index + 1 then row[j] else matrix[i - 1, j])
let insertColumnAfterIndex (matrix: 'a[,]) (column: 'a[]) (index: int) =
    let numRows, numCols = matrix.GetLength(0), matrix.GetLength(1)
    Array2D.init numRows (numCols + 1) (fun i j -> if j <= index then matrix[i, j] elif j = index + 1 then column[i] else matrix[i, j - 1] )
let checkRows (m:char [,]) =
    let emptyRow = Array.init (m.GetLength 1) (fun _ -> '.')
    let withExpandedRows = seq {for x in 0..m.GetUpperBound 0 do x, m[x,*]} |> Seq.fold (fun acc (x, slice) -> slice |>  Array.tryFindIndex (fun c -> c = '#') |> (fun o -> match o with | None -> insertRowAfterIndex acc emptyRow x | _ -> acc)) m 
    let emptyColumn = Array.init (withExpandedRows.GetLength 0) (fun _ -> '.')
    seq {for y in 0..m.GetUpperBound 1 do y, m[*,y]} |> Seq.fold (fun acc (y, slice) -> slice |>  Array.tryFindIndex (fun c -> c = '#') |> (fun o -> match o with | None -> insertColumnAfterIndex acc emptyColumn y | _ -> acc)) withExpandedRows
let printImage (image: char [,]) =
    for x in 0..image.GetUpperBound 0 do
        for y in 0..image.GetUpperBound 1 do printf $"{image[x,y]}"
        printfn ""
        
let z = checkRows image

printfn "Original"
printImage image
printfn "Expanded"
printImage z
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"