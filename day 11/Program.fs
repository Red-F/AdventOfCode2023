
open System.IO

let data = "input.txt" |> File.ReadAllLines
let image = data |> array2D
// let insertRowAfterIndex (matrix: 'a[,]) (row: 'a[]) (index: int) =
//     let topPart = matrix[*,0..index]
//     let bottomPart = matrix[*,index+1..]
//     Array2D.append (Array2D.append topPart (array2D [ [|row|] ])) bottomPart

let insertRowAfterIndex (matrix: 'a[,]) (row: 'a[]) (index: int) =
    let numRows, numCols = matrix.GetLength(0), matrix.GetLength(1)
    Array2D.init numRows numCols (fun i j ->
        if i <= index then
            matrix.[i, j]
        elif i = index + 1 then
            row.[j]
        else
            matrix.[i - 1, j]
    )
let checkRows (m:char [,]) =
    for x in 0..m.GetUpperBound 0 do m[x,*] |> Array.tryFindIndex (fun c -> c = '#') |> (fun o -> match o with | None -> (printfn $"row {x} must expand") | _ -> ())
    for y in 0..m.GetUpperBound 1 do m[*,y] |> Array.tryFindIndex (fun c -> c = '#') |> (fun o -> match o with | None -> (printfn $"column {y} must expand") | _ -> ())
    
checkRows image
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"