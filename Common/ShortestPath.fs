module Common.ShortestPath
open Microsoft.FSharp.Core.Operators.Checked

let inline floydMarshall<'T when 'T: (static member (+): 'T * 'T -> 'T) and 'T: comparison> (graph: 'T[,]) =
    let n = Array2D.length1 graph
    let dist = Array2D.copy graph

    for k in 0 .. n - 1 do
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                let sum =
                    try
                        dist[i, k] + dist[k, j]
                    with
                    | :? System.OverflowException -> if (dist[i,k] >  dist[k,j]) then dist[i,k] else dist[k,j]
                // printfn "[%d, %d] %A, [%d, %d] %A ,sum %A, [%d, %d] %A" i k dist[i, k] k j dist[k, j] sum i j dist[i, j]
                if (sum < dist[i, j]) then dist[i, j] <- sum
                // printfn "[%d, %d] %A" i j dist[i, j]
    dist
