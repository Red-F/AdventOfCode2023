module Common.Combination

let rec combinations n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, x::xs -> List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

//
// let rec combinations n m source =
//     if m = 0 then
//         seq { yield Seq.empty }
//     elif Seq.isEmpty source then
//         Seq.empty
//     else
//         seq {
//             for head, rest in Seq.pairwise source do
//                 yield! Seq.map (fun tail -> head :: tail) (combinations (n - 1) (m - 1) rest)
//             yield! combinations (n - 1) m rest
//         }
//
// // Example usage:
// let source = [1; 2; 3; 4; 5]
// let m = 3
// let result = combinations (List.length source) m (List.toSeq source)
//
// for subset in result do
//     printfn "[%s]" (String.Join(", ", subset |> List.map string))
