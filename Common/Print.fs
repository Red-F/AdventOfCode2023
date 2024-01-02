namespace Common

module Print =
    let printSeq a = a |> Seq.iter (fun i -> printf $"%d{i} "); printfn ""
    let printSeqOfSeq l = l |> Seq.iter printSeq; printfn ""
    let printSeqOfSeqOfSeq l = l |> Seq.iter printSeqOfSeq; printfn ""
