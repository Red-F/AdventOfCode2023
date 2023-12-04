namespace Common

open System.Text.RegularExpressions

module Regex =
    let (|Regex|_|) pattern input = Regex.Match(input, pattern) |> (fun m -> if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None)
