open System.IO

type MapItem = { Source: int64; Destination: int64; Range: int64 }
type Map = { Type: string; SourceStart: int64; SourceEnd: int64; Items: MapItem list }    
           member this.SourceToDestination source = if source < this.SourceStart || source > this.SourceEnd then source else this.Items |> List.fold (fun acc elem -> if (source >= elem.Source && source < elem.Source + elem.Range) then source - elem.Source + elem.Destination else acc) source 
           member this.DestinationToSource destination = this.Items |> List.fold (fun acc elem -> if (destination >= elem.Destination && destination < elem.Destination + elem.Range) then destination - elem.Destination + elem.Source else acc) destination 

let mapTypes = ["seed-to-soil map:"; "soil-to-fertilizer map:"; "fertilizer-to-water map:"; "water-to-light map:"; "light-to-temperature map:"; "temperature-to-humidity map:"; "humidity-to-location map:"]

let parseMaps lines =
    let parseMap s (items: string list) =
        items |> List.map (fun s -> s.Split(' ') |> (fun a -> {Source = int64 a[1]; Destination = int64 a[0]; Range = int64 a[2] })) |> List.sortBy (fun i -> i.Source) |> (fun l -> {Type = s; SourceStart = 0L; SourceEnd = 0L; Items = l })
    let sourceRange (m: Map) =
        m.Items.Tail |> List.fold (fun (f, source, range) elem -> if (elem.Source = source + range) then (f, source, range + elem.Range) else (f, source, range)) (true, m.Items.Head.Source, m.Items.Head.Range) |> (fun (_, source, range) -> { m with SourceStart = source; SourceEnd = source + range - 1L } )
    let step (maps, currentMap) s =
        match s with
        | "" -> (maps @ [ currentMap ]), []
        | _ -> maps, (currentMap @ [ s ])
    lines |> Seq.fold step ([], []) |> (fun (maps, lastMap) -> maps @ [ lastMap ]) |> List.map (fun l -> parseMap l.Head l.Tail |> sourceRange) 

let data = "input.txt" |> File.ReadLines
let seeds = data |> Seq.head |> (fun s -> s.Split(" ")) |> Array.skip 1  |> Array.map int64 |> Array.toSeq
let maps = data |> Seq.skip 2 |> parseMaps
let seedToSoil seed = mapTypes |> List.fold (fun acc elem -> (maps |> List.find (fun l -> l.Type = elem)).SourceToDestination  acc) seed
let part1 = seeds |> Seq.map seedToSoil |> Seq.min

let seedsToSequences =
    let rec pairsFromList (l: int64 list) = match l with | x::y::l' -> (x,y)::pairsFromList l' | _ -> []
    seeds |> Seq.toList |> pairsFromList
let soilToSeed soil =  mapTypes |> List.rev |> List.fold (fun acc elem -> (maps |> List.find (fun l -> l.Type = elem)).DestinationToSource  acc) soil
let inSeedSequences seed = seedsToSequences |> List.fold (fun acc (low, range) -> acc || (seed >= low && seed < low + range)) false
let part2 = Seq.unfold (fun i -> Some((soilToSeed i, i + 1L))) 0L |> Seq.filter inSeedSequences |> Seq.head |> seedToSoil

printfn $"day 5, part 1: %d{part1}"
printfn $"day 5, part 2: %d{part2}"
