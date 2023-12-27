open System
open System.IO

type HandType = Nothing = 0 | HighCard = 1 | OnePair = 2 | TwoPair = 3 | ThreeOfAKind = 4 | FullHouse = 5 | FourOfAKind = 6 | FiveOfAKind = 7
type HandAndBit = { Hand: string; SortedHand: int[]; HandType: HandType; Bid: int }
let cardStrength c = match c with | _ when c >= '2' && c <= '9' -> int (c - '2') | 'T' -> 8 | 'J' -> 9 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12 | _ -> failwith $"Invalid card {c}"
let handStrength (a: int[]) =
    match a with | _ when a |> Array.exists (fun x -> x = 5) -> HandType.FiveOfAKind | _ when a |> Array.exists (fun x -> x = 4) -> HandType.FourOfAKind
                 | _ when Array.exists (fun x -> x = 3) a && Array.exists (fun x -> x = 2) a -> HandType.FullHouse
                 | _ when Array.exists (fun x -> x = 3) a && Array.filter (fun x -> x = 1) a |> Array.length = 2 -> HandType.ThreeOfAKind
                 | _ when Array.filter (fun x -> x = 2) a |> Array.length = 2 -> HandType.TwoPair
                 | _ when Array.filter (fun x -> x = 2) a |> Array.length = 1 && Array.filter (fun x -> x = 1) a |> Array.length = 3 -> HandType.OnePair
                 | _ when Array.filter (fun x -> x = 1) a |> Array.length = 5 && Array.findIndex (fun v -> v = 1) a - Array.findIndexBack (fun v -> v = 1) a = 4 -> HandType.HighCard
                 | _ -> HandType.Nothing
let cardStrengthWithJokers c = match c with | _ when c >= '2' && c <= '9' -> int (c - '1') | 'T' -> 9 | 'J' -> 0 | 'Q' -> 10 | 'K' -> 11 | 'A' -> 12 | _ -> failwith $"Invalid card {c}"
let handStrengthWithJokers (a: int[]) =
    if a[0] = 0 then handStrength a else
    match a[0] with | 5 | 4 -> HandType.FiveOfAKind
                    | 3 when Array.exists (fun x -> x = 2) a -> HandType.FiveOfAKind | 3 -> HandType.FourOfAKind
                    | 2 when Array.exists (fun x -> x = 3) a -> HandType.FiveOfAKind | 2 when Array.filter (fun x -> x = 2) a |> Array.length = 2 -> HandType.FourOfAKind | 2 -> HandType.ThreeOfAKind 
                    | 1 when Array.exists (fun x -> x = 4) a -> HandType.FiveOfAKind | 1 when Array.exists (fun x -> x = 3) a -> HandType.FourOfAKind | 1 when Array.filter (fun x -> x = 2) a |> Array.length = 2 -> HandType.FullHouse
                    | 1 when Array.filter (fun x -> x = 2) a |> Array.length = 1 -> HandType.ThreeOfAKind | 1 -> HandType.OnePair
                    | _ -> HandType.Nothing 
let sortCards s f =
    let result = Array.create 13 0
    s |> Seq.iter (fun c -> f c |> (fun i -> result[i] <- result[i] + 1))
    result
let compareHands (h1: HandAndBit) (h2: HandAndBit) f =
    let c = compare h1.HandType h2.HandType
    if c <> 0 then c else (0, h1.Hand, h2.Hand) |||> Seq.fold2 (fun acc a b -> if acc <> 0 then acc else compare (f a) (f b))
let data f g = "input.txt" |> File.ReadLines |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> (fun a -> {Hand = a[0]; SortedHand = sortCards a[0] f; HandType = g (sortCards a[0] f); Bid = int a[1] }))

let part1 = data cardStrength handStrength |> Seq.sortWith (fun h1 h2 ->compareHands h1 h2 cardStrength) |> Seq.mapi (fun i t -> (i + 1) * t.Bid) |> Seq.sum
let part2 = data cardStrengthWithJokers handStrengthWithJokers |> Seq.sortWith (fun h1 h2 -> compareHands h1 h2 cardStrengthWithJokers) |> Seq.mapi (fun i t -> (i + 1) * t.Bid) |> Seq.sum

printfn $"day 7, part 1: %d{part1}"
printfn $"day 7, part 2: %d{part2}"