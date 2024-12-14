module AoC2024.Days.Day11.Solution

open System
open System.Collections.Generic
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput = "125 17"

module PartOne =
    
    let solve (stones: string list) : int64 =
        
        let hasEvenDigits (n: string) = (n.Length % 2) = 0
        
        let withoutLeadingZeroes (n: string) = 
            let trimmed = n.TrimStart('0')
            if trimmed.Length = 0 then "0" else trimmed
        
        let splitStone (n: string) =
            let half = n.Length / 2
            [ n[..(half - 1)]; withoutLeadingZeroes n[half..] ]
        
        let mulString (a: int64) (n: string) : string = (Int64.Parse(n) * a).ToString()
            
        let updateOneStone = function
            | "0" -> [ "1" ]
            | n when (hasEvenDigits n) -> splitStone n
            | n -> [ mulString 2024L n ]
           
        // let mutable memo: Map<string, string list> = Map.empty
        // let memoize f n =
        //     match (Map.tryFind n memo) with
        //     | Some a -> a
        //     | None -> 
        //         let result = f n
        //         memo <- Map.add n result memo
        //         result
        
        let updateOneStoneMap (input: Map<string, int64>) (n: string) : Map<string, int64> =
            let curCount = Map.find n input
            let updateCountMap m n = Map.change n (fun x -> Some ((Option.defaultValue 0L x) + curCount)) m
            updateOneStone n |> Seq.fold updateCountMap (Map.remove n input)
               
        let blink (stones: Map<string, int64>)  = 
            stones |> Map.keys |> Seq.fold updateOneStoneMap stones
      
        let stones = "1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32" |> ParseInput.words |> List.head
       
        let stonesMap = stones |> Seq.fold (fun m n -> Map.add n 1L m) Map.empty
        
        // [1..25] |> Seq.fold (fun s _ -> blink s) stonesMap |> Map.values |> Seq.sum
        blink stonesMap |> Map.values |> Seq.sum
       
    let parseAndSolve = ParseInput.words >> List.head >> solve
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 55312
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day11/input.txt"
        |> parseAndSolve
        |> should equal 229043