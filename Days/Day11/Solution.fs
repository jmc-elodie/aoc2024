module AoC2024.Days.Day11.Solution

open System
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput = "125 17"

module PartOneAndTwo =
    
    let solve (numBlinks: int) (stones: string list) : int64 =
        
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
           
        let updateOneStoneMap (input: Map<string, int64>) (n: string) (count: int64) : Map<string, int64> =
            let updateCountMap m n = Map.change n (fun x -> Some ((Option.defaultValue 0L x) + count)) m
            updateOneStone n |> Seq.fold updateCountMap input
               
        let blink = Map.fold updateOneStoneMap Map.empty
      
        stones
        |> Seq.fold (fun m n -> Map.add n 1L m) Map.empty // Convert stone list into count map
        |> SeqExt.foldTimes numBlinks blink  // blink n times
        |> Map.values // get the counts and sum them
        |> Seq.sum
       
    let parseAndSolve n = ParseInput.words >> List.head >> solve n
        
    [<Test>]
    let ``p1: example input``() =
        exampleInput
        |> parseAndSolve 25
        |> should equal 55312
        
    [<Test>]
    let ``p1: problem input``() =
        readTextFromFile @"Days/Day11/input.txt"
        |> parseAndSolve 25
        |> should equal 229043
        
    [<Test>]
    let ``p2: problem input``() =
        readTextFromFile @"Days/Day11/input.txt"
        |> parseAndSolve 75
        |> should equal 272673043446478L
