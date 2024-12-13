module AoC2024.Days.Day11.Solution

open System
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput = "125 17"

let parseStones = ParseInput.int64Array >> List.head >> Array.toList


module PartOne =
    
    let solve (stones: int64 list) : int =
        let isEven (n: int64) = (n % 2L) = 0
        let numDigits (n:int64) = n |> float |> log10 |> floor |> (fun n -> (int64 n) + 1L)
        let hasEvenDigits n = numDigits n |> isEven
        let splitStone (n: int64) =
            let nStr = n.ToString()
            let half = nStr.Length / 2
            [ Int64.Parse(nStr[0..(half - 1)]); Int64.Parse(nStr[half..]) ]
            
        let updateOneStone = function
            | 0L -> [ 1L ]
            | n when (hasEvenDigits n) -> splitStone n
            | n -> [ n * 2024L ]
            
        let blinkOnce (stones: int64 list) : int64 list = List.collect updateOneStone stones
        
        [1..25] |> Seq.fold (fun s _ -> blinkOnce s) stones |> Seq.length
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseStones
        |> solve
        |> should equal 55312
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day10/input.txt"
        |> parseStones
        |> solve
        |> should equal 55312
