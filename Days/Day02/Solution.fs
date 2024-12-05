module AoC2024.Days.Day02.Solution

open System
open AoC2024.Util
open NUnit.Framework
open FsUnit


let exampleInput =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """.Trim()

module PartOne =
    
    let hasUnsafeLevels (row: int array) : bool =
        if row.Length < 2 then
            failwith $"invalid row, length %d{row.Length} < 2"
        else
            
        let isIncreasing = row[0] < row[1]
    
        let checkLevelSafety (prev: int, cur: int) : bool =
            let isWithDirection = if isIncreasing then prev < cur else prev > cur
            let difference = Math.Abs(prev - cur)
            not isWithDirection || 1 > difference || difference > 3
            
        row |> Seq.pairwise |> Seq.exists checkLevelSafety
            
    let solve (reports: int array list) : int =
        let oneIfSafe row : int =
            if hasUnsafeLevels row then 0 else 1
      
        reports |> Seq.sumBy oneIfSafe
        
    let parseAndSolve = ParseInput.intArray >> solve


    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 2
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day02\input.txt"
        |> parseAndSolve
        |> should equal 371
    
        
module PartTwo =
    
    // Sequence of arrays where each array is the input array minus a single element
    // If the input is [| 1; 2; 3 |] the output would be:
    //   [| 2; 3 |]
    //   [| 1; 3 |]
    //   [| 1; 2 |]
    let missingEach (row: int array) =
        seq {
            for i in 0 .. (row.Length - 1) do
                let head, tail = Array.splitAt i row
                yield
                    Array.skip 1 tail
                    |> Array.append head
        }
        
    let hasSafeIfIgnoreOneLevel (row: int array) : bool =
        let hasSafeLevels = PartOne.hasUnsafeLevels >> not
        
        if hasSafeLevels row then
            true
        else
        
        missingEach row |> Seq.exists hasSafeLevels
        
    let solve (reports: int array list) : int =
        let oneIfSafe row : int =
            if hasSafeIfIgnoreOneLevel row then 1 else 0
      
        reports |> Seq.sumBy oneIfSafe
        
    let parseAndSolve = ParseInput.intArray >> solve

    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 4
    
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day02\input.txt"
        |> parseAndSolve
        |> should equal 426
