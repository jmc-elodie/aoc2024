module AoC2024.Days.Day01.Solution

open System
open NUnit.Framework
open FsUnit

open AoC2024.Util

let exampleInput = 
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """.Trim()

module PartOne = 
    let solve (left: int list) (right: int list) : int =
        let leftSorted = List.sort left
        let rightSorted = List.sort right
        
        Seq.zip leftSorted rightSorted
        |> Seq.map (fun (l, r) -> Math.Abs(l - r))
        |> Seq.sum
        
    let parseAndSolve = ParseInput.intCols2 >> uncurry solve

    [<Test>]
    let ``example input`` () =
        exampleInput
        |> parseAndSolve 
        |> should equal 11
     
    [<Test>]
    let ``problem input`` () =
        readTextFromFile @"Days\Day01\part1_input.txt"
        |> parseAndSolve
        |> should equal 2031679
 

module PartTwo = 
    let solve (left: int list) (right: int list) : int =
        let rightIndexed = right |> List.countBy id |> Map.ofList
       
        let folder (state: int) (value: int) =
            let occurrences = 
                Map.tryFind value rightIndexed 
                |> Option.defaultValue 0
                
            state + value * occurrences
        
        left |> Seq.fold folder 0
        
    let parseAndSolve = ParseInput.intCols2 >> uncurry solve

    [<Test>]
    let ``example input`` () =
        exampleInput
        |> parseAndSolve 
        |> should equal 31
     
    [<Test>]
    let ``problem input`` () =
        readTextFromFile @"Days\Day01\part2_input.txt"
        |> parseAndSolve
        |> should equal 19678534
