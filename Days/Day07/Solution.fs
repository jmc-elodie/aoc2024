module AoC2024.Days.Day07.Solution

open System.Text.RegularExpressions
open AoC2024.Util
open NUnit.Framework
open FsUnit
open System
open Nessos.Streams

let exampleInput = 
    """ 
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """
    
let parseEquations (str: string) =
    let splitter = function
        | x :: xs -> (x, xs)
        | _ -> failwith "splitter: invalid input"
    
    Regex.Replace(str, @":", "")
    |> ParseInput.int64Array 
    |> Seq.map Array.toList
    |> Seq.map splitter


// Given a sequence of pairs of operators and numbers, compute the total
let computeOperators (args: (char * int64) seq) : int64 =
    let accumulateOp (sum: int64) (op: char, n: int64) : int64 =
        match op with
        | '+' -> sum + n
        | '*' -> sum * n
        | '|' -> Int64.Parse $"%d{sum}%d{n}" 
        
        | _ -> failwith "todo"
        
    args
    |> Seq.fold accumulateOp 0
    

let checkPossibleOperators (validOperators: char list) (mustTotal: int64, numbers: int64 list) : int64 =
    // Given a list of operators, apply them to the numbers and if the sum matches the total
    // return it else 0 
    let checkOperators (ops: char list) : bool =
        assert (ops.Length = numbers.Length)
            
        let args = Seq.zip ops numbers
        let total = computeOperators args
            
        total = mustTotal
   
    let isValid =
        permuteN (numbers.Length - 1) validOperators
        |> Seq.map (fun l -> '+' :: l)
        |> Seq.exists checkOperators
        
    if isValid then mustTotal else 0
    
    
module PartOne =
    
    let solve (equations: (int64 * int64 list) seq) : int64 =
        equations 
        |> Seq.map (checkPossibleOperators [ '+'; '*' ])
        |> Seq.sum
       
    let parseAndSolve = parseEquations >> solve
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 3749
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day07/input.txt"
        |> parseAndSolve
        |> should equal 882304362421L


module PartTwo = 

    let solve (equations: (int64 * int64 list) seq) : int64 =
        equations
        |> ParStream.ofSeq
        |> ParStream.map (checkPossibleOperators [ '+'; '*'; '|' ])
        |> ParStream.sum
       
    let parseAndSolve = parseEquations >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 11387
    
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day07/input.txt"
        |> parseAndSolve
        |> should equal 145149066755184L
