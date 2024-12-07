module AoC2024.Days.Day05.Solution

open System
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """

// Parse the input file into two list, a list of int tuples for the rules and a list of ints for the updates
let parsePrintUpdates (str: string) =
    let parseRuleLine (line: string) : int * int =
        line.Split('|', 2)
        |> Array.map Int32.Parse
        |> Tuple.ofArray2
        
    let parseUpdateLine (line: string) : int list =
        line.Split(',')
        |> Array.toList
        |> List.map Int32.Parse
        
    let rulesLines, updatesLines = 
        str
        |> ParseInput.strings
        |> List.filter (fun s -> s.Length > 0)
        |> List.partition (_.Contains('|'))
        
    let rules = List.map parseRuleLine rulesLines
    let updates = List.map parseUpdateLine updatesLines
    
    (rules, updates)
    

let rec checkOrder rulesIndex = function
    | [] -> true
    | x :: xs ->
        let adj = Graph.getDefault x Set.empty rulesIndex
        let isOutOfOrder =
            xs
            |> Seq.exists (fun p -> Set.contains p adj)
            |> not
        
        isOutOfOrder && (checkOrder rulesIndex xs)
        
// Take the rules and build a graph of pages and their dependencies
// Convert this list to a map of page -> sort rank in the sorted list
let createIndex = List.map Tuple.flip >> Graph.create
   
    
module PartOne =
 
    let solve (rules: (int * int) list) (updates: int list list) =
        // Take the rules and build a graph of pages and their dependencies
        // Convert this list to a map of page -> sort rank in the sorted list
        let rulesIndex = createIndex rules
  
        // Lookup the pages in the index and return the middle page number if all pages in the job are
        // ordered correctly
        let lookupAndReduce (pages: int list) : int =
            if checkOrder rulesIndex pages then
                pages[pages.Length / 2]
            else
                0
         
        updates 
        |> Seq.map lookupAndReduce
        |> Seq.sum
        
    let parseAndSolve = parsePrintUpdates >> (uncurry solve)
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 143
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day05/input.txt"
        |> parseAndSolve
        |> should equal 4959


module PartTwo =
 
    let solve (rules: (int * int) list) (updates: int list list) =
        // Take the rules and build a graph of pages and their dependencies
        // Convert this list to a map of page -> sort rank in the sorted list
        let rulesIndex = createIndex rules
  
        let sortPages (pages: int list) : int list = Graph.topologicalSort rulesIndex pages
        
        // Lookup the pages in the index and return the middle page number if all pages in the job are
        // ordered correctly
        let lookupAndReduce (pages: int list) : int =
            if checkOrder rulesIndex pages then
                0
            else
                
            let sorted = sortPages pages
            sorted[sorted.Length / 2]
         
        updates 
        |> Seq.map lookupAndReduce
        |> Seq.sum

    let parseAndSolve = parsePrintUpdates >> (uncurry solve)
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 123
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day05/input.txt"
        |> parseAndSolve
        |> should equal 4655
