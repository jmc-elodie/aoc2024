﻿module AoC2024.Util

open FsUnit
open System
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework

let tryReadTextFromFile relativePath =
    let filePath = Path.Combine(TestContext.CurrentContext.TestDirectory, relativePath)
    try 
        let inputStr = File.ReadAllText(filePath)
        Ok inputStr
    with
    | ex -> Error ex
    
let readTextFromFile relativePath =
    let filePath = Path.Combine(TestContext.CurrentContext.TestDirectory, relativePath)
    try 
        File.ReadAllText(filePath)
    with
    | ex -> failwith $"Failed to read file: %s{ex.ToString()}"
  
let getValueOrFail (res: Result<'T, 'TError>) : 'T =
    match res with
    | Ok value -> value
    | Error err -> failwith $"Result was error: %s{err.ToString()}"

let uncurry f (a,b) = f a b

let rec permuteN (n: int) (set: 'a list) : 'a list list =
    assert (n >= 0)
    match n, set with
    | 0, _ ->  []
    | _, [] -> []
    | 1, _ ->  List.map (fun x -> [ x ]) set
    | _ ->
        permuteN (n - 1) set
        |> List.allPairs set
        |> List.map (fun (x,xs) -> x :: xs) 

module ParseInput =
    // Parses an input string into a list of strings
    let strings (inputStr: string) : string list =
        inputStr.Trim().Split("\n") 
        |> Array.toList
        |> List.map (_.Trim())
      
    // Parses an input string into 2 int32 columns and returns a tuple of those columns as lists
    let intCols2 (inputStr: string) : int list * int list =
        let parseLine (s: string) =
            let parts = Regex.Split(s, @"\s+") |> Array.map Int32.Parse
            (parts[0], parts[1])
        
        strings inputStr    
        |> List.map parseLine 
        |> List.unzip

    // Parses an input string into a list of int arrays
    let intArray (inputStr: string) : int array list =
        let parseLine (s:string) =
            Regex.Split(s, @"\s+")
            |> Array.map Int32.Parse
            
        inputStr
        |> strings
        |> List.map parseLine

    let charArray2D (inputStr: string) : char[,] =
        let parseLine (s:string) = s.ToCharArray()
        
        let lines =
            inputStr
            |> strings
            |> List.map parseLine
            |> List.toArray
        
        let numLines = lines.Length 
        assert (numLines > 0)
        
        let rowLen = lines[0].Length
        assert (rowLen > 0)
        
        Array2D.init numLines rowLen (fun y x -> lines[y][x])
        
module Array2DExt =

    let foldi (folder: 'TState -> int -> int -> 'T -> 'TState) (state: 'TState) (arr: 'T[,]) : 'TState =
        let mutable _state = state
        let updateState x y value =
            _state <- folder _state x y value
        Array2D.iteri updateState arr
        _state
        
    let inBounds x y arr =
        x >= 0 && x < (Array2D.length1 arr) && 
        y >= 0 && y < (Array2D.length2 arr)
        
    let transpose (arr: 'T[,]) : 'T[,] =
        Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun x y -> arr[y,x])
            
module SeqExt =
    
    // Given a sequence of bools, counts the number of true elements
    let countTrue (seq: seq<bool>) : int =
        Seq.fold (fun acc v -> if v then acc + 1 else acc) 0 seq
    
    // Given a sequence of bools, returns true if all elements are true 
    let all (seq: seq<bool>) : bool = Seq.contains false seq |> not

    let ofArray2D (arr: 'T[,]) : seq<'T> =
        seq {
            let count1 = Array2D.length1 arr
            let count2 = Array2D.length2 arr
            let b1 = Array2D.base1 arr
            let b2 = Array2D.base2 arr
            for i = b1 to b1 + count1 - 1 do
              for j = b2 to b2 + count2 - 1 do
                  yield arr[i,j]
        }

module Tuple =
    
    let flip (a,b) = (b,a)
    
    let ofArray2 (arr: 'a array) =
        assert (arr.Length = 2)
        (arr[0], arr[1])
        
        
// Utilities for creating and sorting a directed graph of values
// The graph is stored as a map of nodes to a set of connected nodes
module Graph =

    let getDefault (k: 'a) (def: 'b) (table: Map<'a, 'b>) : 'b =
        match Map.tryFind k table with
        | Some v -> v
        | None -> def
    
    // Creates a graph
    let create (edges: ('a * 'a) list) =
        let folder acc (key: 'a, other: 'a) =
            let adj = getDefault key Set.empty acc
            Map.add key (Set.add other adj) acc
                
        List.fold folder Map.empty edges
       
    // https://cp-algorithms.com/graph/topological-sort.html 
    let topologicalSort (dag: Map<'a,Set<'a>>) (input: seq<'a>) =
        let inputSet = Set.ofSeq input
        
        // Do a depth first traversal of the list keeping track of already visited elements
        // When an element hasn't been visited, put it in the resulting list
        // this function takes and returns its state so it can be folded over the nodes of the dag
        let rec dfs (visited: Set<'a>, sorted: 'a list) (item: 'a) : (Set<'a> * 'a list) =
            if Set.contains item visited then
                (visited, sorted)
            else
            
            let visited = Set.add item visited
            let children = getDefault item Set.empty dag |> Set.intersect inputSet
            let visited, sorted = Set.fold dfs (visited, sorted) children 
            (visited, item :: sorted)
           
        input
        |> Seq.fold dfs (Set.empty, [])
        |> snd