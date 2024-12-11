module AoC2024.Days.Day08.Solution

open System
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput = 
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """
   
let findAntennae (arr: char[,]) : (int * int) seq seq = 
    arr
    |> Array2DExt.toSeq
    |> Seq.filter (fun (_, _, c) -> c <> '.')
    |> Seq.map (fun (x, y, c) -> (c, (x, y)))
    |> Seq.groupBy fst
    |> Seq.map (fun (_, l) -> Seq.map snd l)
   
    
module PartOne = 
    let nodesFromTwoAntennae ((ax, ay), (bx, by)) : (int * int) seq =
        let dx, dy = (bx - ax, by - ay)
        [ (ax - dx, ay - dy); (bx + dx, by + dy) ]
        
    let solve (arr: char[,]) : int =
        let nodesFromAllAntennae (locs: (int * int) seq) : (int * int) seq =
            Seq.allPairs locs locs 
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.collect nodesFromTwoAntennae
      
        arr
        |> findAntennae 
        |> Seq.collect nodesFromAllAntennae
        |> Seq.filter (fun (x, y) -> Array2DExt.inBounds x y arr)
        |> Seq.distinct
        |> Seq.length

    let parseAndSolve = ParseInput.charArray2D >> Array2DExt.transpose >> solve

    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 14
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile "Days/Day08/input.txt"
        |> parseAndSolve
        |> should equal 293
       
        
module PartTwo = 
        
    let solve (arr: char[,]) : int =
        // Convert two points into a line represented by a slope and y-intercept
        let lineFromTwoAntennae ((ax: int, ay: int), (bx: int, by: int)) : (float * float) =
            let dx, dy = (bx - ax, by - ay)
            let m: float = (float dy) / (float dx)
            let b: float = -m * (float ax) + (float ay)
            (m, b) // y = mx + b
           
        // Given a list of points of like antennae, make a line from every pair of points
        let antennaeToLines (locs: (int * int) seq) : (float * float) seq =
            Seq.allPairs locs locs 
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.map lineFromTwoAntennae
            
        let lines =
            arr
            |> findAntennae  // all antennae
            |> Seq.collect antennaeToLines // lines made by all pairs of like frequency
            |> Seq.distinct
      
        // True if a point is on any line 
        let isOnAnyLine (x: int, y: int) : bool =
            let isOnLine (m, b) = 
                Math.Abs((float y) - (m * (float x) + b)) < 0.0001 // if it satisfies y=mx+b
                
            lines |> Seq.exists isOnLine
      
        // Check each grid coordinate to see if it exists on any of the lines
        Array2DExt.coords arr
        |> Seq.filter isOnAnyLine
        |> Seq.length 
       
    let parseAndSolve = ParseInput.charArray2D >> Array2DExt.transpose >> solve
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 34
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day08/input.txt"
        |> parseAndSolve
        |> should equal 934
        
