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

    let nodesFromSlope (width, height) (m: float, b: float) : (int * int) seq =
        let isOnLine (x: int, y: int) = 
            Math.Abs((float y) - (m * (float x) + b)) < 0.0001 // if it satisfies y-intercept form...
            
        Seq.allPairs [ 0..(width - 1) ] [ 0..(height - 1) ]
        |> Seq.filter isOnLine
        
    let solve (arr: char[,]) : int =
        let lineFromTwoAntennae ((ax: int, ay: int), (bx: int, by: int)) : (float * float) =
            let dx, dy = (bx - ax, by - ay)
            let m: float = (float dy) / (float dx)
            let b: float = m * (float ax) - (float ay)
            (m, b)
            
        let antennaeToLines (locs: (int * int) seq) : (float * float) seq =
            Seq.allPairs locs locs 
            |> Seq.filter (fun (a, b) -> a <> b)
            |> Seq.map lineFromTwoAntennae
            
        arr
        |> findAntennae  // all antennae
        |> Seq.collect antennaeToLines // lines made by all pairs of like frequency
        |> Seq.distinct
        |> Seq.collect (nodesFromSlope (Array2DExt.dims arr)) // all nodes on those lines in bounds
        |> Seq.distinct
        |> tee (CharGrid.withOverlay arr '#' >> CharGrid.debugPrint)
        |> Seq.length // the number of distinct nodes
       
    let parseAndSolve = ParseInput.charArray2D >> Array2DExt.transpose >> tee CharGrid.debugPrint >> solve
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 34
        
