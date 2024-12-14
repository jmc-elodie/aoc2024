module AoC2024.Days.Day12.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit

let example1 =
    """
    AAAA
    BBCD
    BBCC
    EEEC
    """
    
let example2 =
   """
    OOOOO
    OXOXO
    OOOOO
    OXOXO
    OOOOO
    """
    
let example3 =
    """
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """
    
module PartOne =
   
    let solve (garden: char[,]) : int =
        let mutable isPlotYet: bool[,] = Array2DExt.dims garden ||> Array2D.zeroCreate // Track every cell to see if its used
        
        let rec grabRegion (c: char) (x: int, y: int) : (int * int) =
            if isPlotYet[x, y] then (0, 0) else
               
            isPlotYet[x,y] <- true
            
            let neighbors =
                garden
                |> Array2DExt.filterNeighbors (fun v -> v = c) (x, y)
                |> Seq.map fst
                |> Seq.toList
            
            neighbors
            |> Seq.map (grabRegion c)
            |> Seq.fold Tuple.add (1, 4 - neighbors.Length)
           
           
        garden
        |> Array2DExt.toSeq
        |> Seq.map (fun (x, y, v) -> grabRegion v (x, y))
        |> Seq.map (fun (a, p) -> a * p)
        |> Seq.sum
        
        
    let parseAndSolve = CharGrid.fromString >> solve
     
    [<Test>]
    let ``example 1 input``() =
        example1
        |> parseAndSolve
        |> should equal 140
     
    [<Test>]
    let ``example 2 input``() =
        example2
        |> parseAndSolve
        |> should equal 772
     
    [<Test>]
    let ``example 3 input``() =
        example3
        |> parseAndSolve
        |> should equal 1930
    
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day12/input.txt"
        |> parseAndSolve
        |> should equal 1461752
