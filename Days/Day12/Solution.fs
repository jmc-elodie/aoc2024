module AoC2024.Days.Day12.Solution

open System
open AoC2024.Days.Day06.Solution.Game
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


module PartTwo =
  
    let example4 =
        """
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
        """
        
    let example5 =
        """
        AAAAAA
        AAABBA
        AAABBA
        ABBAAA
        ABBAAA
        AAAAAA
        """
    
    let solve (garden: char[,]) : int =
        let mutable isPlotYet: bool[,] = Array2DExt.dims garden ||> Array2D.zeroCreate
        
        let rec grabRegion (c: char) (x: int, y: int) : (int * int) =
            if isPlotYet[x, y] then (0, 0) else
               
            isPlotYet[x,y] <- true
            
            let checkNeighbor (rx, ry) : bool =
                let x, y = rx + x , ry + y
                (Array2DExt.inBounds x y garden) && (garden[x,y] = c)
                
            let neighborLocs =
                Seq.allPairs [-1..1] [-1..1]
                |> Seq.except [ (0, 0) ]
                |> Seq.filter checkNeighbor
                |> Set.ofSeq
          
            // A location is a corner if it either has 2 adjacent sides with no direct neighbors
            // ...
            // .CC - Outside corner
            // or that side has both direct neighbors and no diagonal neighbor
            // ..C
            // .CC - Inside corner
            let isCorner (lr, tb) : bool =
                let direct = Set.ofList [ (lr, 0); (0, tb) ]
                let withDiag = Set.add (lr, tb) direct
                
                let isOutsideCorner =
                    neighborLocs
                    |> Set.intersect direct
                    |> Set.isEmpty
                
                let isInsideCorner =
                    neighborLocs
                    |> Set.intersect withDiag
                    |> (fun s -> s = direct)
            
                isOutsideCorner || isInsideCorner
         
            let corners =
                Seq.allPairs [-1..1] [-1..1]
                |> Seq.filter (fun (x, y) -> x <> 0 && y <> 0)
                |> Seq.filter isCorner
                |> Seq.length
                
            neighborLocs
            |> Seq.filter (fun (x,y) -> x = 0 || y = 0) // filter out diagonal neighbors
            |> Seq.map (Tuple.add (x, y)) // relative to absolute locations
            |> Seq.map (grabRegion c) // recurse to gobble up the rest of the region
            |> Seq.fold Tuple.add (1, corners) // Each loc is 1 area and numCorners sides
           
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
        |> should equal 80
     
    [<Test>]
    let ``example 2 input``() =
        example2
        |> parseAndSolve
        |> should equal 436
     
    [<Test>]
    let ``example 3 input``() =
        example3
        |> parseAndSolve
        |> should equal 1206
     
    [<Test>]
    let ``example 4 input``() =
        example4
        |> parseAndSolve
        |> should equal 236
    
    [<Test>]
    let ``example 5 input``() =
        example5
        |> parseAndSolve
        |> should equal 368
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day12/input.txt"
        |> parseAndSolve
        |> should equal 904114
