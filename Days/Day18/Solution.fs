module AoC2024.Days.Day18.Solution

open System
open AoC2024.Util
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsUnit
open AoC2024.Common.Algo

let exampleInput =
    """
    5,4
    4,2
    4,5
    3,0
    2,1
    6,3
    2,4
    1,5
    0,6
    3,3
    2,6
    5,1
    1,2
    5,5
    2,5
    6,5
    1,4
    0,4
    6,4
    1,1
    6,1
    1,0
    0,5
    1,6
    2,0
    """
   
let parsePoints (str: string) : Point seq =
    let parseLine (line: string) : Point =
        line.Split(',', 2) |> Array.map Int32.Parse |> Tuple.ofArray2
        
    str |> ParseInput.lines |> Seq.map parseLine

// A weighted graph of the grid where each node is a position connected to its neighbors.
// Weights are distance (1)
let gridGraph (grid: CharGrid) (pos: Point) : (int * Point) seq = 
    [ N; E; W; S ]
    |> Seq.map (fun d -> Direction.toPoint d |> Tuple.add pos )
    |> Seq.filter (fun (x, y) -> (Array2DExt.inBounds x y grid) && (CharGrid.charAt grid (x, y)) <> '#')
    |> Seq.map (fun p -> 1, p)


module PartOne =
    
    let solve (width: int) (height: int) (t: int) (points: Point seq) : int =
        let grid = CharGrid.withOverlay (Array2D.create width height '.') '#' (Seq.take t points)
        let start = (0, 0)
        let exit = (width - 1, height - 1)
            
        Pathfinding.shortestDist (gridGraph grid) start exit 
        
    [<Test>] 
    let ``example input``() =
        exampleInput
        |> parsePoints
        |> solve 7 7 12
        |> should equal 22
        
    [<Test>] 
    let ``problem input``() =
        readTextFromFile @"Days/Day18/input.txt"
        |> parsePoints
        |> solve 71 71 1024
        |> should equal 284

    
module PartTwo =
    
    let solve (width: int) (height: int) (t: int) (points: Point seq) : Point =
        let grid = CharGrid.withOverlay (Array2D.create width height '.') '#' (Seq.take t points)
        let start = (0, 0)
        let exit = (width - 1, height - 1)
            
        let addWallAndTry (grid: CharGrid) (p: Point) : Point option * CharGrid =
            let grid = CharGrid.withOverlay grid '#' [ p ]
            
            // Return Some p if we *cannot* find a path, else return None
            let a =
                Pathfinding.tryShortestDist (gridGraph grid) start exit
                |> (function 
                    | Some _ -> None
                    | None -> Some p)
                
            a, grid
       
        Seq.skip t points
        |> Seq.mapFold addWallAndTry grid
        |> fst
        |> Seq.choose id
        |> Seq.head
        
    [<Test>] 
    let ``example input``() =
        exampleInput
        |> parsePoints
        |> solve 7 7 12
        |> should equal (6,1)
        
    [<Test>] 
    let ``problem input``() =
        readTextFromFile @"Days/Day18/input.txt"
        |> parsePoints
        |> solve 71 71 1024
        |> should equal (51,50)
