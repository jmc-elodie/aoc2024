module AoC2024.Days.Day16.Solution

open AoC2024.Common.Algo
open AoC2024.Util
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open NUnit.Framework
open FsUnit

let example1 =
    """
    ###############
    #.......#....E#
    #.#.###.#.###.#
    #.....#.#...#.#
    #.###.#####.#.#
    #.#.#.......#.#
    #.#.#####.###.#
    #...........#.#
    ###.#.#####.#.#
    #...#.....#.#.#
    #.#.#.###.#.#.#
    #.....#...#.#.#
    #.###.#.#.#.#.#
    #S..#.....#...#
    ###############
    """
    
let example2 =
    """
    #################
    #...#...#...#..E#
    #.#.#.#.#.#.#.#.#
    #.#.#.#...#...#.#
    #.#.#.#.###.#.#.#
    #...#.#.#.....#.#
    #.#.#.#.#.#####.#
    #.#...#.#.#.....#
    #.#.#####.#.###.#
    #.#.#.......#...#
    #.#.###.#####.###
    #.#.#...#.....#.#
    #.#.#.#####.###.#
    #.#.#.........#.#
    #.#.#.#########.#
    #S#.............#
    #################
    """
  
type Move = Direction * Point

// Find possible exit states 
let findExits grid exit =
    // an exit direction is valid if its opposite neighbor is not a #
    let isValid d = Direction.toPoint d |> Tuple.sub exit |> CharGrid.charAt grid |> (fun c -> c <> '#')
    [ N; S; E; W ] |> List.filter isValid |> List.map (fun d -> d, exit)
    
let neighbors (grid: CharGrid) (facing: Direction, pos: Point) : (int * Move) seq =
    
    // Calculate the cost of turning to a point 
    let turnCost (d: Direction, _: Point) = if d = facing then 0 else 1000

    [ facing; Direction.clockwiseOf facing; Direction.clockwiseOf facing |> Direction.opposite ]
    |> Seq.map (fun d -> d, Direction.toPoint d |> Tuple.add pos )
    |> Seq.filter (fun (_, p) -> (CharGrid.charAt grid p) <> '#')
    |> Seq.map (fun m -> 1 + turnCost m, m)
        
module PartOne =

    let solve (grid: CharGrid) : int =
        let start = Array2DExt.findIndexOf 'S' grid
        let exit = Array2DExt.findIndexOf 'E' grid
      
        let exits = (findExits grid exit)
       
        let graph = neighbors grid 
        let paths = Pathfinding.shortestPaths graph (E, start)
        exits |> Seq.map (fun i -> Map.find i paths |> fst) |> Seq.min
        
    
    [<Test>]
    let ``example 1 input``() =
        example1
        |> CharGrid.fromString
        |> solve
        |> should equal 7036
 
    [<Test>]
    let ``example 2 input``() =
        example2
        |> CharGrid.fromString
        |> solve
        |> should equal 11048
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day16/input.txt"
        |> CharGrid.fromString
        |> solve
        |> should equal 83432 

module PartTwo =
    
    let solve (grid: CharGrid) : int =
        let start = Array2DExt.findIndexOf 'S' grid
        let exit = Array2DExt.findIndexOf 'E' grid
        
        let exits = (findExits grid exit)
        
        let rec walkPathsBackwards (graph: Map<Move, int * Move list>) (move: Move) : Set<Point> =
            let _, p = move
            if p = start then
                Set.add p Set.empty
            else
                
            let _, nextMoves = Map.find move graph
            
            let rest =
                nextMoves
                |> Seq.map (walkPathsBackwards graph)
                |> Set.unionMany
            
            Set.add p rest
           
        let graph = neighbors grid 
        let shortestPaths = Pathfinding.shortestPaths graph (E, start)
        let bestExits = 
            exits |> Seq.map (fun i -> (Map.find i shortestPaths |> fst), i) |> Seq.sortBy fst |> Seq.toList
        
        let shortestDist = List.head bestExits |> fst
            
        let exitPaths = bestExits |> Seq.filter (fun (d, _) -> d = shortestDist) |> Seq.map (snd >> (walkPathsBackwards shortestPaths)) |> Set.unionMany
                    
        (exitPaths |> Set.count)
            
    [<Test>]
    let ``example 1 input``() =
        example1
        |> CharGrid.fromString
        |> solve
        |> should equal 45
 
    [<Test>]
    let ``example 2 input``() =
        example2
        |> CharGrid.fromString
        |> solve
        |> should equal 64
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day16/input.txt"
        |> CharGrid.fromString
        |> solve
        |> should equal 467 
