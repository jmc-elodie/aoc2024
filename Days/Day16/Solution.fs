module AoC2024.Days.Day16.Solution

open System.Collections.Generic
open System.Collections.Immutable
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
  
type Maze = {
    grid: CharGrid
    start: (int * int)
    exit: (int * int)
}
   
let parseMaze (str: string) : Maze =
    let grid = str |> CharGrid.fromString
    let start = Array2DExt.findIndexOf 'S' grid
    let exit = Array2DExt.findIndexOf 'E' grid
    {
        grid = grid
        start = start
        exit = exit
    }
    
type MazeLocation = {
    pos: (int * int)
    dir: Direction
}

type MazePath = MazeLocation list
   
let debugPrint (maze: Maze) (path: MazePath) =
    printfn $"start: %A{maze.start}, exit: %A{maze.exit}" 
    CharGrid.debugPrint maze.grid
   
type Point = (int * int)

type Move = Direction * Point

let dirToPoint (d: Direction) : Point =
    match d with
    | N -> (0, -1)
    | E -> (1, 0)
    | S -> (0, 1)
    | W -> (-1, 0)
    
let turnsBetween (a: Direction) (b: Direction) : int =
    let clockwise = [| N; E; S; W |]
    let ai = Array.findIndex (fun v -> v = a) clockwise
    let bi = Array.findIndex (fun v -> v = b) clockwise
    abs (ai - bi)
    
module PartOne =

    type DistQueue = ImmutableSortedSet<int * Move>
    
    let solve (grid: CharGrid) : int =
        let start = Array2DExt.findIndexOf 'S' grid
        let exit = Array2DExt.findIndexOf 'E' grid
       
        let charAt (p: Point) =
            let x, y = p
            grid[x,y]
         
        let neighbors (pos: Point) : (Direction * Point) seq =
            [ N; E; S; W ]
            |> Seq.map (fun d -> d, dirToPoint d)
            |> Seq.map (fun (d, p) -> d, Tuple.add pos p)
            |> Seq.filter (fun (_, p) -> (charAt p) <> '#')
       
        let removeAll (items: (int * Move) seq) (dists: DistQueue) : DistQueue = 
            items |> Seq.fold (_.Remove) dists
            
        let insertAll (items: (int * Move) seq) (dists: DistQueue) : DistQueue = 
            items |> Seq.fold (_.Add) dists
           
        let updateAll (updates: (int * Move) seq) (dists: DistQueue) (visited: Map<Move, int>) : DistQueue =
            let existingItems =
                updates
                |> Seq.choose (fun (_, m) ->
                    match Map.tryFind m visited with
                    | Some c -> Some (c, m)
                    | None -> None)
           
            let minItems = 
                updates
                |> Seq.map (fun (updatedCost, m) ->
                    let minCost = min updatedCost (Map.tryFind m visited |> Option.defaultValue updatedCost)
                    minCost, m)
             
            dists |> removeAll existingItems |> insertAll minItems
             
        // https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/ 
        let rec cheapestPath (dists: DistQueue) (visited: Map<Move, int>) : int =
            let notVisited m = Map.containsKey m visited |> not
            
            // find the lowest cost position that hasn't been visited  
            let cost, (facing, pos) = dists |> Seq.find (snd >> notVisited)
            
            // If the lowest cost next position is the exit, we're done
            if pos = exit then cost else
               
            // Mark this position as visited 
            let visited = Map.add (facing, pos) cost visited
               
            // Calculate the cost of turning to a point 
            let turnCost (d: Direction, p: Point) =
                let turns = turnsBetween facing d
                turns * 1000
           
            // Update dists by... 
            let dists =
                neighbors pos // Finding all neighboring positions
                |> Seq.filter notVisited // that haven't already been visited
                |> Seq.map (fun l -> cost + 1 + turnCost l, l) // calculating the cost
                |> (fun updates -> updateAll updates dists visited) // update the dist tree
            
            // Recurse to the next thing
            cheapestPath dists visited
        
        let q = ImmutableSortedSet<int * Move>.Empty
        cheapestPath (q.Add (0, (E, start))) Map.empty
    
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
        |> should equal 11048
        
        // 103432 too high
