module AoC2024.Days.Day16.Solution

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
    
module Direction =
    let opposite = function
        | N -> S
        | W -> E
        | E -> W
        | S -> N
    
    let clockwiseOf = function
        | N -> E
        | E -> S
        | S -> W
        | W -> N
module PartOne =

    type DistQueue = ImmutableSortedSet<int * Move>
    
    let solve (grid: CharGrid) : int =
        let start = Array2DExt.findIndexOf 'S' grid
        let exit = Array2DExt.findIndexOf 'E' grid
       
        let charAt (p: Point) =
            let x, y = p
            grid[x,y]
         
        let neighbors (facing: Direction) (pos: Point) : (Direction * Point) seq =
            [ facing; Direction.clockwiseOf facing; Direction.clockwiseOf facing |> Direction.opposite ]
            |> Seq.map (fun d -> d, dirToPoint d |> Tuple.add pos )
            |> Seq.filter (fun (_, p) -> (charAt p) <> '#')
            
        let insertAll (items: (int * Move) seq) (dists: DistQueue) : DistQueue = 
            items |> Seq.fold (_.Add) dists
           
        let pop (dists: DistQueue) : (int * Move) * DistQueue =
            let res = dists.Min
            res, dists.Remove res
        
        // Find possible exit states 
        let exits =
            // an exit direction is valid if its opposite neighbor is not a #
            let isValid d = dirToPoint d |> Tuple.sub exit |> charAt |> (fun c -> c <> '#')
            [ N; S; E; W ] |> List.filter isValid |> List.map (fun d -> d, exit)
        
        // https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/ 
        let rec cheapestPath (dists: DistQueue) (visited: Map<Move, int>) : int =
            let notVisited m = Map.containsKey m visited |> not
            
            // find the lowest cost position that hasn't been visited  
            let (cost, (facing, pos)), dists = pop dists
            
            // Mark this position as visited 
            let visited = Map.add (facing, pos) cost visited
            
            let visitedExits = exits |> List.choose (fun m -> Map.tryFind m visited)
            if visitedExits.Length = exits.Length then
                List.min visitedExits
            else
            
            // Calculate the cost of turning to a point 
            let turnCost (d: Direction, _: Point) = if d = facing then 0 else 1000
           
            let updates =
                neighbors facing pos // Find all neighboring positions
                |> Seq.filter notVisited // that haven't already been visited
                |> Seq.map (fun l -> cost + 1 + turnCost l, l) // calculating the cost
                
            let dists = insertAll updates dists // update the dist tree
            
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
        |> should equal 83432 
