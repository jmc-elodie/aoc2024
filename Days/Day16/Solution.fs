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

type DistQueue = ImmutableSortedSet<int * Move>

let charAt (grid: CharGrid) (p: Point) =
    let x, y = p
    grid[x,y]
        
let shortestPaths (grid: CharGrid) (start: Move) : Map<Move, int * Move list> =
     
    let neighbors (facing: Direction, pos: Point) : (Direction * Point) seq =
        [ facing; Direction.clockwiseOf facing; Direction.clockwiseOf facing |> Direction.opposite ]
        |> Seq.map (fun d -> d, dirToPoint d |> Tuple.add pos )
        |> Seq.filter (fun (_, p) -> (charAt grid p) <> '#')
        
    let insertAll (dists: DistQueue) (items: (int * Move) seq) : DistQueue = 
        items |> Seq.fold (_.Add) dists
       
    let pop (dists: DistQueue) : (int * Move) * DistQueue =
        let res = dists.Min
        res, dists.Remove res
    
    // https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/ 
    let rec loop (queue: DistQueue) (visited: Set<Move>) (results: Map<Move, int * Move list>) =
        let notVisited m = Set.contains m visited |> not
       
        if queue.IsEmpty then
            results
        else
         
        // Find the lowest cost position that hasn't been visited  
        let (cost, move), queue = pop queue
        let (facing, pos) = move
        
        // Mark this position as visited 
        let visited = Set.add move visited
       
        // If we have found the shortest path to all exit states 
        // let visitedExits = exits |> List.choose (fun m -> Map.tryFind m results)
        // if visitedExits.Length = exits.Length then
        //     results
        // else
        
        // Calculate the cost of turning to a point 
        let turnCost (d: Direction, _: Point) = if d = facing then 0 else 1000
       
        let better =
            neighbors move // Find all neighboring positions
            |> Seq.filter notVisited // that haven't already been visited
            |> Seq.map (fun l -> cost + 1 + turnCost l, l) // calculating the cost
            |> Seq.choose (fun (newDist, m) ->
                match Map.tryFind m results with
                | None -> Some (m, (newDist, [ move ]))
                | Some (dist, _) when dist < newDist -> Some (m, (newDist, [ move ]))
                | Some (dist, rest) when dist = newDist -> Some (m, (newDist, move :: rest))
                | _ -> None)
        
        // Update the result graph with the new entries 
        let results = better |> Seq.fold (fun r (k, v) -> Map.add k v r) results
         
        // It's valid to just insert additional items instead of trying to update the values in the queue
        let queue =
            better
            |> Seq.map (fun (m, (d, _)) -> d, m)
            |> insertAll queue 
        
        // Recurse to the next thing
        loop queue visited results
    
    let q = ImmutableSortedSet<int * Move>.Empty // tuples automatically sort on first by default o.O
    loop (q.Add (0, start)) Set.empty Map.empty
   
// Find possible exit states 
let findExits grid exit =
    // an exit direction is valid if its opposite neighbor is not a #
    let isValid d = dirToPoint d |> Tuple.sub exit |> charAt grid |> (fun c -> c <> '#')
    [ N; S; E; W ] |> List.filter isValid |> List.map (fun d -> d, exit)
    
module PartOne =

    let solve (grid: CharGrid) : int =
        let start = Array2DExt.findIndexOf 'S' grid
        let exit = Array2DExt.findIndexOf 'E' grid
      
        let exits = (findExits grid exit)
        
        let graph = shortestPaths grid (E, start)
        exits |> Seq.map (fun i -> Map.find i graph |> fst) |> Seq.min
    
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
            
        let graph = shortestPaths grid (E, start)
        let bestExits = 
            exits |> Seq.map (fun i -> (Map.find i graph |> fst), i) |> Seq.sortBy fst |> Seq.toList
        
        let shortestDist = List.head bestExits |> fst
            
        let paths = bestExits |> Seq.filter (fun (d, _) -> d = shortestDist) |> Seq.map (snd >> (walkPathsBackwards graph)) |> Set.unionMany
                    
        (paths |> Set.count)
            
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
