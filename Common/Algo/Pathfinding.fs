module AoC2024.Common.Algo.Pathfinding

open System.Collections.Immutable
open Microsoft.FSharp.Core

type 'TNode DistQueue = ImmutableSortedSet<int * 'TNode>
type 'TNode PathGraph when 'TNode: comparison = Map<'TNode, int * 'TNode list>

let shortestPaths (graph: 'TNode -> (int * 'TNode) seq) (start: 'TNode) : 'TNode PathGraph = 
     
    let insertAll (dists: 'TNode DistQueue) (items: (int * 'TNode) seq) : 'TNode DistQueue = 
        items |> Seq.fold (_.Add) dists
       
    let pop (dists: 'TNode DistQueue) : (int * 'TNode) * 'TNode DistQueue =
        let res = dists.Min
        res, dists.Remove res
    
    // https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/ 
    let rec loop (queue: 'TNode DistQueue) (visited: Set<'TNode>) (results: 'TNode PathGraph) =
        let notVisited m = Set.contains m visited |> not
       
        if queue.IsEmpty then
            results
        else
         
        // Find the lowest cost position that hasn't been visited  
        let (dist, pos), queue = pop queue
        
        // Mark this position as visited 
        let visited = Set.add pos visited
        
        // Find the neighbors with closer distances than we have seen before
        let better =
            graph pos // Find all neighboring positions
            |> Seq.filter (snd >> notVisited) // that haven't already been visited
            |> Seq.map (fun (d, p) -> dist + d, p) // calculating the cost
            |> Seq.choose (fun (newDist, p) ->
                match Map.tryFind p results with
                | None -> Some (p, (newDist, [ pos ]))
                | Some (dist, _) when dist < newDist -> Some (p, (newDist, [ pos ]))
                | Some (dist, rest) when dist = newDist -> Some (p, (newDist, pos :: rest))
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
    
    let q = ImmutableSortedSet<int * 'TNode>.Empty // tuples automatically sort on first by default o.O
    
    loop (q.Add (0, start)) Set.empty Map.empty
    
    

let tryShortestDist (graph: 'TNode -> (int * 'TNode) seq) (start: 'TNode) (exit: 'TNode) : int option =
    shortestPaths graph start
    |> Map.tryFind exit
    |> Option.map fst
    
let shortestDist (graph: 'TNode -> (int * 'TNode) seq) (start: 'TNode) (exit: 'TNode) : int =
    tryShortestDist graph start exit |> Option.get
