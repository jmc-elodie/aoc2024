module AoC2024.Days.Day10.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """

let parseTrailMap = CharGrid.fromString >> Array2D.map ParseInput.atoi

module PartOne =
    
    let solve (trailMap: int[,]) : int =
        let isValidLoc x y = Array2DExt.inBounds x y trailMap
        
        // Check the neighbor and return Some (x,y) if they are the desired value 
        let checkNeighbor h (cx, cy) (rx, ry) : (int * int) option =
            let x, y = cx + rx, cy + ry
            if (isValidLoc x y) && (trailMap[x, y] = h) then
                Some (x, y)
            else
                None
               
        let rec findNinesOnPath (x: int, y: int) : (int * int) seq =
            seq {
                let h = trailMap[x, y]
                
                if h = 9 then
                    yield (x,y)
                else
                    
                yield!
                    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
                    |> Seq.choose (checkNeighbor (h + 1) (x, y))
                    |> Seq.collect findNinesOnPath
            }
        
        trailMap
        |> Array2DExt.findIndexesOf 0 // 0s are the trailheads
        |> Seq.map ( // count the distinct locations of 9 along the path from each head
            findNinesOnPath
            >> Seq.distinct
            >> Seq.length)
        |> Seq.sum // sum these
        
    let parseAndSolve = parseTrailMap >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 36
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day10/input.txt"
        |> parseAndSolve
        |> should equal 538

module PartTwo =
    
    let solve (trailMap: int[,]) : int =
        let isValidLoc x y = Array2DExt.inBounds x y trailMap
        
        // Check the neighbor and return Some (x,y) if they are the desired value 
        let checkNeighbor h (cx, cy) (rx, ry) : (int * int) option =
            let x, y = cx + rx, cy + ry
            if (isValidLoc x y) && (trailMap[x, y] = h) then
                Some (x, y)
            else
                None
               
        let rec findNinesOnPath (x: int, y: int) : int = 
            let h = trailMap[x, y]
            
            if h = 9 then 1 else
                
            [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
            |> Seq.choose (checkNeighbor (h + 1) (x, y))
            |> Seq.sumBy findNinesOnPath
        
        trailMap
        |> Array2DExt.findIndexesOf 0 // 0s are the trailheads
        |> Seq.sumBy findNinesOnPath 
        
    let parseAndSolve = parseTrailMap >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 81
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day10/input.txt"
        |> parseAndSolve
        |> should equal 1110
