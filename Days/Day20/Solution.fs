module AoC2024.Days.Day20.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput =
    """
    ###############
    #...#...#.....#
    #.#.#.#.#.###.#
    #S#...#.#.#...#
    #######.#.#.###
    #######.#.#...#
    #######.#.###.#
    ###..E#...#...#
    ###.#######.###
    #...###...#...#
    #.#####.#.###.#
    #.#...#.#.#...#
    #.#.#.#.#.#.###
    #...#...#...###
    ###############
    """

let parseTrack (str: string) : CharGrid * Point * Point =
    let grid = ParseInput.charArray2D str
    let start = Array2DExt.findIndexOf 'S' grid
    let exit = Array2DExt.findIndexOf 'E' grid

    (grid, start, exit)


let neighbors grid p =
    [ N; E; W; S ]
    |> Seq.choose (CharGrid.tryNext grid p)


let findTrack grid start exit =
    let rec loop prev acc = function
        | p when p = exit -> p :: acc
        | p ->
            let next =
                neighbors grid p
                |> Seq.filter (fst >> Pred.notEquals '#')
                |> Seq.map snd
                |> Seq.find (fun p -> p <> prev)

            loop p (p :: acc) next

    loop (-1, -1) [] start


module PartOne =

    let solve (threshold: int) (grid: CharGrid) (start: Point) (exit: Point) : int =
        let roadNeighbor prev p =
            neighbors grid p
            |> Seq.filter (snd >> Pred.notEquals prev)
            |> Seq.tryFind (fst >> Pred.notEquals '#')
            |> Option.map snd

        let skippableWalls p =
            neighbors grid p
            |> Seq.filter (fst >> Pred.equals '#')
            |> Seq.choose (snd >> roadNeighbor p)
            |> Seq.map (fun n -> p, n)

        let track = findTrack grid start exit

        let dists =
            track
            |> Seq.mapi (fun i p -> p, i)
            |> Map.ofSeq

        track
        |> Seq.collect skippableWalls
        |> Seq.map (fun (a, b) ->
            let da = Map.find a dists
            let db = Map.find b dists
            db - da - 2)
        |> Seq.filter (fun d -> d >= threshold)
        |> Seq.length

    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseTrack
        |> uncurry3 (solve 4)
        |> should equal (14 + 2 + 4 + 2 + 3 + 5)

    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day20/input.txt"
        |> parseTrack
        |> uncurry3 (solve 100)
        |> should equal 1286


module PartTwo =

    let solve (threshold: int) (grid: CharGrid) (start: Point) (exit: Point) : int =

        let dist (ax, ay) (bx, by) =
            abs (bx - ax) + abs (by - ay)

        // The set of all relative points reachable within the skip distance
        let generateSearchMask len =
            Seq.allPairs [ (-len)..(len) ] [ (-len)..(len) ]
            |> Seq.filter (fun (x,y) ->
                let d = (abs x + abs y)
                d >= 2 && d <= len)
            |> Seq.toList

        let walls = Array2D.map (Pred.notEquals '#') grid

        let searchMask = generateSearchMask 20

        // find unique reachable roads from a given point
        let reachableRoads p =
            searchMask
            |> Seq.map (Tuple.add p)
            |> Seq.filter (fun p ->
                match Array2DExt.tryGet walls p with
                | None -> false
                | Some b -> b)
            |> Seq.map (fun a -> p, a)

        let track = findTrack grid start exit

        let dists =
            track
            |> Seq.mapi (fun i p -> p, i)
            |> Map.ofSeq

        track
        |> Seq.collect reachableRoads
        |> Seq.map (fun (a, b) ->
            let da = Map.find a dists
            let db = Map.find b dists
            db - da - (dist a b))
        |> Seq.filter (fun d -> d >= threshold)
        |> Seq.length


    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseTrack
        |> uncurry3 (solve 50)
        |> should equal 285

    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day20/input.txt"
        |> parseTrack
        |> uncurry3 (solve 100)
        |> should equal 989316
