﻿module AoC2024.Util

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open NUnit.Framework

let tryReadTextFromFile relativePath =
    let filePath = Path.Combine(TestContext.CurrentContext.TestDirectory, relativePath)
    try
        let inputStr = File.ReadAllText(filePath)
        Ok inputStr
    with
    | ex -> Error ex

let readTextFromFile relativePath =
    let filePath = Path.Combine(TestContext.CurrentContext.TestDirectory, relativePath)
    try
        File.ReadAllText(filePath)
    with
    | ex -> failwith $"Failed to read file: %s{ex.ToString()}"

let getValueOrFail (res: Result<'T, 'TError>) : 'T =
    match res with
    | Ok value -> value
    | Error err -> failwith $"Result was error: %s{err.ToString()}"

let uncurry f (a,b) = f a b
let uncurry3 f (a,b,c) = f a b c

let inline (%!) a b = (a % b + b) % b

let rec permuteN (n: int) (set: 'a list) : 'a list list =
    assert (n >= 0)
    match n, set with
    | 0, _ ->  []
    | _, [] -> []
    | 1, _ ->  List.map (fun x -> [ x ]) set
    | _ ->
        permuteN (n - 1) set
        |> List.allPairs set
        |> List.map (fun (x,xs) -> x :: xs)

let inline tee fn x = x |> fn |> ignore; x

module ParseInput =
    // Parses an input string into a list of strings
    let lines (inputStr: string) : string list =
        inputStr.Trim().Split("\n")
        |> Array.toList
        |> List.map (_.Trim())


    let words (inputStr: string) : string list list =
        let parseLine (s: string) = Regex.Split(s, @"\s+") |> Array.toList

        lines inputStr
        |> List.map parseLine

    // Parses an input string into 2 int32 columns and returns a tuple of those columns as lists
    let intCols2 (inputStr: string) : int list * int list =
        let parseLine (s: string) =
            let parts = Regex.Split(s, @"\s+") |> Array.map Int32.Parse
            (parts[0], parts[1])

        lines inputStr
        |> List.map parseLine
        |> List.unzip

    // Parses an input string into a list of int arrays
    let intArray (inputStr: string) : int array list =
        let parseLine (s:string) =
            Regex.Split(s, @"\s+")
            |> Array.map Int32.Parse

        inputStr
        |> lines
        |> List.map parseLine

    // Parses an input string into a list of int64 arrays
    let int64Array (inputStr: string) : int64 array list =
        let parseLine (s:string) =
            Regex.Split(s, @"\s+")
            |> Array.map Int64.Parse

        inputStr
        |> lines
        |> List.map parseLine

    let charArray2D (inputStr: string) : char[,] =
        let parseLine (s:string) = s.ToCharArray()

        let lines =
            inputStr
            |> lines
            |> List.map parseLine
            |> List.toArray

        let numLines = lines.Length
        assert (numLines > 0)

        let rowLen = lines[0].Length
        assert (rowLen > 0)

        Array2D.init rowLen numLines (fun x y -> lines[y][x])

    let atoi c = $"%c{c}" |> Int32.Parse

    // Parses an input string like 12345... into a sequence of ints like [ 1; 2; 3; 4; 5; ... ]
    let digits (str: string) : int seq =
        str |> lines |> List.head |> (_.ToCharArray()) |> Seq.map atoi

    let split (str: string) : (string * string) =
        let p = str.Trim().ReplaceLineEndings().Split(Environment.NewLine + Environment.NewLine, 2) |> Array.map (_.Trim())
        (p[0], p[1])

    let charSeq (str: string) : (char seq) =
        str.Trim().ToCharArray() |> Array.toSeq |> Seq.filter (Char.IsWhiteSpace >> not)

module SeqExt =

    // Given a sequence of bools, counts the number of true elements
    let countTrue (seq: seq<bool>) : int =
        Seq.fold (fun acc v -> if v then acc + 1 else acc) 0 seq

    // Given a sequence of bools, returns true if all elements are true
    let all (seq: seq<bool>) : bool = Seq.contains false seq |> not

    let ofArray2D (arr: 'T[,]) : seq<'T> =
        seq {
            let count1 = Array2D.length1 arr
            let count2 = Array2D.length2 arr
            let b1 = Array2D.base1 arr
            let b2 = Array2D.base2 arr
            for i = b1 to b1 + count1 - 1 do
              for j = b2 to b2 + count2 - 1 do
                  yield arr[i,j]
        }

    let foldTimes n f s = Seq.fold (fun s _ -> f s) s [1..n]

module Tuple =

    let flip (a,b) = (b,a)

    let ofArray2 (arr: 'a array) =
        assert (arr.Length = 2)
        (arr[0], arr[1])

    let toArray2 (x: 'a, y: 'a) =
        [| x; y |]

    let add (ax, ay) (bx, by) = (ax + bx, ay + by)

    let sub (ax, ay) (bx, by) = (ax - bx, ay - by)


// Utilities for creating and sorting a directed graph of values
// The graph is stored as a map of nodes to a set of connected nodes
module Graph =

    let getDefault (k: 'a) (def: 'b) (table: Map<'a, 'b>) : 'b =
        match Map.tryFind k table with
        | Some v -> v
        | None -> def

    // Creates a graph
    let create (edges: ('a * 'a) list) =
        let folder acc (key: 'a, other: 'a) =
            let adj = getDefault key Set.empty acc
            Map.add key (Set.add other adj) acc

        List.fold folder Map.empty edges

    // https://cp-algorithms.com/graph/topological-sort.html
    let topologicalSort (dag: Map<'a,Set<'a>>) (input: seq<'a>) =
        let inputSet = Set.ofSeq input

        // Do a depth first traversal of the list keeping track of already visited elements
        // When an element hasn't been visited, put it in the resulting list
        // this function takes and returns its state so it can be folded over the nodes of the dag
        let rec dfs (visited: Set<'a>, sorted: 'a list) (item: 'a) : (Set<'a> * 'a list) =
            if Set.contains item visited then
                (visited, sorted)
            else

            let visited = Set.add item visited
            let children = getDefault item Set.empty dag |> Set.intersect inputSet
            let visited, sorted = Set.fold dfs (visited, sorted) children
            (visited, item :: sorted)

        input
        |> Seq.fold dfs (Set.empty, [])
        |> snd

type CharGrid = char[,]
type Direction = N | W | E | S
type Point = (int * int)

module Array2DExt =

    let foldi (folder: 'TState -> int -> int -> 'T -> 'TState) (state: 'TState) (arr: 'T[,]) : 'TState =
        let mutable _state = state
        let updateState x y value =
            _state <- folder _state x y value
        Array2D.iteri updateState arr
        _state

    let fold (folder: 'TState -> 'T -> 'TState) (state: 'TState) (arr: 'T[,]) : 'TState =
        let wrappedFolder = (fun s _ _ -> folder s)
        foldi wrappedFolder state arr

    let inBounds x y arr =
        x >= 0 && x < (Array2D.length1 arr) &&
        y >= 0 && y < (Array2D.length2 arr)

    let transpose (arr: 'T[,]) : 'T[,] =
        Array2D.init (Array2D.length2 arr) (Array2D.length1 arr) (fun x y -> arr[y,x])

    let dims (arr: 'T[,]) =
        let base1 = Array2D.base1 arr
        let base2 = Array2D.base2 arr
        let length1 = Array2D.length1 arr
        let length2 = Array2D.length2 arr
        (length1 - base1, length2 - base2)

    let toSeq (arr: 'T[,]) : seq<(int * int * 'T)> =
        seq {
            let base1 = Array2D.base1 arr
            let base2 = Array2D.base2 arr
            let length1 = Array2D.length1 arr
            let length2 = Array2D.length2 arr

            for x = base1 to base1 + length1 - 1 do
                for y = base2 to base2 + length2 - 1 do
                    yield (x, y, arr[x,y])
        }

    let findIndex (pred: 'T -> bool) (arr: 'T[,]) : (int * int) =
        arr |> toSeq |> Seq.find (fun (_, _, v) -> pred v) |> fun (x, y, _) -> (x, y)

    let findIndexOf (item: 'T) (arr: 'T[,]) : (int * int) =
        arr |> toSeq |> Seq.find (fun (_, _, v) -> v = item) |> fun (x, y, _) -> (x, y)

    let setWith (x: int) (y: int) (v: 'T) (arr: 'T[,]) : 'T[,] =
        let arrCopy = Array2D.copy arr
        Array2D.set arrCopy x y v
        arrCopy

    let coords (arr: 'T[,]) =
        let (width, height) = dims arr
        Seq.allPairs [ 0..(width - 1) ] [ 0..(height - 1) ]

    let findIndexesOf (value: 'T) =
        toSeq
        >> Seq.filter (fun (_, _, v) -> v = value)
        >> Seq.map (fun (x, y, _) -> (x, y))

    let filterNeighbors (f: 'T -> bool) (x: int, y: int) (arr: 'T[,]) : ((int * int) * 'T) seq =
        let checkNeighbor (cx: int, cy: int) : ((int * int) * 'T) option =
            let x, y = x + cx, y + cy
            if (inBounds x y arr) then
                let v = arr[x, y]
                if (f v) then Some ((x, y), v) else None
            else
                None

        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> Seq.choose checkNeighbor

    let isValidPoint (arr: 'T[,]) (p: Point) =
        let width, height = dims arr
        let x, y = p
        x >= 0 && x < width && y >= 0 && y < height

    let tryGet (arr: 'T[,]) (p: Point) : 'T option =
        if not (isValidPoint arr p) then
            None
        else
            let (x, y) = p
            Some arr[x,y]

    let get (arr: 'T[,]) (p: Point) : 'T =
        tryGet arr p |> Option.get

module CharGrid =

    let fromString = ParseInput.charArray2D

    let withOverlay (grid: CharGrid) (c: char) (points: (int * int) seq) : CharGrid =
        let newGrid = Array2D.copy grid
        points |> Seq.iter (fun (x, y) -> Array2D.set newGrid x y c)
        newGrid

    let debugPrint (grid: CharGrid) : unit =
        let width, height = Array2DExt.dims grid
        for y = 0 to (height - 1) do
            [0..(width - 1)]
            |> Seq.map (fun x -> grid[x,y])
            |> Seq.fold (fun (sb: StringBuilder) -> sb.Append) (StringBuilder())
            |> _.ToString()
            |> printfn "%s"

    // Does an in-place swap of values between two points
    let swap (grid: CharGrid) (ax, ay) (bx, by) =
        let temp = grid[ax, ay]
        grid[ax, ay] <- grid[bx, by]
        grid[bx, by] <- temp


    let tryNext (grid: CharGrid) (pos: (int * int)) (m: Direction) : (char * (int * int)) option =
        let delta =
            match m with
            | W -> (-1, 0)
            | E -> (1, 0)
            | N -> (0, -1)
            | S -> (0, 1)

        let newPos = Tuple.add pos delta
        if not (Array2DExt.isValidPoint grid newPos) then
            None
        else

        let c = newPos ||> Array2D.get grid
        Some (c, newPos)

    let next (grid: CharGrid) (pos: (int * int)) (m: Direction) : (char * (int * int)) =
        tryNext grid pos m |> Option.get

    let charAt (grid: CharGrid) (p: Point) =
        let x, y = p
        grid[x,y]

module SetExt =

    let without (other: Set<'a>) (s: Set<'a>) = Set.difference s other


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

    let toPoint (d: Direction) : Point =
        match d with
        | N -> (0, -1)
        | E -> (1, 0)
        | S -> (0, 1)
        | W -> (-1, 0)

module Pred =
    let equals a b = a = b
    let notEquals a b = a <> b
