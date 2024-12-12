module AoC2024.Days.Day09.Solution

open System
open NUnit.Framework
open FsUnit
open AoC2024.Util

let exampleInput = "2333133121414131402"

let atoi c = $"%c{c}" |> Int32.Parse

let parseInts (str: string) : int seq =
    str |> ParseInput.strings |> List.head |> (_.ToCharArray()) |> Seq.map atoi
    
    
type DiskMapItem =
    | File of (int * int)
    | Free of int
    
let parseDiskMap (str: string) : DiskMapItem list =
    str
    |> parseInts
    |> Seq.mapi (fun i v -> (i, v))
    |> Seq.map (function
        | i, n when (i % 2 = 0) -> File (i / 2, n)
        | _, n -> Free n)
    |> Seq.toList

module PartOne =
    
    let defragAndExpand (diskMap: DiskMapItem list) : int seq =
        
        let rec inner (diskMap: DiskMapItem list) (reversedFileBlocks: DiskMapItem list) : int seq =
            seq {
                match diskMap, reversedFileBlocks with
                // if the next file block to defrag is the same as the next file block to move, then we're done
                | (File (idxA, _) :: _), (File (idxB, n) :: _) when idxA = idxB ->
                    yield! [0..(n - 1)] |> Seq.map (fun _ -> idxA)
                 
                // if the next file block on the end is empty, remove it and recurse
                | _, (File (_, 0) :: remainingFiles) -> 
                    yield! inner diskMap remainingFiles
               
                // if the head of the map is an empty file block, recurse and move on 
                | File (_, 0) :: remainingDiskMap, _ ->
                    yield! inner remainingDiskMap reversedFileBlocks
                    
                // if the head of the map is an empty free block, recurse and move on 
                | Free 0 :: remainingDiskMap, _ ->
                    yield! inner remainingDiskMap reversedFileBlocks
                 
                // if the head of the map is a file, generate it and recurse 
                | File (idx, n) :: remainingDiskMap, _ ->
                    yield idx
                    yield! inner (File (idx, n - 1) :: remainingDiskMap) reversedFileBlocks
                
                // if the head of the map is a free block, consume it with a file from the end
                | (Free freeN :: remainingDiskMap), (File (idx, fileN) :: remainingFiles) ->
                    yield idx
                    yield! inner (Free (freeN - 1) :: remainingDiskMap) (File (idx, fileN - 1) :: remainingFiles)
            }
      
        // A reversed list of only the file blocks. This makes it easier to pull file blocks to fill
        let reversedFiles =
            diskMap
            |> List.filter (function
                | File _ -> true
                | _ -> false)
            |> List.rev
        
        inner diskMap reversedFiles
    
    let solve (diskMap: DiskMapItem list) : int64 =
        diskMap
        |> defragAndExpand
        |> tee (printfn "%A")
        |> Seq.mapi (fun i n -> int64 (i * n))
        |> Seq.sum
        
    let parseAndSolve = parseDiskMap >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 1928
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day09/input.txt"
        |> parseAndSolve
        |> tee (printfn "%A")
        |> should equal 6471961544878L
