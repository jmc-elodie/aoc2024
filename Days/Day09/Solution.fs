module AoC2024.Days.Day09.Solution

open NUnit.Framework
open FsUnit
open AoC2024.Util

let exampleInput = "2333133121414131402"
   
    
type DiskMapItem =
    | File of (int * int)
    | Free of int
   
let parseDiskMap (str: string) : DiskMapItem list =
    str
    |> ParseInput.digits
    |> Seq.mapi (fun i v -> (i, v))
    |> Seq.map (function
        | i, n when (i % 2 = 0) -> File (i / 2, n)
        | _, n -> Free n)
    |> Seq.toList

let isFile = (function
    | File _ -> true
    | _ -> false)

module PartOne =
    
    let defragBlocks (diskMap: DiskMapItem list) : int seq =
       
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
                
                // if the head of the map is a free block, consume it with a file from the end and recurse
                | (Free freeN :: remainingDiskMap), (File (idx, fileN) :: remainingFiles) ->
                    yield idx
                    yield! inner (Free (freeN - 1) :: remainingDiskMap) (File (idx, fileN - 1) :: remainingFiles)
            }
      
        // A reversed list of only the file blocks. This makes it easier to pull file blocks to fill
        let reversedFiles =
            diskMap
            |> List.filter isFile 
            |> List.rev
        
        inner diskMap reversedFiles
    
    let solve =
        defragBlocks
        >> Seq.mapi (fun i n -> int64 (i * n))
        >> Seq.sum
        
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
        |> should equal 6471961544878L

module PartTwo =
 
    let defragFiles (diskMap: DiskMapItem list) : (int * int) seq =
      
        let rec inner (diskMap: DiskMapItem list) =
            seq {
                match diskMap with
                | [] -> ()
                | Free n :: rest ->
                    yield (Free n)
                    yield! inner rest
                
                | File (idx, n) :: rest ->
                    
                    let file = File (idx, n)
                    let freeSpace =
                        rest
                        |> Seq.indexed
                        |> Seq.tryFindBack (
                            snd
                            >> (function
                                | Free space -> space >= n
                                | _ -> false))
                    
                    match freeSpace with
                    | None ->
                        yield file
                        yield! inner rest
                        
                    | Some (i, (Free space)) when space = n ->
                        yield (Free space)
                        yield! inner (List.updateAt i file rest)
                        
                    | Some (i, (Free space)) when space > n ->
                        yield (Free n)
                        
                        let edit =
                            rest
                            |> List.updateAt i (Free (space - n))
                            |> List.insertAt (i + 1) file
                            
                        yield! inner edit
            }
        
        let expandBlock (offset: int) (block: DiskMapItem) : ((int * int) seq * int) =
            let idx, n =
                match block with
                | File (idx, n) -> idx, n
                | Free n -> 0, n
            
            let blocks =
                [0..(n-1)]
                |> Seq.map (fun i -> (idx, offset + i))
        
            (blocks, offset + n)
            
        diskMap
        |> List.rev
        |> inner
        |> Seq.rev
        |> Seq.mapFold expandBlock 0
        |> fst
        |> Seq.concat
            
    let solve =
        defragFiles
        >> Seq.map (fun (idx, n) -> int64 (idx * n))
        >> Seq.sum
        
    let parseAndSolve = parseDiskMap >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 2858

    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day09/input.txt"
        |> parseAndSolve
        |> should equal 6511178035564L