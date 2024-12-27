module AoC2024.Days.Day19.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit


let exampleInput =
    """
    r, wr, b, g, bwu, rb, gb, br

    brwrr
    bggr
    gbbr
    rrbgbr
    ubwu
    bwurrg
    brgr
    bbrgwb
    """

let parseCloths (str: string) =
    let parseColorPart (line: string) =
        line.Split(',')
        |> Array.toSeq
        |> Seq.map (_.Trim())
        |> Seq.map (_.ToCharArray())
        |> Seq.map Array.toList
    
    let parsePattern (line: string) =
        line.Trim().ToCharArray()
        |> Array.toList
        
    let parsePatterns (str: string) =
        ParseInput.lines str
        |> Seq.map parsePattern
        
    let colorPart, patternPart = ParseInput.split str
    
    parseColorPart colorPart, parsePatterns patternPart


module PartOne =
    
    let solve (cloths: char list seq) (patterns: char list seq) : int =
        let matchPattern (pattern: char list) : bool =
            let mutable cache = Map.empty
            
            let rec loop (pattern: char list) : bool =
                match Map.tryFind pattern.Length cache with
                | Some b -> b
                | None ->
                
                let res =
                    cloths
                    |> Seq.filter (fun cloth ->
                        if cloth.Length > pattern.Length then false else
                        (List.take cloth.Length pattern) = cloth)
                    |> Seq.map (fun matched ->
                        let rest = List.skip matched.Length pattern
                        if rest.Length = 0 then true else
                        loop rest)
                    |> Seq.contains true
                
                cache <- Map.add pattern.Length res cache
            
                res
      
            loop pattern
        
        patterns
        |> Seq.filter matchPattern
        |> Seq.length

   
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseCloths
        |> uncurry solve
        |> should equal 6
     
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day19/input.txt"
        |> parseCloths
        |> uncurry solve
        |> should equal 319


module PartTwo =
    
    let solve (cloths: char list seq) (patterns: char list seq) : int64 =
        let countMatches (pattern: char list) : int64 =
            let mutable cache = Map.empty
            
            let rec loop (pattern: char list) : int64 =
                match Map.tryFind pattern.Length cache with
                | Some b -> b
                | None ->
                
                let res =
                    cloths
                    |> Seq.filter (fun cloth ->
                        if cloth.Length > pattern.Length then false else
                        (List.take cloth.Length pattern) = cloth)
                    |> Seq.sumBy (fun matched ->
                        let rest = List.skip matched.Length pattern
                        if rest.Length = 0 then 1L else
                        loop rest)
                
                cache <- Map.add pattern.Length res cache
                
                res
            
            loop pattern
             
        patterns
        |> Seq.sumBy countMatches
        
   
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseCloths
        |> uncurry solve
        |> should equal 16
     
     
    [<Test>]
    let ``problem input``() =
        // 309, 311 were too low
        readTextFromFile @"Days/Day19/input.txt"
        |> parseCloths
        |> uncurry solve
        |> should equal 692575723305545L
