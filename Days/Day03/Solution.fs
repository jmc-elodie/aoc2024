module AoC2024.Days.Day03.Solution

open System
open System.Text.RegularExpressions
open AoC2024.Util
open NUnit.Framework
open FsUnit

module PartOne =
    
    let parseMuls (str: string) =
        seq {
            let matches = Regex.Matches(str, @"mul\((\d+),(\d+)\)")
            for m in matches do
                assert (m.Groups.Count = 3)
                let a = (m.Groups.Item 1).Value |> Int32.Parse
                let b = (m.Groups.Item 2).Value |> Int32.Parse
                yield (a, b)
        }
        
    let solve (str: string) =
        str
        |> parseMuls
        |> Seq.map (fun (a,b) -> a * b)
        |> Seq.sum
     
    [<Test>]
    let ``example input``() =
        let exampleInput = @"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
        
        exampleInput
        |> solve
        |> should equal 161
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day03\input.txt" 
        |> solve
        |> should equal 167090022
       
        
module PartTwo =
    type MemOperator =
        | Mul of (int * int)
        | Do
        | Dont
        
    let parseOperators (str:string) =
        seq {
            let matches = Regex.Matches(str, @"mul\((\d+),(\d+)\)|do\(\)|don't\(\)")
            for m in matches do
                let op = m.Value.Split('(', 2) |> Array.head
                
                let parsed =
                    match op with
                    | "mul" ->
                        let a = (m.Groups.Item 1).Value |> Int32.Parse
                        let b = (m.Groups.Item 2).Value |> Int32.Parse
                        Mul(a, b)
                    | "do" -> Do
                    | "don't" -> Dont
                    | _ -> failwith $"unknown operator %s{op}"
                    
                yield parsed
        }
        
    let solve (str: string) =
        
        // This is a fold function where the sum * mulActive tuple is the state
        // when mulActive is true, Mul operators multiple and add to the sum
        // when mulActive is false, Mul operators do nothing
        // Do and Dont operators change mulActive
        let compute (sum: int, mulActive: bool) (op: MemOperator) =
            match op with 
            | Mul (a, b) ->
                if mulActive then (sum + a * b, true) else (sum, false)
            | Do -> (sum, true)
            | Dont -> (sum, false)
            
        str
        |> parseOperators
        |> Seq.fold compute (0, true)
        |> fst
    
    [<Test>]
    let ``example input``() =
        let exampleInput = @"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        
        exampleInput
        |> solve
        |> should equal 48
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day03\input.txt" 
        |> solve
        |> should equal 89823704
