module AoC2024.Days.Day13.Solution

open System
open System.Text.RegularExpressions
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput =
    """
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    """

// 3 tokens for A(X), 1 token for B(Y)
// 0 < X, Y <= 100
// Example 1:
// X * 94 + Y * 22 = 8400
// X * 34 + Y * 67 = 5400
// Intersection on (80, 40)
// Example 2, intersection > X = 100 -> no solution
// Example 3, intersection (38, 86)
// Example 4, no solution

// Solve by substitution:
// X * 94 + Y * 22 = 8400
// X * 34 + Y * 67 = 5400
//
// Y = (8400 - 94*X) / 22
// Y = (5400 - 34*X) / 67
// let X = (5400.0/67.0 - 8400.0/22.0) / (34.0/67.0 - 94.0/22.0)
// let Y = (8400.0 - 94.0 * X) / 22.0
// X = 80, Y = 40

type Machine = {
    A: (float * float)
    B: (float * float)
    P: (float * float)
}

let parseMachines (str: string) =
    let parseLine (i: int) (line: string) =
        let m, indexes =
            match i % 3 with
            | 0
            | 1 -> 
                let m = Regex.Match(line, @"Button (A|B): X\+(\d+), Y\+(\d+)")
                m, [ 2; 3 ]
            | 2 -> 
                let m = Regex.Match(line, @"Prize: X=(\d+), Y=(\d+)")
                m, [ 1; 2 ]
            
        indexes
        |> Seq.map (fun i -> Int32.Parse(m.Groups[i].Value) |> float)
        |> Seq.toArray
        |> Tuple.ofArray2
   
    let toMachine (nums: (float * float) array) : Machine =
        assert (nums.Length = 3)
        {
            A = nums[0]
            B = nums[1]
            P = nums[2]
        }
     
    str
    |> ParseInput.strings
    |> Seq.filter (fun s -> s.Trim().Length > 0)
    |> Seq.mapi parseLine
    |> Seq.chunkBySize 3
    |> Seq.map toMachine


module PartOne =

    let solve (machines: Machine seq) : int =
        let solveMachine (m: Machine) : (int * int) option =
            let isInt (f: float) = (abs (f - (round f))) < 0.00001
            let { A=(ax,ay); B=(bx,by); P=(px,py) } = m
            let X = (py/ by - px/bx) / (ay/by - ax/bx)
            let Y = (px - ax * X) / bx
            if (isInt X) && (isInt Y) && X > 0.0 && X <= 100.0 && Y > 0.0 && Y <= 100.0 then
                Some (int (round X), (int (round Y)))
            else
                None
        
        machines
        |> Seq.choose solveMachine
        |> Seq.map (fun (a, b) -> 3 * a + b)
        |> Seq.sum
 
    let parseAndSolve = parseMachines >> solve

    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 480

    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day13/input.txt"
        |> parseAndSolve
        |> should equal 37901

