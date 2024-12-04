module AoC2024.Util

open System
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework

let readTextFromFile relativePath =
    let filePath = Path.Combine(TestContext.CurrentContext.TestDirectory, relativePath)
    try 
        let inputStr = File.ReadAllText(filePath)
        Ok inputStr
    with
    | ex -> Error ex
  
let getValueOrFail (res: Result<'T, 'TError>) : 'T =
    match res with
    | Ok value -> value
    | Error err -> failwith $"Result was error: %s{err.ToString()}"
   
// Parses an input string into 2 int32 columns and returns a tuple of those columns as lists
let parseInput_2IntCols (inputStr: string) : int list * int list =
    let breakLine (s: string) =
        let parts = Regex.Split(s.Trim(), @"\s+") |> Array.map Int32.Parse
        (parts[0], parts[1])
        
    inputStr.Trim().Split("\n") 
    |> Array.toList 
    |> List.map breakLine 
    |> List.unzip

let uncurry f (a,b) = f a b

let parseAndSolve inputStr parser solver = inputStr |> parser |> solver

let readParseSolve filePath parser solver = 
    readTextFromFile filePath 
    |> Result.map (fun s -> parseAndSolve s parser solver) 
    |> getValueOrFail
