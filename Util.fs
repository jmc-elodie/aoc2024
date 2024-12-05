module AoC2024.Util

open System
open System.IO
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

module ParseInput =
    // Parses an input string into a list of strings
    let strings (inputStr: string) : string list =
        inputStr.Trim().Split("\n") 
        |> Array.toList 
      
    // Parses an input string into 2 int32 columns and returns a tuple of those columns as lists
    let intCols2 (inputStr: string) : int list * int list =
        let parseLine (s: string) =
            let parts = Regex.Split(s.Trim(), @"\s+") |> Array.map Int32.Parse
            (parts[0], parts[1])
        
        strings inputStr    
        |> List.map parseLine 
        |> List.unzip

    // Parses an input string into a list of int arrays
    let intArray (inputStr: string) : int array list =
        let parseLine (s:string) =
            Regex.Split(s.Trim(), @"\s+")
            |> Array.map Int32.Parse
            
        inputStr
        |> strings
        |> List.map parseLine

