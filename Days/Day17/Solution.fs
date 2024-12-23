module AoC2024.Days.Day17.Solution

open System
open System.Text.RegularExpressions
open AoC2024.Util
open Microsoft.FSharp.Collections
open NUnit.Framework
open FsUnit

  
type Register = A | B | C
let registerFromChar = function
    | 'A' -> A
    | 'B' -> B
    | 'C' -> C
    
let regToInt = function
    | A -> 0
    | B -> 1
    | C -> 2
   
type ProgramState = {
    regs: int64 array
    ops: int array
    ip: int
    out: int list
    halt: bool
}  
  
let parseProgram (str: string) : ProgramState =
    
    let registerParts, programPart = ParseInput.split str
    
    let parseRegister (line: string) =
        let m = Regex.Match(line, @"Register ([A-Z]): (\d+)")
        (m.Groups.[1].Value.Chars 0 |> registerFromChar, m.Groups[2].Value |> Int64.Parse)
   
    let parseProgram (line: string) =
        let m = Regex.Match(line, @"Program: (.+)$")
        m.Groups[1].Value.Split(',') |> Array.map Int32.Parse
    
    let registers = Array.zeroCreate 3
    
    registerParts
    |> ParseInput.strings
    |> Seq.map parseRegister
    |> Seq.iter (fun (r, i) -> registers[regToInt r] <- i)
        
    let operations = programPart |> parseProgram
    
    {
        regs = registers
        ops = operations
        ip = 0
        out = []
        halt = false
    }

let setCopy (i: int) (v: 'a) (arr: 'a array) : 'a array =
    let cpy = Array.copy arr
    cpy[i] <- v
    cpy

let setReg (ps: ProgramState) (reg: Register) (value: int64) : ProgramState =
    { ps with regs = setCopy (regToInt reg) value ps.regs }

let setRegAndAdvance (ps: ProgramState) (reg: Register) (value: int64) : ProgramState =
    { ps with regs = setCopy (regToInt reg) value ps.regs; ip = ps.ip + 2 }
    
let getReg (ps: ProgramState) (reg: Register) : int64 =
    ps.regs[regToInt reg]

let getComboOp (ps: ProgramState) : int64 =
    let op = ps.ops[ps.ip + 1]
    match op with
    | n when n >= 0 && n <= 3 -> n
    | 4 -> getReg ps A
    | 5 -> getReg ps B
    | 6 -> getReg ps C
    | n -> failwith $"combo op %d{n} is invalid"
    
let getLiteralOp (ps: ProgramState) : int = ps.ops[ps.ip + 1]
    
let jumpTo (newIp: int) (ps: ProgramState) : ProgramState = { ps with ip = newIp }

let halt (ps: ProgramState) : ProgramState = { ps with halt = true }

let runOneOp (ps: ProgramState) : ProgramState =
    let ip = ps.ip
    if ip >= ps.ops.Length || ip < 0 then
        // printfn $"invalid ip: %d{ip}"
        halt ps
    else
    
    match ps.ops[ip] with
    | 0 (* adv *) ->
        let n = getReg ps A
        let co = getComboOp ps
        let d = 1L <<< (int co)
        let r = n / d
        
        // printfn $">%02d{ip}: adv %d{n} %d{d} -> %d{r}"
        
        setRegAndAdvance ps A r
        
    | 1 (* bxl *) ->
        let b = getReg ps B
        let n = getLiteralOp ps
        let r = b ^^^ n
        
        // printfn $">%02d{ip}: bxl %d{b} %d{n} -> %d{r}"
        
        setRegAndAdvance ps B r
        
    | 2 (* bst *) ->
        let n = getComboOp ps
        let r = n % 8L
        
        // printfn $">%02d{ip}: bst %d{n} -> %d{r}"
        
        setRegAndAdvance ps B r
        
    | 3 (* jnz *) ->
        let a = getReg ps A
        let n = getLiteralOp ps
        let r = if a = 0 then (ip + 2) else n
        
        // printfn $">%02d{ip}: jnz %d{a} %d{n} -> %d{r}"
        
        jumpTo r ps
            
    | 4 (* bxc *) ->
        let b = getReg ps B
        let c = getReg ps C
        let r = b ^^^ c
        
        // printfn $">%02d{ip}: bxc %d{b} %d{c} -> %d{r}"
        
        setRegAndAdvance ps B r
        
    | 5 (* out *) ->
        let n = getComboOp ps
        let r = n % 8L |> int
        
        // printfn $">%02d{ip}: out %d{n} -> %d{r}"
        
        { ps with out=(r :: ps.out) } |> jumpTo (ip + 2)
        
    | 6 (* bdv *) ->
        let n = getReg ps A
        let co = getComboOp ps
        let d = 1L <<< (int co)
        let r = n / d
        
        // printfn $">%02d{ip}: bdv %d{n} %d{d} -> %d{r}"
        
        setRegAndAdvance ps B r
        
    | 7 (* cdv *) -> 
        let n = getReg ps A
        let co = getComboOp ps
        let d = 1L <<< (int co)
        let r = n / d
        
        // printfn $">%02d{ip}: cdv %d{n} %d{d} -> %d{r}"
        
        setRegAndAdvance ps C r
        
    | n ->
        // printfn $"err: invalid opcode %d{n}"
        halt ps
    
let runUntilHalt (ps: ProgramState) : ProgramState =
    let mutable ps = ps
    while not ps.halt do
        ps <- runOneOp ps
    ps


module PartOne =
  
    let exampleInput =
        """
        Register A: 729
        Register B: 0
        Register C: 0

        Program: 0,1,5,4,3,0
        """
        
    let solve (ps: ProgramState) : string =
        let ps = runUntilHalt ps
        let out = ps.out |> List.rev |> List.toArray
        String.Join(',', out)
     
    [<Test>] 
    let ``example input``() =
        exampleInput
        |> parseProgram
        |> solve
        |> should equal "4,6,3,5,6,3,5,2,1,0"
        
    [<Test>] 
    let ``problem input``() =
        readTextFromFile @"Days/Day17/input.txt"
        |> parseProgram
        |> solve
        |> should equal "2,1,0,4,6,2,4,2,0"

module PartTwo =
    
    let exampleInput =
        """
        Register A: 2024
        Register B: 0
        Register C: 0

        Program: 0,3,5,4,3,0
        """
         
    let fromOctal (str: string) : int64 = Convert.ToInt64(str, 8)
    
    let fromOctalDigits (digits: int seq) : int64 =
        String.Join("", digits |> Seq.rev |> Seq.toArray) |> fromOctal

    let reset (a: int64) (ps: ProgramState) : ProgramState =
        { ps with halt = false; ip = 0; out = []; regs = [| a; 0L; 0L |] }
    
    let solve (ps: ProgramState) : int64 =
            
        let firstDigitOfOutput (a: int64) =
            ps
            |> reset a
            |> runUntilHalt
            |> (_.out)
            |> List.last
            
        let rec loop (digits: int list) = function
            | [] -> Some digits
            | (nextProgramDigit :: programDigits) ->
            
                let checkDigit (d: int) =
                    let res = fromOctalDigits (d :: digits) |> firstDigitOfOutput
                    res = nextProgramDigit
                    
                [0..7]
                |> Seq.filter checkDigit
                |> Seq.choose (fun d -> loop (d :: digits) programDigits)
                |> Seq.tryHead
            
        let reversedProgramDigits = ps.ops |> Array.toList |> List.rev
       
        loop [] reversedProgramDigits |> Option.get |> fromOctalDigits
    
    [<Test>] 
    let ``example input``() =
        exampleInput
        |> parseProgram
        |> solve
        |> should equal 117440
        
    [<Test>] 
    let ``problem input``() =
        readTextFromFile @"Days/Day17/input.txt"
        |> parseProgram
        |> solve
        |> should equal 109685330781408L