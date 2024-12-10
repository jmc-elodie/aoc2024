module AoC2024.Days.Day06.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

    
module Game =
    
    type Position = (int * int)
    type Direction =
        | Up
        | Left
        | Right
        | Down
        
    type Board = char[,]
    type History = Set<Position * Direction>
 
    type PlayingState = { position: Position; direction: Direction; board: Board; history: History }
    type DoneState = { board: Board; history: History }
       
    type GameState =
        | Playing of PlayingState
        | Done of DoneState
        | Loop
    
    let move (state: GameState) : GameState =
        match state with
        | Loop -> state
        | Done _ -> state // If we move an already Done game state, then just return it and noop
        | Playing playingState ->
     
        let { position = (x, y); direction = dir; board = board; history = hist } = playingState
        
        let posAndDir = ((x, y), dir)
     
        if Set.contains posAndDir hist then
            Loop
        else
         
        let dx, dy =
            match dir with
            | Up -> 0, -1
            | Left -> -1, 0
            | Right -> 1, 0
            | Down -> 0, 1
                
        let nx, ny = (x + dx, y + dy)
        let hist = Set.add posAndDir hist
        let isNextOnBoard = Array2DExt.inBounds nx ny board
       
        // If the next position is off the board, then we're done 
        if not isNextOnBoard then
            Done { board = board; history = hist }
        else
      
        if board[nx, ny] = '#' then
            let turnRight =
                match dir with
                | Up -> Right
                | Left -> Up
                | Right -> Down
                | Down -> Left
            
            Playing { playingState with direction = turnRight }
        else
            Playing { playingState with position = (nx, ny); history = hist }
       
    let getBoardAndHistory = function
        | Done { board = b; history = h } -> b, h
        | Playing { board = b; history = h } -> b, h
        | _ -> failwith "getBoardAndHistory: invalid game state"
            
    let getDirection = function
        | Done _ -> "N/A"
        | Playing ps -> sprintf "%A" ps.direction
        | _ -> failwith "getBoardAndHistory: invalid game state"
        
    let countXs (state: GameState) =
        state
        |> getBoardAndHistory
        |> snd
        |> Set.map fst
        |> Set.count
            
    let debugPrint (state: GameState) =
        let getBoardWithXs (state: GameState) =
            let board, hist = getBoardAndHistory state
            let board = Array2D.copy board
            
            hist |> Set.iter (fun ((x,y), _) -> Array2D.set board x y 'X')
            board
       
        let fixPrintOrientation = Array2DExt.transpose >> Array2DExt.transpose >> Array2DExt.transpose
        let board = getBoardWithXs state |> fixPrintOrientation
        let (_, hist) = getBoardAndHistory state
        printfn $"move: %s{getDirection state}\ncount:%d{countXs state}\nxs: %A{hist}\n%A{board}"
    
    let fromString =
        let caretToDirection c : Option<Direction> =
            match c with
            | '^' -> Some Up
            | '<' -> Some Left
            | '>' -> Some Right
            | 'v' -> Some Down
            | _ -> None
            
        let arrayToState (arr: char[,]) : GameState =
            let position = Array2DExt.findIndex (caretToDirection >> Option.isSome) arr
            let x,y = position
            let direction = caretToDirection arr[x,y] |> Option.get
            let board = Array2DExt.setWith x y '.' arr
            
            GameState.Playing {
                position = position
                direction = direction
                board = board
                history = Set.empty 
            }
            
        ParseInput.charArray2D >> Array2DExt.transpose >> arrayToState
        
        
module PartOne =
    
    open Game 
        
    [<TailCall>] 
    let rec moveUntilDone (state: GameState) =
        Game.debugPrint state
        
        match state with
        | Loop -> state
        | Done _ -> state
        | Playing _ ->
            
        move state
        |> moveUntilDone
        
    let solve = moveUntilDone >> countXs
     
    let parseAndSolve = Game.fromString >> solve
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> should equal 41

    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day06/input.txt"
        |> parseAndSolve
        |> should equal 5331 

    
module PartTwo =
   
    let solve (board: char[,]) =
        0        
        
        
    let parseAndSolve = ParseInput.charArray2D >> Array2DExt.transpose >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> tee (printfn "%A")
        |> should equal 2
        
