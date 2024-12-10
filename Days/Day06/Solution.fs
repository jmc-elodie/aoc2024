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
    type Board = (char[,] * Set<(int*int)>)
    type Direction =
        | Up
        | Left
        | Right
        | Down
        
    type PlayingState = { position: Position; direction: Direction; board: Board }
       
    type GameState =
        | Playing of PlayingState
        | Done of Board
    
    let move (state: GameState) : GameState =
        match state with
        | Done _ -> state // If we move an already Done game state, then just return it and noop
        | Playing playingState ->
    
        let { position = (x, y); direction = dir; board = (board, xs) } = playingState
        
        let dx, dy =
            match dir with
            | Up -> 0, -1
            | Left -> -1, 0
            | Right -> 1, 0
            | Down -> 0, 1
                
        let nx, ny = (x + dx, y + dy)
      
        let updatedXs = Set.add (x, y) xs
        let boardWithX = (board, updatedXs)
        
        let isNextOnBoard = Array2DExt.inBounds nx ny board
       
        // If the next position is off the board, then we're done 
        if not isNextOnBoard then
            Done boardWithX
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
            Playing { playingState with board = boardWithX; position = (nx, ny) }
       
    let getBoard = function
        | Done b -> b
        | Playing { board = b } -> b
       
    let getBoardWithXs (state: GameState) =
        let (board, xs) = getBoard state
            
        let updatedBoard = Array2D.copy board
        
        xs |> Set.iter (fun (x,y) -> Array2D.set updatedBoard x y 'X')
        updatedBoard
       
    let getDirection = function
        | Done _ -> "N/A"
        | Playing ps -> sprintf "%A" ps.direction
        
    let countXs (state: GameState) =
        match state with
        | Done (_, xs) -> Set.count xs
        | _ -> failwith $"invalid state to count Xs %A{state}"
            
    let debugPrint (state: GameState) =
        let fixPrintOrientation = Array2DExt.transpose >> Array2DExt.transpose >> Array2DExt.transpose
        let board = getBoardWithXs state |> fixPrintOrientation
        let (_, xs) = getBoard state
        printfn $"move: %s{getDirection state}\nnum:%d{countXs state}\nxs: %A{xs}{{xs}}\n%A{board}"
    
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
                board = (board, Set.empty)
            }
            
        ParseInput.charArray2D >> Array2DExt.transpose >> arrayToState
        
        
module PartOne =
    
    open Game 
        
    [<TailCall>] 
    let rec moveUntilDone (state: GameState) =
        // debugPrintState state
        
        match state with
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
        
        
    let parseAndSolve = ParseInput.charArray2D >> Array2DExt.transpose >> solve
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseAndSolve
        |> tee (printfn "%A")
        |> should equal 2
        
