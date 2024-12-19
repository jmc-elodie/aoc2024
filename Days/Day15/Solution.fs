module AoC2024.Days.Day15.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit

open AoC2024.Util.CharGrid

let exampleInput =
    """
    ##########
    #..O..O.O#
    #......O.#
    #.OO..O.O#
    #..O@..O.#
    #O#..O...#
    #O..O..O.#
    #.OO.O.OO#
    #....O...#
    ##########

    <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
    vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
    ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
    <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
    ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
    ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
    >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
    <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
    ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
    v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    """

type Warehouse = {
    Moves: Direction seq
    Layout: char[,]
    Pos: (int * int)
}

let parseMoves (str: string) : Direction seq =
    str
    |> ParseInput.charSeq
    |> Seq.map (function
        | '<' -> L
        | '>' -> R
        | '^' -> U
        | 'v' -> D)
    
let parseWarehouse (str: string) : Warehouse =
    let gridInput, movesInput = ParseInput.split str
   
    let grid = ParseInput.charArray2D gridInput 
    let moves = parseMoves movesInput
   
    let pos = Array2DExt.findIndex (fun v -> v = '@') grid
    let x, y = pos
     
    {
        Moves = moves
        Layout = Array2DExt.setWith x y '.' grid
        Pos = pos
    }

module PartOne = 

    let smallerExampleInput =
        """
        ########
        #..O.O.#
        ##@.O..#
        #...O..#
        #.#.O..#
        #...O..#
        #......#
        ########

        <^^>>>vv<v>>v<<
        """
        
    let solve (wh: Warehouse) : int =
        let { Moves = moves; Layout = grid; Pos = pos } = wh
        
        let mutable grid = grid
      
        let push (pos: (int * int)) (m: Direction) : bool =
            let rec findEmpty p =
                let nc, np = next grid p m
                match nc with
                | '.' -> Some np
                | '#' -> None
                | 'O' -> findEmpty np
                
            match findEmpty pos with
            | None -> false
            | Some emptyPos ->
           
            CharGrid.swap grid emptyPos pos
            true    
            
        // cases, next char is:
        // . - update pos
        // # - do nothing
        // O - update pos and push
        let move (pos: (int * int)) (m: Direction) : (int * int) =
            let nextChar, nextPos = next grid pos m
            match nextChar with
            | '.' -> nextPos
            | '#' -> pos
            | 'O' -> if push nextPos m then nextPos else pos
   
        // Calculate the GPS score of a location 
        let gpsScore x y = 100 * y + x
      
        let moveAndPrint p m =
            printfn "move: %A" m
            let newPos = move p m
            CharGrid.withOverlay grid '@' [ newPos ] |> CharGrid.debugPrint
            newPos
            
        // Do all moves, this mutates the grid as a sideffect 
        // moves |> Seq.fold moveAndPrint pos |> ignore
        moves |> Seq.fold move pos |> ignore
        
        // Score the grid 
        Array2DExt.toSeq grid
        |> Seq.filter (fun (_, _, c) -> c = 'O')
        |> Seq.map (fun (x, y, _) -> gpsScore x y)
        |> Seq.sum
    
    [<Test>]
    let ``smaller example input``() =
        smallerExampleInput
        |> parseWarehouse
        |> solve
        |> should equal 2028
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseWarehouse
        |> solve
        |> should equal 10092
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day15/input.txt"
        |> parseWarehouse
        |> solve
        |> should equal 1505963
        

module PartTwo =
    
    let smallerExampleInput =
         """
         #######
         #...#.#
         #.....#
         #..OO@#
         #..O..#
         #.....#
         #######

         <vv<<^^<<^^
         """
         
         
     
    let smallerExampleInput2 =
         """
         #######
         #...#.#
         #.....#
         #..O..#
         #..O..#
         #..@..#
         #######

         ^^^
         """

    let expandGrid (grid: char[,]) : char[,] =
        let w, h = Array2DExt.dims grid
        
        let expandedCell x y =
            let ox, oy = x / 2, y
            match grid[ox, oy] with
            | '#' -> '#'
            | '.' -> '.'
            | 'O' -> if (x + 1) / 2 = ox then '[' else ']'
            
        Array2D.init (w * 2) h expandedCell
     
    let solve (wh: Warehouse) : int =
        let { Moves = moves; Layout = grid; Pos = pos } = wh
       
        // Expand the grid and position to match 
        let mutable grid = expandGrid grid
        let pos =
            match pos with
            | x, y -> x * 2, y
        
        // Recurse through the pushes to figure out what swaps we need to make and return them as a list of swaps or None for a blocked path 
        let rec stagePush (isPushingNeighbor: bool) (c: char) (pos: (int * int)) (m: Direction) : ((int * int) * (int * int)) list option =
            
            let shouldPushNeighbor = (not isPushingNeighbor) && (c = '[' || c = ']') && (m = U || m = D)
            let pushNeighborResult =
                if shouldPushNeighbor then
                    let x, y = pos
                    match c with
                    | '[' -> stagePush true ']' (x + 1, y) m
                    | ']' -> stagePush  true '[' (x - 1, y) m
                else Some []
             
            match pushNeighborResult with
            | None -> None
            | Some rest -> 
             
            let nc, np = CharGrid.next grid pos m
            match nc with
            | '#' -> None
            | '.' -> Some ((np, pos) :: rest)
                
            | '[' | ']' ->
                 match stagePush false nc np m with
                 | Some swaps -> Some ((np, pos) :: swaps @ rest)
                 | None -> None
            
        let push (pos: (int * int)) (m: Direction) : bool =
            let x, y = pos
            let c = grid[x,y]
            match stagePush false c pos m with
            | None -> false
            | Some swaps ->
                swaps |> Seq.rev |> Seq.distinct |> Seq.iter (uncurry (CharGrid.swap grid))
                true
            
        // cases, next char is:
        // . - update pos
        // # - do nothing
        // O - update pos and push
        let move (pos: (int * int)) (m: Direction) : (int * int) =
            let nc, np = CharGrid.next grid pos m
            match nc with
            | '.' -> np
            | '#' -> pos
            | '[' | ']' -> if push np m then np else pos
   
        // Calculate the GPS score of a location 
        let gpsScore x y = 100 * y + x
      
        let moveAndPrint p m =
            printfn "move: %A" m
            let newPos = move p m
            CharGrid.withOverlay grid '@' [ newPos ] |> CharGrid.debugPrint
            newPos
            
        // Do all moves, this mutates the grid as a sideffect 
        // moves |> Seq.fold moveAndPrint pos |> ignore
        moves |> Seq.fold move pos |> ignore
        
        // Score the grid 
        Array2DExt.toSeq grid
        |> Seq.filter (fun (_, _, c) -> c = '[')
        |> Seq.map (fun (x, y, _) -> gpsScore x y)
        |> Seq.sum

    [<Test>]
    let ``smaller example input``() =
        smallerExampleInput
        |> parseWarehouse
        |> solve
        |> should equal 618
            
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseWarehouse
        |> solve
        |> should equal 9021
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day15/input.txt"
        |> parseWarehouse
        |> solve
        |> should equal 1543141
        
