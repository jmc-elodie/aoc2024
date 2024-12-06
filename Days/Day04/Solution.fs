module AoC2024.Days.Day04.Solution

open AoC2024.Util
open NUnit.Framework
open FsUnit


let exampleInput =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """.Trim()
            

module PartOne =
    
    // This generates a list of tuples of the possible directions we can match in
    // e.g. [(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)]
    let directions = permuteN 2 [-1 .. 1] |> List.except [ [0; 0] ] |> List.map (fun x -> (x[0], x[1]))
   
    // This takes a direction and a length and creates a list of all the relative
    // coordinates for that search.
    // for example given the direction (1, 0) which is forward in x, and a length of 4, you get
    // [(0, 0); (1, 0); (2, 0); (3, 0)]
    let searchDirections n (xDir, yDir) =
        [ 0 .. n - 1 ]
        |> List.map (fun i -> (i * xDir, i * yDir))
       
    // Given a word and a board, searches the board for the word and returns the total number of occurrences 
    let wordSearch (word: string) (board: char[,]) : int =
        let wordChars = word.ToCharArray() |> Array.toList
        
        // This index has every relative search coordinate and the letter it needs to be to match, for example
        // [ [ ('X', (0, 0)); ('M', (1, 0)); ('A', (2, 0)); ('S', (3, 0)) ]; ... ]
        // Given a cell location in the board, you can add every relative coordinate and check the letter
        // if this matches, then the word is found at that point in this direction.
        let searchIndex =
            directions
            |> List.map (searchDirections wordChars.Length)
            |> List.map (List.zip wordChars)
      
        // Given a cell location, uses the index to search in every direction and return the count of matches 
        let countWordMatchesInCell cellX cellY =
            searchIndex
            |> Seq.map (
                Seq.map (fun (c,(dx, dy)) ->
                    let x, y = cellX + dx, cellY + dy
                    if Array2DExt.inBounds x y board then
                        board[x, y] = c
                    else
                        false)
                >> SeqExt.all)
            |> SeqExt.countTrue
      
        // Folder to iterate every cell of the board and count the matches in each cell and return the sum
        let folder acc x y _ =
            acc + (countWordMatchesInCell x y)
            
        board |> Array2DExt.foldi folder 0
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> ParseInput.charArray2D
        |> wordSearch "XMAS"
        |> should equal 18
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day04\input.txt"
        |> ParseInput.charArray2D
        |> wordSearch "XMAS"
        |> should equal 2583


module PartTwo =
    
    let xmasRefs =
        [
            """
            M.S
            .A.
            M.S
            """

            """
            M.M
            .A.
            S.S
            """

            """
            S.M
            .A.
            S.M
            """

            """
            S.S
            .A.
            M.M
            """
        ] |> List.map ParseInput.charArray2D
    
    // Search every cell of the board for matches against the provided refs and return the total number of matches 
    let refSearch (refs: char[,] list) (board: char[,]) =
        
        // This compares the reference (passed in with rx, ry and v) to the board cell
        // if the board cell is 0,0 and the reference is 1, 1, S then the board's
        // 0 + 1, 0 + 1 cell will be compared to S; matches will return true.
        let refMatch cx cy rx ry v =
            let x, y = (cx + rx, cy + ry)
            match v with
            | '.' -> true
            | c ->
                if Array2DExt.inBounds x y board then
                    board[x, y] = c
                else
                    false
        
        // This will search the board at cx, cy against all refs and return the number of matches
        let countRefMatches cx cy =
            refs
            |> List.map (
                Array2D.mapi (refMatch cx cy)
                >> SeqExt.ofArray2D
                >> SeqExt.all)
            |> SeqExt.countTrue
        
        let folder acc x y _ =
            acc + (countRefMatches x y)
            
        board |> Array2DExt.foldi folder 0
    
    [<Test>]
    let ``example input``() =
        exampleInput
        |> ParseInput.charArray2D
        |> refSearch xmasRefs
        |> should equal 9
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days\Day04\input.txt"
        |> ParseInput.charArray2D
        |> refSearch xmasRefs
        |> should equal 1978
