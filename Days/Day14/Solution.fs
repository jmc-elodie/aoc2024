module AoC2024.Days.Day14.Solution

open System
open System.Numerics
open System.Text.RegularExpressions
open AoC2024.Util
open NUnit.Framework
open FsUnit

let exampleInput = 
    """
    p=0,4 v=3,-3
    p=6,3 v=-1,-3
    p=10,3 v=-1,2
    p=2,0 v=2,-1
    p=0,0 v=1,3
    p=3,0 v=-2,-2
    p=7,6 v=-1,-3
    p=3,0 v=-1,-2
    p=9,3 v=2,3
    p=7,3 v=-1,2
    p=2,4 v=2,-3
    p=9,5 v=-3,-3
    """
    
let parseRobots (str: string) =
    let parseLine (line: string) : (Vector2 * Vector2) =
        let m = Regex.Match(line, @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
        let parts = [1..4] |> Seq.map (fun i -> Int32.Parse(m.Groups[i].Value) |> float32) |> Seq.toArray
        (Vector2(parts[0], parts[1]), Vector2(parts[2], parts[3]))
        
    str
    |> ParseInput.lines
    |> Seq.map parseLine

module PartOne =
    
    let solve (width: int) (height: int) (robots: (Vector2 * Vector2) seq) : int =
        let seconds = 100.0f
        let fwidth, fheight = float32 width, float32 height
        
        let locAfterSeconds (t: float32) (pos: Vector2, velocity: Vector2) : Vector2 =
            let fmod a b = a - b * (floor (a / b))
            
            let x = pos.X + velocity.X * t
            let y = pos.Y + velocity.Y * t
            Vector2(fmod x fwidth, fmod y fheight)
        
        let toQuadrants (loc: Vector2) : int = 
            let wq, hq = floor (fwidth / 2.0f), floor (fheight / 2.0f)
            
            if loc.X < wq && loc.Y < hq then
                1
            elif loc.X > wq && loc.Y < hq then 
                2
            elif loc.X < wq && loc.Y > hq then
                3
            elif loc.X > wq && loc.Y > hq then
                4
            else 0
            
        let mul a b = a * b
         
        robots
        |> Seq.map (locAfterSeconds seconds)
        |> Seq.groupBy toQuadrants
        |> Seq.filter (fun (q, _) -> q <> 0)
        |> Seq.map (snd >> Seq.length)
        |> Seq.reduce mul
        
    [<Test>]
    let ``example input``() =
        exampleInput
        |> parseRobots 
        |> (solve 11 7)
        |> should equal 12
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day14/input.txt" 
        |> parseRobots 
        |> (solve 101 103)
        |> should equal 221142636

 
module PartTwo =
   
    let genChristmasTree (width: int) (height: int) : Vector2 seq =
        // let's say a Christmas tree is a triangle centered on the image, with a square at the bottom. 
        // 11 x 7 looks like:
        // .....X.....
        // ....X.X....
        // ...X...X...
        // ..X.....X..
        // .X.......X.
        // X.........X
        // ....X.X....
        
        // 15% is trunk, 85% is tree ??
        // each row has 2 points on it
     
        let fwidth, fheight = float32 width, float32 height
        let halfWidth = floor (fwidth / 2.0f)
        
        let treeRows = int (ceil (fheight * 0.85f))
        let treeStep: float32 = (halfWidth + 1.0f) / (float32 treeRows)
      
        let rowToTree (i: int) : Vector2 seq =
            let dx = treeStep * (float32 i) |> round
            let points = 
                if i = 0 then
                    [ Vector2(halfWidth, 0.0f) ]
                else
                    [ 
                        Vector2(halfWidth - dx, float32 i)
                        Vector2(halfWidth + dx, float32 i)
                    ]
            Seq.ofList points 
            
        let rowToTrunk (i: int) : Vector2 seq =
            [ 
                Vector2(halfWidth - 1.0f, float32 i)
                Vector2(halfWidth + 1.0f, float32 i)
            ]
            
        let tree = 
            [0..(treeRows - 1)]
            |> Seq.collect rowToTree
            
        let trunk =
            [treeRows..(height - 1)]
            |> Seq.collect rowToTrunk
            
        Seq.append tree trunk
       
        
    let solve (width: int) (height: int) (robots: (Vector2 * Vector2) seq) : int =
        let maxSeconds = 10000.0f
        let fwidth, fheight = float32 width, float32 height
        
        let debugPrint (robots: Vector2 seq) =
            let mutable grid : char[,] = Array2D.create width height '.'
            
            let drawPoint (loc: Vector2) =
                let x, y = int loc.X, int loc.Y
                grid[x, y] <- 'X'
            
            robots |> Seq.iter drawPoint
            
            CharGrid.debugPrint grid
            
        let locAfterSeconds (t: float32) (pos: Vector2, velocity: Vector2) : Vector2 =
            let fmod a b = a - b * (floor (a / b))
            
            let x = pos.X + velocity.X * t
            let y = pos.Y + velocity.Y * t
            Vector2(fmod x fwidth, fmod y fheight)
       
        let allRobotLocsAtTime (t: float32) = robots |> Seq.map (locAfterSeconds t)
        
        let refTree = genChristmasTree width height |> Seq.cache
        
        let distFromRef x y _ =
            let p = Vector2(float32 x, float32 y)
            refTree |> Seq.map (fun rp -> Vector2.DistanceSquared(p, rp)) |> Seq.min
        
        let refTreeIndex : float32[,] = Array2D.zeroCreate width height |> Array2D.mapi distFromRef
        let indexedDistance (loc: Vector2) : float32 =
            let x, y = int loc.X, int loc.Y
            refTreeIndex[x, y]
            
        let computeTotalDistance (robots: Vector2 seq) : float32 =
            robots |> Seq.map indexedDistance |> Seq.sum
            
        [0..((int maxSeconds) - 1)] 
        |> Seq.map (fun t -> 
            let dist = 
                t 
                |> float32 
                |> allRobotLocsAtTime 
                |> computeTotalDistance
            t, dist)
        |> Seq.minBy snd
        |> fst
        |> tee (float32 >> allRobotLocsAtTime >> debugPrint)
        
    [<Test>]
    let ``problem input``() =
        readTextFromFile @"Days/Day14/input.txt" 
        |> parseRobots 
        |> (solve 101 103)
        |> should equal 7916

 
