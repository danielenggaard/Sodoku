open System
type board = Option<int> [,]
type coord = int * int
type neighbors = coord -> Option<int> []
let sqrtI = float >> Math.Sqrt >> int
let ( .**) (a:int) (b:int) = (float a) ** (float b) |> int
let emptyB : unit -> board = fun _ -> Array2D.init 9 9 (fun _ _ -> None)
let legalNumber max = function
    | n when n >= 0 && n <= max -> true
    | _ -> false
    
let availableColors = Enum.GetValues(typeof<ConsoleColor>) :?> (ConsoleColor []) |>
                          Array.filter (fun e -> e <> ConsoleColor.Black &&
                                                 e <> ConsoleColor.Gray &&
                                                 e <> ConsoleColor.DarkGray)

/// Return the numbers in the same row.
let row (b:board) (_, row) = b.[*, row]

/// Return the numbers in the same column.
let col (b:board) (col, _) = b.[col, *]

/// Given coordinate and number of groups return the given group number.
let groupNumber (col, row) = sqrtI >> fun length -> col / length + (row / length) * length

let allDiff (row:neighbors) (col:neighbors) (grp:Option<int> []) (c:coord) number =
    Array.Parallel.map (Array.exists((=) number)) [| row c; col c; grp |] |>
    Array.exists id |>
    not
    

/// Print the board by enumerating through all the columns.
let groupMembers numGrps (board:board) grp =
   let grpLengths = sqrtI numGrps
   let colS = grp % grpLengths * grpLengths
   let rowS = grp / grpLengths * grpLengths
   let arr = board.[colS..colS + grpLengths - 1,
                    rowS..rowS + grpLengths - 1]
   [| for col in 0..Array2D.length1 arr - 1 do yield! arr.[col, *] |]

let legalMove grps (b:board) (c:coord) (number:int) =
    allDiff (row b) (col b) (groupMembers grps b (groupNumber c grps)) c (Some number) &&
    legalNumber grps number
    
let place board (col, row) = Array2D.set board col row
    
let space = "   "
let formatPrint = function
    | None -> printf "X%s" space
    | Some v -> printf "%i%s" v space
    
let consoleColor n coord =
    Console.ForegroundColor <- availableColors.[groupNumber coord n % availableColors.Length]
    
let resetColors () =
    Console.BackgroundColor <- ConsoleColor.Black
    Console.ForegroundColor <- ConsoleColor.White
    
let standardPrinter n coord v =
    consoleColor n coord
    formatPrint v
    
let printRow rowN printer=
    resetColors()
    printf "\n%i%s" (rowN + 1) space
    Array.iteri(fun i e -> printer (i, rowN) e)
    
let newLines () = printf "\n\n"
    
let print (l:int) (b:board) printer () =
                        resetColors()
                        newLines ()
                        printf " "
                        for x in 1..l do printf "%s%i" space x
                        for x in 0..l - 1 do printRow x printer b.[*, x]
                        newLines ()
                            

let play () =
    
    let b = emptyB()
    let plc = place b                      // Place a number
    let legal = legalMove 9 b              // Check if a move is legal
    let prnt = print 9 b (standardPrinter 9)
    
    
    let rec aux () =
        prnt ()
        let col = Console.ReadLine() |> int |> (-) 1 |> abs
        let row = Console.ReadLine() |> int |> (-) 1 |> abs
        let n = Console.ReadLine() |> int
        
        if legal (col, row) n then
            plc (col, row) (Some n)
        else
            printf "\n\nIllegal (%i, %i) - %i\n\n" col row n
        
        aux ()
        ()
    
    aux ()


open System

[<EntryPoint>]
let main argv =
    play()
    printfn "Hello World from F#!"
    0 // return an integer exit code
