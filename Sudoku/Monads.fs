module Sudoku.Monads

type SodokuBoard = {
    n : int
    board : Option<int> [,]
}

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b
    
type SodokuError =
    | Unknown


type BM<'a> = B of (SodokuBoard -> Result<'a * SodokuBoard, SodokuError>)
let bind (f : 'a -> BM<'b>) = function
    | B a ->
            B (fun s ->
                match a s with
                | Success (b, s') ->
                    match f b with
                    | B g -> g s'
                | Failure err -> Failure err)
                
let (>>=) x f = bind f x
let (>>>=) x f = x >>= (fun () -> f)