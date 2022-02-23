module Rational

    [<Sealed>]
    type Rat =
        static member ( + ) : Rat * Rat -> Rat
        static member ( - ) : Rat * Rat -> Rat
        static member ( * ) : Rat * Rat -> Rat
        static member ( / ) : Rat * Rat -> Rat

    val mkRat : int -> int -> Rat
    val fromRat : Rat -> (int * int)

