module Rational

    type Rat = 
        | R of int * int
        static member ( + ) ((R(a, b)),(R(c, d))) =
            R(a * d + b*c, b*d)
    
        static member ( - ) ((R(a, b)),(R(c, d))) =
            R(a * d - b*c, b*d)
    
        static member ( * ) ((R(a, b)) ,(R(c, d))) =
            R(a * c, b * d)
    
        static member ( / ) (r, (R(c, d))) =
            r * R(d,c)

    
    let mkRat a b = R(a, b)

    let fromRat (R(a,b)) = (a,b)

    
   
    
    
    