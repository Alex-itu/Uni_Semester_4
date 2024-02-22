module Rational.Rational

    type Rat = R of int * int

    // type 'A List = 
    //     | Nil
    //     |

    let mkRat a b = R (a, b) 
    let add r1 r2 =
        match r1, r2 with
        | R (a, b), R (c, d) -> mkRat (a * d + b * c)  (b * d)

    let addExtra (R (a, b))  (R (c, d)) = R (a * d + b * c, b * d)

    let sub (R (a, b))  (R (c, d)) = mkRat (a * d - b * c) (b * d)