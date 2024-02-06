module Assignment1

    open System
    
    let sqr x = x * x

    let pow x n = System.Math.Pow (x, n)
    
    let rec sum n = 
        match n with
        | 0 -> 0
        | n -> n + sum(n-1);;

    let rec fib n  =
        match n with
        | 0 -> 0
        | 1 -> 1
        | n -> fib(n-1) + fib(n-2)


    let dup s: string = s + " " + s + ""

    let rec dupn s n = 
        match n with
        | 0 -> ""
        | n -> s + " " + dupn s (n-1)  

    let rec bin (n, k) =
        match (n, k) with
        | n, 0 -> 1
        | n, k when k = n -> 1
        | n, k  when n > k ->  bin ((n-1), (k-1)) + bin ((n-1), k)

    let timediff (hh1, mm1) (hh2, mm2) = (hh2*60 - hh1*60) + (mm2 - mm1)

    let minutes (t1, t2) = timediff(0, 0)(t1,t2)

    let curry  f x y  = f (x, y)

    let uncurry f (x, y) = f x y
    
    let empty (letter, pointValue) = fun pos -> (letter, pointValue) 
    
    let add _ = failwith "not implemented"

    let singleLetterScore _ = failwith "not implemented"
    let doubleLetterScore _ = failwith "not implemented"
    let trippleLetterScore _ = failwith "not implemented"

    let hello _ = failwith "not implemented"