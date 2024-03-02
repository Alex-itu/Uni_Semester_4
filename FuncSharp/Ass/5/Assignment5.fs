
module Assignment5

let modulebeingABip _ = failwith "Module problem"
(* Exercise 5.1 *)
// let rec sum m n = 
//     match m, n with
//     | x, 1 -> x + 1 
//     | x, z -> x + sum x (z-1) 
let sum m n =
    let rec count acc z =
        match acc, z with
        | _, z when z = n -> acc + n
        | _, z -> count (acc + (m + z)) (z + 1)
    count m 0

(* Exercise 5.2 *)

let length (lst : 'a list) = 
    let rec count acc (lst : 'a list) =
        match lst with
        | [] -> acc
        | x :: xs -> count (acc+1) xs
    count 0 lst

(* Exercise 5.3 *)

let foldBack f lst acc = 
    let rec nestedFunc lst folder =
        match lst with
        | [] -> folder acc
        | x :: xs -> nestedFunc xs (fun r -> folder (f x r))
    nestedFunc lst id
    


(* Exercise 5.4 *)

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x' -> aux (x' * acc) (x' - 1)
    aux 1 x

let factC x = 
    let rec aux folder acc  =
        match acc with
        | 0 -> folder 1
        | x' -> aux (fun r -> folder (x' * r)) (x' - 1)
    aux id x

(* TODO: *)
(* Compare the running time between factA and factC. Which solution is faster and why? 
FactA is faster then factC by a lot. When given 1000000, factC takes 00:00:00.022,
whereas factA only takes 00:00:00.002. This big different must be to all the extra functions
that factC has, since each recursing "makes" a new function. that then has to be counted up.
that or I did something wrong with it. I thought that continuation would be faster, but i guess
even if you make sure that the heap is used instead of the stack frames, it does not mean it's faster.
And so just having a "normal" recursion is faster in this case.

ps. I couldnt figuer out how to do this with function instead match with.
I dont think that has anything to do with the runtime, so i think this is right. 
*)

(* Exercise 5.5 *)

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA _ = failwith "not implemented"

let fibC _ = failwith "not implemented"


(* TODO: *)
(* Compare the running time of fibW, fibA and fibC. Which solution is faster and why? 
<Your answer goes here>

*)

(* Exercise 5.6 *)

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(* TODO *)
(* The call bigListK id 130000 causes a stack overflow. 
Analyse the problem and describe exactly why this happens. 
Why is this not an iterative function?

To make a compelling argument (and to prepare for the exam) you must make a step-by-step evalution of a call to
bigListK. Test correctness of your evaluations by ensuring that they all evaluate to the same value. For example:

(5 + 4) * 3 -->
9 * 3 -->
27

If you input any of these lines into FSharp Interactive it will produce the same result. Do the same here and point
to where you can see that this function is not tail recursive.

<Your answer goes here>
*)

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | CharToInt of cExp


and cExp =
    | C  of char  (* Character value *)
    | CV of aExp  (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp


let arithEvalSimple _ = failwith "not implemented"

let charEvalSimple _ = failwith "not implemented"

let arithEval _ = failwith "not implemented"
let charEval _ = failwith "not implemented"