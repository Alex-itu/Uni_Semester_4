module Eval

    open StateMonad
    open Types

    (* Code for testing *)

    let hello = [('H', 4); ('e', 1); ('l', 1); ('l', 1); ('o', 1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    //green 6.7
    let add a b = a >>= (fun s -> b >>= (fun ss -> ret (s + ss)))      
    
    let sub a b = a >>= (fun s -> b >>= (fun ss -> ret (s - ss)))

    //green 6.8
    let div a b = a >>= (fun s -> b >>= (fun ss -> 
        match s, ss with
        | _, 0 -> fail DivisionByZero
        | s, ss -> ret (s / ss)
        )) 

    let mul a b = a >>= (fun s -> b >>= (fun ss -> 
        match s, ss with
        | _, 0 -> fail DivisionByZero
        | s, ss -> ret (s * ss)
        ))     

    let modd a b = a >>= (fun s -> b >>= (fun ss -> 
        match s, ss with
        | _, 0 -> fail DivisionByZero
        | s, ss -> ret (s * ss)
        )) 

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    //green 6.9
    let rec arithEval a : SM<int> = 
        match a with
        | N x -> ret x
        | V x -> lookup x 
        | WL -> wordLength
        | PV x -> (arithEval x) >>= pointValue
        | Add(x, z) -> add (arithEval x)  (arithEval z) 
        | Sub(x, z) -> sub (arithEval x)  (arithEval z) 
        | Mul(x, z) -> mul (arithEval x)  (arithEval z)
        | Div(x, z) -> div (arithEval x)  (arithEval z)
        | Mod(x, z) -> modd (arithEval x)  (arithEval z)

    let rec charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | ToUpper x -> System.Char.ToUpper(charEval x)
        | ToLower x -> System.Char.ToLower(charEval x)
        | CV x -> charEval (match w |> List.tryItem(arithEval x w s) with
                                    | Some (cc, _) -> C cc
                                    | _ -> C ' ') w s
      

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false

        | AEq(x, z) -> arithEval x  = arithEval z
        | ALt(x, z) -> arithEval x  < arithEval z

        | Not x -> not (boolEval x)
        | Conj(x, z) -> boolEval x w s && boolEval z w s

        | IsDigit x -> System.Char.IsDigit(charEval x w s)
        | IsLetter x -> System.Char.IsLetter(charEval x w s)
        | IsVowel x -> isVowel x


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c x boardStmnt ids = failwith "Not implemented"
    