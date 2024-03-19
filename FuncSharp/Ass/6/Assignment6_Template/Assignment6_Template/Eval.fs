module Eval

    open StateMonad
    open Types

    (* Code for testing *)

    let hello = [('H', 4); ('e', 1); ('l', 1); ('l', 1); ('o', 1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    //green 6.7
    // I made these into a big helper function at 6.9, so i am using these
    let add a b = a >>= (fun s -> b >>= (fun ss -> ret (s + ss)))      
    
    let sub a b = a >>= (fun s -> b >>= (fun ss -> ret (s - ss)))

    //green 6.8
    // I made these into a big helper function at 6.9, so i am using these
    let div a b = a >>= (fun s -> b >>= (fun ss -> 
        match s, ss with
        | _, 0 -> fail DivisionByZero
        | s, ss -> ret (s / ss)
        )) 

    let mul a b = a >>= (fun s -> b >>= (fun ss -> ret (s * ss)))   

    let modd a b = a >>= (fun s -> b >>= (fun ss -> 
        match s, ss with
        | _, 0 -> fail DivisionByZero
        | s, ss -> ret (s % ss)
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

    let doSomething a b tp=
        match tp with
        | "add" -> a >>= (fun s -> b >>= (fun ss -> ret (s + ss)))
        | "sub" ->  a >>= (fun s -> b >>= (fun ss -> ret (s - ss)))
        | "mul" ->  a >>= (fun s -> b >>= (fun ss ->  ret (s * ss)))
        | "div" -> a >>= (fun s -> 
            b >>= (fun ss -> 
            match s, ss with
            | _, 0 -> fail DivisionByZero
            | s, ss -> ret (s / ss)
            ))
        | "mod" -> a >>= (fun s -> b >>= (fun ss -> 
            match s, ss with
            | _, 0 -> fail DivisionByZero
            | s, ss -> ret (s % ss)
            )) 

    //green 6.9
    let rec arithEval a : SM<int> = 
        match a with
        | N x -> ret x
        | V x -> lookup x 
        | WL -> wordLength
        | PV x -> (arithEval x) >>= pointValue
        | Add(x, z) -> doSomething (arithEval x)  (arithEval z) "add"
        | Sub(x, z) -> doSomething (arithEval x)  (arithEval z) "sub"
        | Mul(x, z) -> doSomething (arithEval x)  (arithEval z) "mul"
        | Div(x, z) -> doSomething (arithEval x)  (arithEval z) "div"
        | Mod(x, z) -> doSomething (arithEval x)  (arithEval z) "mod"
        | CharToInt x -> (charEval x) >>= (fun s -> ret (int s))
    and charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | ToUpper x -> charEval x >>= (fun z -> ret (System.Char.ToUpper(z)))
        | ToLower x -> charEval x >>= (fun z -> ret (System.Char.ToLower(z)))
        | CV x -> (arithEval x) >>= characterValue 
    
    
    // for some reason, I cant do this:
    // let doSomethingBool (a : SM<'a>) (b : SM<'a>) tp =
    //     match tp with
    //     | "aeq" -> a >>= (fun s -> b >>= (fun ss -> ret (s = ss)))
    //     | "alt" -> a >>= (fun s -> b >>= (fun ss -> ret (s < ss)))
    //     | "conj" -> a >>= (fun s -> b >>= (fun ss -> ret (s && ss)))
    //     | "???" -> a >>= (fun s -> b >>= (fun ss -> ret (s > ss)))
    
    // let isVowel c = 
    //   let bigC = c
    //   match bigC with
    //   |  'A' | 'E' | 'I' | 'O' | 'U' -> ret true
    //   | _ -> ret false
    
    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        
        | AEq(x, z) -> (arithEval x) >>= (fun s -> (arithEval z) >>= (fun ss -> ret (s = ss)))
        | ALt(x, z) -> (arithEval x) >>= (fun s -> (arithEval z) >>= (fun ss -> ret (s < ss)))

        | Not x -> (boolEval x) >>= (fun z -> ret (not z))
        | Conj(x, z) -> (boolEval x) >>= (fun s -> (boolEval z) >>= (fun ss -> ret (s && ss)))

        | IsDigit x -> (charEval x) >>= (fun s -> ret (System.Char.IsDigit(s)))
        | IsLetter x -> (charEval x) >>= (fun s -> ret (System.Char.IsLetter(s)))
        | IsVowel x -> (charEval x) >>= (fun s -> ret(
            match s with
            |  'A' | 'E' | 'I' | 'O' | 'U' -> true
            | _ -> false))

    


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
    