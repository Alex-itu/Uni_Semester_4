module Exam2024

(* 
    As you work make sure your code always compiles. Rebuild often to make sure you haven't broken anything.
    
    You can work with the exam in three ways
    
    1. Print test results in Program.fs and run the project.
    2. Export code snippets to the interactive environment (mark code and press Alt-Enter on VS)
    3. Write #load "ExamInteractive.fsx", or export the code in ExamInteractive.fsx in the same way as point 2,
       in the interactive environment. Depending on your platform you may have
       to change the paths. A complete path to the file should always work.
*)

(* 2019 Re-exam *)
    let f s =
        let l = String.length s
        let rec aux =
            function
            | i when i = l -> []
            | i -> s.[i] :: aux (i + 1)
        aux 0
         
     (* Show that f is not tail recursive *)
     (*
     f "hello" -->
     let l = String.length "hello"
     let rec aux =
         function
         | i when i = l -> []
         | i -> "hello".[i] :: aux (i + 1)
     aux 0 -->
     let rec aux =
         function
         | i when i = 5 -> []
         | i -> "hello".[i] :: aux (i + 1)
     aux 0 -->
     "hello".[0] :: aux (0 + 1) -->
     'h' :: aux 1 -->
     'h' :: "hello".[1] :: aux (1 + 1) -->
     'h' :: 'e' :: aux (2) -->
     'h' :: 'e' :: "hello".[2] :: aux (2 + 1) -->
     'h' :: 'e' :: 'l' :: aux 3 -->
     'h' :: 'e' :: 'l' :: "hello".[3] :: aux (3 + 1) -->
     'h' :: 'e' :: 'l' :: 'l' :: aux (4) -->
     'h' :: 'e' :: 'l' :: 'l' :: "hello".[4] :: aux (4 + 1) -->
     'h' :: 'e' :: 'l' :: 'l' :: 'o' :: aux (5) -->
     'h' :: 'e' :: 'l' :: 'l' :: 'o' :: [] -->
     ['h'; 'e'; 'l'; 'l'; 'o']
     *)
    let fTail s =
        let l = String.length s
        let rec aux cont =
            function
            | i when i = l -> cont []
            | i -> aux (fun result -> cont (s.[i] :: result)) (i + 1)
        aux id 0
         
         
    let rec foo =
        function
        | 0            -> true
        | x when x > 0 -> bar (x - 1)
        | x            -> bar (x + 1)
    and bar =
        function
        | 0            -> false
        | x when x > 0 -> foo (x - 1)
        | x            -> foo (x + 1)
        
    let rec baz =
        function
        | []                 -> [], []
        | x :: xs when foo x ->
            let ys, zs = baz xs
            (x::ys, zs)
        | x :: xs ->
            let ys, zs = baz xs
            (ys, x::zs)
       

    let bazTail =
       let rec aux cont =
           function
           | []  -> cont ([], [])
           | x :: xs when foo x ->
               aux (fun result -> 
                       let ys, zs = result
                       cont (x::ys, zs)) xs
           | x :: xs ->
               aux (fun result -> 
                       let ys, zs = result
                       cont (ys, x::zs)) xs
            
       aux id
       
    let bazTail2 =
       let rec aux cont =
           function
           | []  -> cont ([], [])
           | x :: xs when foo x ->
               aux (fun (ys, zs) -> cont (x::ys, zs)) xs
           | x :: xs ->
               aux (fun (ys, zs) -> cont (ys, x::zs)) xs
            
       aux id
       
    (* evaluate baz *)
     
     (*
     baz [1; 2] -->
     let ys, zs = baz [2]
     (ys, 1::zs) -->
     let ys, zs =
        let ys, zs = baz []
        (2::ys, zs)
     (ys, 1::zs) -->
     let ys, zs =
        let ys, zs = [], []
        (2::ys, zs)
     (ys, 1::zs) -->
     let ys, zs = (2::[], [])
     (ys, 1::zs) -->
     let ys, zs = ([2], [])
     (ys, 1::zs) -->
     ([2], 1::[]) -->
     ([2], [1])
     *)
        
     
     (* Re-exam 2023 *)
     
(* 4: BASIC *)
    
    
    type var = string

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of var              // Variable lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
    
    type stmnt =
    | If of expr * uint32       // Conditional statement (if-then (no else)).
    | Let of var * expr        // Variable update/declaration
    | Goto of uint32           // Goto
    | End                      // Terminate program
      
    type prog = (uint32 * stmnt) list  // Programs are sequences of commands with their own line numbers 

    
    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    
    let fibProg xarg =  
        [(10u, Let("x",    Num xarg))                         // x = xarg
         (20u, Let("acc1", Num 1))                            // acc1 = 1
         (30u, Let("acc2", Num 0))                            // acc2 = 0
         
         (40u, If(Lookup "x", 60u))                           // if x > 0 then goto 60 (start loop)
         
         (50u, Goto 110u)                                     // Goto 110 (x = 0, terminate program)
         
         (60u, Let ("x", Lookup "x" .-. Num 1))               // x = x - 1
         (70u, Let ("result", Lookup "acc1"))                 // result = acc1
         (80u, Let ("acc1", Lookup "acc1" .+. Lookup "acc2")) // acc1 = acc1 + acc2
         (90u, Let ("acc2", Lookup "result"))                 // acc2 = result
         (100u, Goto 40u)                                     // Goto 40u (go to top of loop)
         
         (110u, End)                                          // Terminate program
                                                              // the variable result contains the
                                                              // fibonacci number of xarg
         ]

(* Question 4.1: Basic programs *)

    type basicProgram = Map<uint32, stmnt>
    
    let mkBasicProgram prog : basicProgram = Map.ofList prog
    
    let getStmnt l (p : basicProgram) = Map.find l p
        
    
    let nextLine l (p : basicProgram) =
        Map.findKey (fun l' _ -> l' > l) p
    
    let firstLine (p : basicProgram) = Map.minKeyValue p |> fst
    
(* Question 4.2: State *)

    
    type state = {lineNumber : uint32; vars :  Map<string, int>}
    
    let emptyState (p : basicProgram) =
        { lineNumber = firstLine p; vars = Map.empty }
        
    let goto l st = {st with lineNumber = l}
    let gotoTuple l (_, vs) = (l, vs) // if your type is a tuple
    

    let getCurrentStmnt p st = getStmnt st.lineNumber p
    
    let update v a st = {st with vars = Map.add v a st.vars}
    
    let lookup v st = Map.find v st.vars
    
    
(* Question 4.3: Evaluation *)
    
    let rec evalExpr e st =
        match e with
        | Num x -> x
        | Lookup v -> lookup v st
        | Plus(e1, e2) -> evalExpr e1 st + evalExpr e2 st
        | Minus(e1, e2) -> evalExpr e1 st - evalExpr e2 st    
    
    let step p st = {st with lineNumber = nextLine st.lineNumber p}
  
        
    let evalProg p =
        let rec aux st =
            match getCurrentStmnt p st with
            | If(e, l) when evalExpr e st <> 0 -> aux (goto l st)
            | If(_, _)                         -> aux (step p st)
            | Let(v, e) -> aux (step p (update v (evalExpr e st) st))
                           // st |> evalExpr e |> update v |> step p |> aux
            | Goto l    -> aux (goto l st)
            | End       -> st
        aux (emptyState p)
    
(* Question 4.4: State monad *)
    type StateMonad<'a> = SM of (basicProgram -> state -> 'a * state)  
      
    let ret x = SM (fun _ s -> (x, s))
    
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun p s ->
            let x, s' = a p s
            let (SM g) = f x
            g p s')
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM p (SM f) = f p (emptyState p)

    let goto2 l = SM (fun _ st -> ((), goto l st))
    
    let getCurrentStmnt2 = SM (fun p st -> (getCurrentStmnt p st, st))
    
    let update2 v a = SM (fun _ st -> ((), update v a st))
    
    let lookup2 v = SM (fun _ st -> (lookup v st, st))
    
    let step2 = SM (fun p st -> ((), step p st))

(* Question 4.5: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    
    
    let rec evalExpr2 e =
        match e with
        | Num x -> ret x
        | Lookup v -> lookup2 v
        | Plus (e1, e2) ->
            evalExpr2 e1 >>=
            (fun x -> evalExpr2 e2 >>=
                      (fun y -> ret (x + y)))
        | Minus (e1, e2) ->
            evalExpr2 e1 >>=
            (fun x -> evalExpr2 e2 >>=
                      (fun y -> ret (x - y)))
                
    
    let rec evalProg2 =
        getCurrentStmnt2 >>=
        (function
         | If(e, l) ->
             evalExpr2 e >>=
             (fun x ->
                 if x <> 0 then
                     goto2 l >>>= evalProg2
                 else
                     step2 >>>= evalProg2)
         | Let(v, e) ->
             evalExpr2 e >>= (fun x -> update2 v x >>>= step2 >>>= evalProg2)
         | Goto l -> goto2 l >>>= evalProg2
         | End    -> ret ()
        )
    
    (* Same assignment, but using computation expressions.
       I find this cleaner and easier to understand *)
        
    let rec evalExpr3 e =
        state {
            match e with
            | Num x -> return x
            | Lookup v -> return! lookup2 v
            | Plus(e1, e2) ->
                let! v1 = evalExpr3 e1
                let! v2 = evalExpr3 e2
                return v1 + v2
            | Minus(e1, e2) ->
                let! v1 = evalExpr3 e1
                let! v2 = evalExpr3 e2
                return v1 - v2
        }
      
    let rec evalProg3 =
        state {
            let! s = getCurrentStmnt2
            match s with
            | If(e, l) ->
                let! v = evalExpr3 e
                if v <> 0 then
                    do! goto2 l
                    do! evalProg3
                else
                    do! step2
                    do! evalProg3
            | Let(v, e) ->
                let! x = evalExpr3 e
                do! update2 v x
                do! step2
                do! evalProg3
            | Goto l ->
                do! goto2 l
                do! evalProg3
            | End ->
                return ()
        }
        
    (* Example of adding signature
       Even if you cannot solve add2, if you know that it
       has the type int -> string -> int you can give the signature
       and still get full points for mul5Add2 that uses add2.
    *)
        
    let add2 (_ : int) (_ : string) : int = failwith "not implemented"
    let mul5Add2 x = add2 (x * 5) "hello"