module Ass3

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    
    
    type word = (char * int) list
    type squareFun = word -> int -> int -> int
    
    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"
    
    let a1 = N 42
    let a2 = N 4 .+. (N 5 .-. N 6)
    let a3 = N 4 .*. N 2 .+. N 34
    let a4 = (N 4 .+. N 2) .*. N 34
    let a5 = N 4 .+. (N 2 .*. N 34)
    let rec arithEvalSimple a =
      match a with
      | N x -> x
      | Add(x,z) -> arithEvalSimple(x) + arithEvalSimple(z)
      | Sub(x, z) -> arithEvalSimple(x) - arithEvalSimple(z)
      | Mul(x, z) -> arithEvalSimple(x) * arithEvalSimple(z)
    
    let a6 = V "x"
    let a7 = N 4 .+. (V "y" .-. V "z")
    let a8 = V "y" .+. V "z"
    let rec arithEvalState a s = 
      match a with
      | N x -> x
      | V m -> match Map.tryFind m s with
                        | Some n -> n
                        | None -> 0
      | Add(x, z) -> arithEvalState x s + arithEvalState z s
      | Sub(x, z) -> arithEvalState x s - arithEvalState z s
      | Mul(x, z) -> arithEvalState x s * arithEvalState z s
      | _ -> 0


    let hello : word = [('H', 4); ('e', 1); ('l', 1); ('l', 1); ('o', 1)] // Insert your version of hello here from the last assignment

    let rec arithEval a w s = 
      match a with
      | N x -> x
      | V x -> match Map.tryFind x s with
                        | Some n -> n
                        | None -> 0
      | WL -> List.length w
      | PV x -> match w |> List.tryItem(arithEval x w s) with
                      | Some (_, pv) -> pv
                      | _ -> 0
      | Add(x, z) -> arithEval x w s + arithEval z w s
      | Sub(x, z) -> arithEval x w s - arithEval z w s
      | Mul(x, z) -> arithEval x w s * arithEval z w s
      | _ -> 0

    type cExp =
        | C  of char      (* Character value *)
        | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
        | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
        | CV of aExp      (* Character lookup at word index *)

    let rec charEval c w s = 
      match c with
      | C x -> x
      | ToUpper x -> System.Char.ToUpper(charEval x w s)
      | ToLower x -> System.Char.ToLower(charEval x w s)
      | CV x -> charEval (match w |> List.tryItem(arithEval x w s) with
                                | Some (cc, _) -> C cc
                                | _ -> C ' ') w s

    type bExp =             
        | TT                   (* true *)
        | FF                   (* false *)

        | AEq of aExp * aExp   (* numeric equality *)
        | ALt of aExp * aExp   (* numeric less than *)

        | Not of bExp           (* boolean not *)
        | Conj of bExp * bExp   (* boolean conjunction *)

        | IsDigit of cExp       (* check for digit *)
        | IsLetter of cExp      (* check for letter *)
        | IsVowel of cExp       (* check for vowel *)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)
    let isVowel c = 
      let bigC = System.Char.ToUpper(charEval c [] Map.empty)
      match bigC with
      | 'A' | 'E' | 'I' | 'O' | 'U' -> true
      | _ -> false

    let rec boolEval b w s = 
      match b with
      | TT -> true
      | FF -> false

      | AEq(x, z) -> arithEval x w s = arithEval z w s
      | ALt(x, z) -> arithEval x w s < arithEval z w s

      | Not x -> not (boolEval x w s)
      | Conj(x, z) -> boolEval x w s && boolEval z w s

      | IsDigit x -> System.Char.IsDigit(charEval x w s)
      | IsLetter x -> System.Char.IsLetter(charEval x w s)
      | IsVowel x -> isVowel x

    let isConsonant _ = failwith "not implemented"

    type stmnt =
        | Skip                        (* does nothing *)
        | Ass of string * aExp        (* variable assignment *)
        | Seq of stmnt * stmnt        (* sequential composition *)
        | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
        | While of bExp * stmnt       (* while statement *)

    let evalStmnt _ = failwith "not implemented"

    let stmntToSquareFun (_: stmnt) : squareFun = fun _ _ _ -> 0
    
    let singleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithSingleLetterScore))
    let doubleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithDoubleLetterScore))
    let tripleLetterScore : squareFun = stmntToSquareFun (Ass ("_result_", arithTripleLetterScore))

    let doubleWordScore : squareFun = stmntToSquareFun (Ass ("_result_", arithDoubleWordScore))
    let tripleWordScore : squareFun = stmntToSquareFun (Ass ("_result_", arithTripleWordScore))

    
    let oddConsonants = Skip // Replace this with your version of oddConsonants

    type square2 = (int * stmnt) list
    
    let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
    let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
    let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

    let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
    let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

    let calculatePoints2 _ = failwith "not implemented"