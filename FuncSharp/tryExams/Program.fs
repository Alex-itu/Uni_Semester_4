// 1.1
    type prop =
    | TT
    | FF
    | And of prop * prop
    | Or of prop * prop

    let p1 = And(TT, FF)
    let p2 = Or(TT, FF)
    let p3 = And(Or(TT, And(TT, FF)), TT)
    let p4 = And(Or(TT,And(TT,FF)), Or(FF,And(TT,FF)))

    let rec eval P =
        match P with
        | TT -> true
        | FF -> false
        | And(p1, p2) -> eval(p1) && eval(p2)
        | Or(p1, p2) -> eval(p1) || eval(p2)


    // 1.2
    // remember, in the assignment you can indicate that (¬P) ∨ (¬Q) calls negate, ie
    // (¬P) is negate(p1), etc..
    let rec negate (P:prop) =
        match P with
        | TT -> FF
        | FF -> TT
        | And(p1, p2) -> Or(negate(p1), negate(p2))
        | Or(p1, p2) -> And(negate(p1), negate(p2))


    // if another function has a recursive call then you don't
    // need to use recursive in, for instance, implies
    // another fact, there is never a reason to call Q recursively,
    // and p4 (in the exam assignment) is always the same.
    // Q doesn't change in this assignment.
    let implies (P:prop) (Q:prop) :prop =
        match P, Q with
        | TT,_ -> Or(negate(P), Q) 
        | FF,_ -> Or(negate(P), Q)
        | And(p1, p2), _ -> Or((negate(P), Q))
        | Or(p1, p2), _ -> Or((negate(P), Q))

    //1.3
    //tail recursive functions
    //The recursive call must be the last expression evaluated in the function body.
    //The function shouldn't perform any additional computations after the recursive call.



    //examples:
    //non-tail recursive
    // let rec map f = function
    // | [] -> []
    // | x::xs -> f x::map f xs
    // use an accumulator variable to make it tail recursive
    // let map f l =
    // let rec loop acc = function
    //     | [] -> List.rev acc
    //     | x::xs -> loop (f x::acc) xs
    // loop [] l

    // for tail recursion, this is how our pattern should go
    // And( And( And( And( And(1 * 5) * 4) * 3) * 2) * 1)

    let forall (f: 'a -> prop) (l: 'a list) : prop =
        let rec loop acc lst =
            match lst with
            | [] -> acc
            //this is not tail recursive, see underneath for explenations
            //| x::xs -> And((loop xs),  f x) 
            //this is tail recursive since it programmed to go through
            //the code from xs, and then read it from right to left
            | x::xs -> loop (And(acc, (f x))) xs
        loop TT l

    //1.4
    //the outcommented code is how you dont do it with higher-order functions
    //notice how you can write it without match cases.
    // let exists f l =
    //     match l with
    //     | [] -> FF
    //     | x::xs -> List.fold (fun el fu -> fu el) f l
    
    // let exists f l =
    //     let loop2 acc lst =
    //         match lst with
    //         | [] -> acc
    //         | x::xs -> List.fold f FF l
    //     loop2 FF l

    let exists (f: 'a -> prop) (l: 'a list) : prop =
        List.fold (fun acc el -> Or(acc, f el)) FF l

    //1.5
    // let existsOne f l =
    //     List.fold (fun acc)


    //2.0
    let rec foo xs ys =
        match xs, ys with
        | _ , [] -> Some xs
        | x :: xs', y :: ys' when x = y -> foo xs' ys'
        | _ , _ -> None

    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys 
        | None -> match xs with
                    | [] -> []
                    | x :: xs' -> x :: (bar xs' ys)
    

    let baz (a : string) (b : string) =
        bar [for c in a -> c] [for c in b -> c] |>
        List.fold (fun acc c -> acc + string c) ""

    //2.1
    //What are the types of functions foo , bar , and baz ?
    
    // foo has type 'a list option -> 'a list option -> 'a list option
    // bar has type 'a list option -> 'a list option -> 'a list option
    // baz has type string -> string -> string

    //What do functions foo , bar , and baz do? Focus on what they do rather than how they do it
    //foo checks for an identical prefix in the first list, xs, which is identical to the beginning of the second list, ys.
    
    //bar strips a prefix recursively, If foo returns None (no match found for the current potential prefix):
    // If the first list (xs) is empty ([]), it means no prefix was found, so bar returns an empty list ([]).
    // Otherwise, bar tries a different approach. It prepends the first element of xs to the result of calling itself recursively with the tail of xs and the original second list (ys). This essentially removes the first element from xs and checks if the remaining list is a prefix. This process continues recursively until either a match is found or xs becomes empty.
    
    // baz takes two words and tells you the longest string that starts both of them,
    // so it shows the letters which are in the two lists, if they are the same letters in both strings, then "" is shown, if we do baz "something" "someth";; it would show "ing", since the rest is identical and should not be shown

    //What would be appropriate names for functions foo , bar , and baz ?
    //foo = removeSelectedPrefix
    //bar = stripSelectedPrefix
    //baz = removeSubstrings

    //2.2
    //hard to understand the question, but if i understand it correctly:
    // What do A B and C do
    // A: [for c in a -> c]
    // A shows a foreach loop which traverses a string, a,
    // and takes the chars from it and inserts it into a list

    // B: [for c in b -> c]
    // B does the same as A, where it takes the chars from a string and inserts them into a list
    
    // C: List.fold (fun acc c -> acc + string c) ""
    // C uses List.Fold to insert the chars into a string with an accumulator

    // explain the use of the |> -operator in the baz function.
    // the piping operator takes the list of chars and uses List.fold to insert them into a string


    //2.3 No recursion
    //when getting a question like this, where we're supposed to create a non-recursive
    //function by using a high order function, like List.map, List.fold, List,splitAt, etc...
    //
    // let rec foo xs ys =
    //     match xs, ys with
    //     | _ , [] -> Some xs
    //     | x :: xs', y :: ys' when x = y -> foo xs' ys'
    //     | _ , _ -> None

    //more examples than this one
    let foo2 xs ys =
        if List.length ys > List.length xs then
            None
        else
            let front, back = List.splitAt (List.length ys) xs
            if front = ys then
                Some back
            else
                None

    //2.4 Tail recursion
    //The function bar is not tail recursive. Demonstrate why
        
        // let rec bar xs ys =
        // match foo xs ys with
        // | Some zs -> bar zs ys 
        // | None -> match xs with
        //             | [] -> []
        //             | x :: xs' -> x :: (bar xs' ys)

    // we'll take two lists and go through them as evaluation
    // bar ['s';'o';'m';'e';] ['h';'i';];;
    // the symbol ==> is, in this case, "leads to"
    // bar ['s'; 'o'; 'm'; 'e'] ['h'; 'i']

    // Since foo ['s'; 'o'; 'm'; 'e'] ['h'; 'i']
    // will return None because the lists do not start with the same element:

    // (1) ==> 's' :: bar ['o'; 'm'; 'e'] ['h'; 'i']

    // Again, foo ['o'; 'm'; 'e'] ['h'; 'i'] returns None:
    // (2) ==> 's' :: ('o' :: bar ['m'; 'e'] ['h'; 'i'])

    // The pattern continues since foo will keep returning None:
    // (3) ==> 's' :: ('o' :: ('m' :: bar ['e'] ['h'; 'i']))

    // (4) ==> 's' :: ('o' :: ('m' :: ('e' :: bar [] ['h'; 'i'])))

    // Now, xs is an empty list, so the None case of bar matches the empty list:
    // (5) ==> 's' :: ('o' :: ('m' :: ('e' :: [])))

    // This begins the process of constructing the final list, unwinding the recursive calls:
    // (6) ==> 's' :: ('o' :: ('m' :: ['e']))

    // (7) ==> 's' :: ('o' :: ['m'; 'e'])

    // (8) ==> 's' :: ['o'; 'm'; 'e']
    // (9) ==> ['s'; 'o'; 'm'; 'e']

    //the evaluation shows how its not tail recursive since the append happens on the stack (double check this)
    //(jespers answer: the list keeps growing and added to the list, and cannot stop until it collapses into the list. Therefore its not tail recursive
    //2.5 continuations
    
    //Create a tail-recursive version, barTail using continuations (not accumulators) that behaves
    //exactly the same way as bar for all possible inputs.
        // let rec bar xs ys =
        // match foo xs ys with
        // | Some zs -> bar zs ys 
        // | None -> match xs with
        //             | [] -> []
        //             | x :: xs' -> x :: (bar xs' ys)
    //hints for this one, look at the examples in our notes
    let bartail xs ys =
        let rec looper xs ys c  =
            match foo xs ys with
            | Some zs -> looper zs ys c
            | None -> match xs with
                        | [] -> c [] 
                        | x :: xs' -> looper xs' ys (fun r -> c (x::r))
        looper xs ys id

    //3.1 collatz sequences
    
    let collatz x =
        let rec collatzSeq (x: int) (lst: int list) = 
            match x with
            | x when x = 1 -> lst @ [x]
            //if even
            | x when (x % 2) = 0 -> [x] @ collatzSeq (x/2) lst
            //if odd
            | x when (x % 2) = 1 -> [x] @ collatzSeq (3*x + 1) lst
            //if negative
            | x when x < 0 -> failwith ("Non positive number: " + string x)
        collatzSeq x []

    //3.2: Even and odd Collatz sequence elements
    let evenOddCollatz x =
        let rec evOdd lst coord = // in the tuble: first is even, second is odd
            match lst with
            | [] -> coord
            //even
            | x::xs when (x % 2) = 0 -> evOdd xs ((fst coord) + 1, snd coord)
            //odd
            | x::xs when (x % 2) = 1 -> evOdd xs (fst coord, (snd coord) + 1)
        
        evOdd (collatz x) (0, 0)

    //3.3: Maximum length Collatz Sequence
    //note to self, make it simple. Use all examples instead of jumping to the last one.
    let maxCollatz x y =
        let rec findCollatz min maxi lst   =
            match min, maxi with
            | _,_ when min > maxi -> lst
            | _,_ -> ([collatz min]) @ (findCollatz (min+1) maxi lst) 
        let collatzLists = findCollatz x y []
        
        let rec findMax lst currentMax  = // lst is our list of lists of collatz sequences, so we make another list which is called currentMax
            match lst with
            | [] -> match currentMax with
                    | x::_ -> (x, (List.length currentMax))
                    | _ -> failwith ("something" + string collatzLists)

            | x::xs when (List.length x) > (List.length currentMax) -> findMax xs x
            | x::xs -> findMax xs currentMax
        findMax collatzLists []
    
    //another way of doing it, by using a tuple
    // let maxCollatz2 x y =
    //     let rec findCollatz min maxi lst =
    //         match min with
    //         | _ when min > maxi -> lst
    //         | _ -> ([collatz min]) @ (findCollatz (min+1) maxi lst) 
    //     let collatzLists = findCollatz x y []
        
    //     let rec findMax lst (maxNum, maxLength) =
    //         match lst with
    //         | [] -> (maxNum, maxLength)
    //         | x::xs ->
    //             let currentSeqLength = List.length x
    //             if currentSeqLength > maxLength then
    //                 findMax xs (List.head x, currentSeqLength)
    //             else
    //                 findMax xs (maxNum, maxLength)
    //     findMax collatzLists (x, List.length (collatz x))

    //another way of doing it with if and else statements
    // let maxCollatz x y =
    //     let rec findMaxCollatz (currentMaxNumber, currentMaxLength) currentNumber =
    //         if currentNumber > y then
    //             (currentMaxNumber, currentMaxLength)
    //         else
    //             let currentSeq = collatz currentNumber
    //             let currentSeqLength = List.length currentSeq
    //             if currentSeqLength > currentMaxLength then
    //                 findMaxCollatz (currentNumber, currentSeqLength) (currentNumber + 1)
    //             else
    //                 findMaxCollatz (currentMaxNumber, currentMaxLength) (currentNumber + 1)
    //     findMaxCollatz (x, List.length (collatz x)) x
    
    //3.4
    let collect x y =
        let rec findCollatz min maxi lst   =
            match min, maxi with
            | _,_ when min > maxi -> lst
            | _,_ -> ([collatz min]) @ (findCollatz (min+1) maxi lst) 
        let collatzLists = findCollatz x y []

        let rec mapCollatz lst (mp:Map<int,Set<int>>) =
            match lst with
            | [] -> mp
            //use map.tryFind instead of set.ofList ... etc
            | x::xs -> mp.Add((List.length x), (Set.ofList x)) |> mapCollatz xs

        mapCollatz collatzLists Map.empty

