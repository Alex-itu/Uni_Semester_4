module StateMonad

    // just a type with different things in it, so it can be all those things
    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    // used for stateful computations, means give thing, do something with it, then give a result and new updated thing back
    // A type used for indacating what a given result is.
    // So if something CAN be done, make sure it of Success, to say it's good.
    // And Give something Failure if it CAN'T do it, like x/0, not gonna happend.
    // Maybe: Can prob give a Failure out of Error type, so they work together.
    // Type_result_example_for_understanding.png for more
    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    // a type with 3 thing in it,
    // vars is a list of maps, that is of string keys and int values
    // word is a list tuples, that is of a char and then a int. Like this: [('h', 4); ('a', 1)]
    // reserved is a set of string
    // both vars and reserved need a list to be maded, with .ofList that both set and map have
    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    // Is a State Monad

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    // this push is a stateful computation
    // unit is used for a lack of information
    // Maybe: the () seems to because of unit, as that also that as a value
    // () is the return, we return nothing since this push
    // side note: this pushes to the tail
    // my take: take a state with a new element in it and then updates the state.
    // real take: updates the variable state by pushing an empty map to the top of the stack.
    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    //green 6.1
    // seems like this needs to pop from the tail, so when making it, use Tail and not head.
    // takes a state and with it take the tail, which mean all but the first element gets in the new state
    let pop : SM<unit> = S (fun s -> Success ((), {s with vars = s.vars.Tail}))
    
    //green 6.2
    let wordLength : SM<int> = S (fun s -> Success (s.word.Length, s))      

    //green 6.3
    let characterValue (pos : int) : SM<char> = 
        S (fun s -> 
        match pos with
        | x when pos >= s.word.Length  -> Failure (IndexOutOfBounds x)
        | x -> Success ((
            match List.tryItem x s.word with
            | Some (c, _)  -> c
            | None -> 'õ'
            ), s)
        )      

    //green 6.4
    let pointValue (pos : int) : SM<int> = 
        S (fun s -> 
        match pos with
        | x when pos >= s.word.Length  -> Failure (IndexOutOfBounds x)
        | x -> Success ((
            match List.tryItem x s.word with
            | Some (_, v)  -> v 
            | None -> 999999
            ), s)
        )         

    //yellow 6.5
    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    //yellow 6.6
    let declare (var : string) : SM<unit> = failwith "Not implemented"   
    let update (var : string) (value : int) : SM<unit> = failwith "Not implemented"      
              

    