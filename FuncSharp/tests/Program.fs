// For more information see https://aka.ms/fsharp-console-apps
open System
    type CTrie =
        | Leaf of char * string
        | Node of (char * string) * CTrie * CTrie * CTrie
    
    let empty = Leaf (System.Char.MinValue, "")

    let char2num char = (int char - int 'a') + 1 

    let trie2bool trie ch = 
        match trie with
        | Leaf (cha, _) -> if (cha = ch) then true else false
        | Node ((cha, _), _ , _ ,_) when (cha = ch) -> true 
        | Node ((cha, _), _ , _ ,_) when (cha > ch) -> true 
        | Node ((cha, _), _ , _ ,_) when (cha < ch) -> true 

    // let lookup x = 
    //     let word = x
    //     let x = insertRec x
    //     match x with
    //     | Leaf (char, str) when word = str -> Leaf (char, str)
    //     | Node ((char, str), l, m, r) when word = str -> Node((char, str), l, m, r)

    //TODO make root ctrie -> ctrie
    let root = empty
    // string -> (string -> Ctrie -> Ctrie)
    let insert word =
        let rec insertRec x =
            function
            | Node ((ch, str), l,m,r) when x = "" ->
                Node((ch, word), l, m, r)
            
            | Leaf (ch, str) when x = "" ->
                Leaf (ch, word)
            
            // This will change an already existing leaf to a node and continue down the middle to the new leaf
            | Leaf (ch, str)   -> 
                match ch with
                | ch when ch = x.[0] -> Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                | ch when ch = System.Char.MinValue && String.length (word) = 1 -> Leaf (x.[0], word) //Node((ch, str),  empty, insertRec (x.[1..]) empty, empty)
                | _ -> Node((x.[0], str),  empty, insertRec (x.[1..]) empty, empty)

            // This will continue down the left
            | Node ((ch, str), l, m, r) when char2num ch < char2num x.[0]  ->
                match l with 
                | l when l = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                | l when l = empty && String.length(x) > 1 -> Node((ch, str), insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), m, r)
                | l when String.length(x) >= 1 -> Node((ch, str), insertRec (x) l, m, r)

            // This will continue down the right
            | Node ((ch, str), l, m, r) when char2num ch > char2num x.[0]  ->
                match r with 
                | r when r = empty && String.length(x) = 1 -> Node((ch, str), Leaf(x.[0], word), m, r)
                | r when r = empty && String.length(x) > 1 -> Node((ch, str), l, m, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)))
                | r when String.length(x) >= 1 -> Node((ch, str), l, m, insertRec (x) r)

            // if middle child node is empty insert
            | Node ((ch, str), l,m,r) when m = empty -> Node((ch,str), l, insertRec x.[1..] (Leaf(x.[0],"")), r)

            // This will continue down the middle
            | Node ((ch, str), l, m, r) when char2num ch  = char2num x.[0]  ->
                match m, str with
                | m, _ when m = empty && String.length(x) = 1 -> Node((ch, str), l, Leaf(x.[0], word), r)
                | m, _ when m = empty && String.length(x) > 1 -> Node((ch, str), l, insertRec (x.[1..]) (Node((x.[0],""), empty, empty, empty)), r)
                | m, _ when String.length(x) >= 1 -> Node((ch, str), l, insertRec (x.[1..]) m, r)
                | _, str when str = "" && String.length(x) = 0 -> Node((ch, word), l, m, r) 
            
            
            | _ -> insertRec (x) empty
        
        insertRec word root;;
        printfn "2" 
