
module Assignment2
    // let downto1Extra n = 
    //     if n > 0 then
    //         [n.. -1 ..1]
    //     else
    //         []
    let rec downto1 n =
        if n > 0 then
            n :: downto1 (n-1)
        else
         []
            
    let rec downto2 n = 
        match n with
        | n when n = 0 -> []
        | n when n < 0 -> []
        | n when n > 0 -> n :: downto2 (n-1)


    let rec removeOddIdx (xs : 'a list) = 
        match xs with
        | [] -> []
        | [h] -> [h]
        | h :: h2 :: t ->  h :: removeOddIdx t

    // let removeOddIdxExtra (xs : 'a list) = 
    //     [for i in 0.. (xs.Length-1) do
    //         if i % 2 = 0 then xs.Item(i)]

    let rec combinePair (xs : 'a list) = 
        match xs with
        | [] -> []
        | [a] -> []
        | h :: h2 :: t -> (h, h2) :: combinePair t

    // don't think this is right, but can also with this
    // type complex = Complex of float * float
    // Complex (x, z)
    type complex = { x: float; z: float }
    let mkComplex pairX pairZ = {x = pairX; z = pairZ}
    let complexToPair plex  = (plex.x, plex.z)
    let (|+|) plex1 plex2 = mkComplex (plex1.x + plex2.x) (plex1.z + plex2.z) 
    let (|*|) plex1 plex2 = 
        mkComplex ((plex1.x * plex2.x) - (plex1.z * plex2.z)) ((plex1.z * plex2.x) + (plex1.x * plex2.z)) 
    let (|-|) plex1 plex2 = plex1 |+| mkComplex (-plex2.x) (-plex2.z) 
    let (|/|) plex1 plex2 = 
        mkComplex ((plex1.x * plex2.x) + (plex1.z * plex2.z) / ((plex2.x * plex2.x) + (plex2.z * plex2.z))) (((plex1.z * plex2.x) - (plex1.x * plex2.z)) / ((plex2.x * plex2.x) + (plex2.z * plex2.z)))

    let explode1 (s : string) = s.ToCharArray() |> List.ofArray 

    let rec explode2 s = 
        match s with
        | "" -> []
        | _ -> s[0] :: explode2 s[1..(s.Length-1)]

    let implode (cs : char List) = List.foldBack (fun c1 c2 -> string c1 + c2) cs ""
    let implodeRev (cs : char List) = List.fold (fun c1 c2 ->  string c2 + c1) "" cs

    let toUpper = explode1 >> List.map (fun s -> System.Char.ToUpper(s)) >> implode

    let rec ack ((m : int), (n : int)) = 
        match (m, n) with
        | m, n when m = 0 -> n + 1
        | m, n when m > 0 && n = 0 -> ack((m-1), 1)
        | m, n when m > 0 && n > 0 -> ack(m-1, ack(m,(n-1)))
        | m, n when m < 0 || n < 0 ->  failwith "not implemented"



    let downto3 _ = failwith "not implemented"

    let fac _ = failwith "not implemented"
    let range _ = failwith "not implemented"

    type word = (char * int) list


    let hello = [] // Fill in your answer here

    type squareFun = (char * int) list -> int -> int -> int

    let singleLetterScore _ = failwith "not implemented"
    let doubleLetterScore _ = failwith "not implemented"
    let tripleLetterScore _ = failwith "not implemented"

    let doubleWordScore _ = failwith "not implemented"
    let tripleWordScore _ = failwith "not implemented"

    type square = (int * squareFun) list


    let oddConsonants _ = failwith "not implemented"
    
    

    let SLS : square = [(0, singleLetterScore)]
    let DLS : square = [(0, doubleLetterScore)]
    let TLS : square = [(0, tripleLetterScore)]

    let DWS : square = SLS @ [(1, doubleWordScore)]
    let TWS : square = SLS @ [(1, tripleWordScore)]

    let calculatePoints _ = failwith "not implemented"