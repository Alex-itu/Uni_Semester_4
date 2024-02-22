module MultiSet

    type MultiSet<'a when 'a : comparison> = R of unit // replace with your type

    let empty = R ()

    let isEmpty (_ : MultiSet<'a>) = true

    let size (R ()) = 0u
    
    let contains (_ : 'a) (_ : MultiSet<'a>) = true

    let numItems (_ : 'a) (_ : MultiSet<'a>) = 0u

    let add (_ : 'a) (_ : uint32) (_ : MultiSet<'a>) : MultiSet<'a> = R ()

    let addSingle (_ : 'a) (_ : MultiSet<'a>) : MultiSet<'a> = R ()
    
    let remove (_ : 'a) (_ : uint32) (_  : MultiSet<'a>) : MultiSet<'a> = R ()

    let removeSingle (_ : 'a) (_ : MultiSet<'a>) : MultiSet<'a>= R ()


    let fold (_ : 'b -> 'a -> uint32 -> 'b) (x : 'b) (_ : MultiSet<'a>) = x 
    let foldBack (_ : 'a -> uint32 -> 'b -> 'b) (_ : MultiSet<'a>) (x : 'b) = x
    
    let ofList (_ : 'a list) : MultiSet<'a> = R ()
    let toList (_ : MultiSet<'a>) : 'a list = []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = R ()

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R ()
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R ()
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R ()
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = R ()
       
    