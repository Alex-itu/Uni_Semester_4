module ImpParser

    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    // seems like this just need to be a pstring og the let's name - the "p"
    // if so, this would just take a string and parse to check if a given string is the same as the pstring string
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    // reminder: satisfy takes a function
    // doblue reminder: the taken function has to be char -> bool
    // seems like these is a checker for some things, since there is a bool in it
    // they dont need an argument in them, it gives errors
    // gives a function that take a function (or more) and gives back a parser of char
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace" 
    let pletter     = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    // has to use many, since I want a list of the char in the parser and ...
    // gives back a parser of list of a 'a, which i want to be a char here
    let spaces         = many whitespaceChar  <?> "space"
    let spaces1        = many1 whitespaceChar  <?> "space1"

    let (.>*>.) a b = ((a .>> spaces) .>>. b)
    let (.>*>) a b  = (a .>>  (spaces >>. b))
    let (>*>.) a b  = ((a .>>  spaces) >>. b)

    // use >*>. and .>*> to discard the parentheses from the result.
    // Hint: It may be beneficial to create a similar function that uses { and } in stead of parentheses.
    // Such a function will come in handy when we parse statements like while-loops or if-statements.
    // use pchar to take away parent....
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let tobor p = pchar '{' >*>. p .>*> pchar '}' 
    
    // neeed to check with <|> if is a letter or have "_" in it
    // many has to be used, it can handel the "arbitrary number or letter..." part
    // reminder still check if it start with letter or underscore else error, see example 3 with 1x
    // while letter is fine at the start, i need to use palphanumeric later, check both number or letter
    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> (fun (v, vs) ->  System.String.Concat (v :: vs))


    // think i get this, note: read explaination for assignment
    let unop op a = op >*>. a
    
    // same here, note: read explaination for assignment
    // of course the op was suppose to be in the middel.... thanks brain
    let binop op a b = a .>*> op .>*>. b

    // 7.8
    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let subParse= binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; subParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let divParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let modParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; divParse; modParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let neParse = unop (pchar '-') AtomParse |>> (fun x -> Mul ((N -1), x)) <?> "Neg"
    let pointParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let VParse = pid |>> V <?> "V"
    
    

    let AexpParse = TermParse   


    let CParse, cref = createParserForwardedToRef<cExp>()

    // for AexpParse's aref since this is something that was made good............... 
    let charToInt = unop (pCharToInt) (parenthesise CParse) |>> CharToInt <?> "CharToInt" 
    // note: the placement of choices are vaild for understanding
    // last note means two things now......
    do aref := choice [charToInt; neParse; pointParse; VParse; NParse; ParParse]

    // found out it was already made
    // use between instead
    //let getChar p = (pchar ''')  >*>. p .>*> (pchar ''')
    let getChar p = between (pchar ''') (pchar ''') p 
    let charC = getChar (palphanumeric <|> whitespaceChar) |>> C <?> "C"
    let toLowerC = unop (pToLower) (parenthesise CParse) |>> ToLower <?> "ToLower"
    let toUpperC = unop (pToUpper) (parenthesise CParse) |>> ToUpper <?> "ToUpper"
    let valueC = unop (pCharValue) (parenthesise AexpParse) |>> CV <?> "CV"
    let intToCharC = unop (pIntToChar) (parenthesise AexpParse) |>> IntToChar <?> "IntToChar"
    
    do cref := choice [charC; toLowerC; toUpperC; valueC; intToCharC;]
    
    
    let CexpParse = CParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

