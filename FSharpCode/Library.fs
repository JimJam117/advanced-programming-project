namespace FSharpCodeLib

// Interpreter in F#
// Group: Group 9, James Sparrow and Seb Taylor
// Author of original interpreter: R.J. Lapeer 
// Date: 06/01/2024
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report
open System

module lang =

    // exception functions
    let lexError = System.Exception("Lexer error")
    let parseError = System.Exception("Parser error")
    let divisionByZeroError = System.Exception("Division by zero error!")
    let denomZeroError = System.Exception("Denominator of a rational cannot be zero!")
    let symError = System.Exception("No value associated to variable name")
    let linearError = System.Exception("The coefficient cannot be 0.")

    // reducing rationals helper funcs
    let rec gcd a b =
        if b = 0 then a
        else gcd b (a % b)
    let lcm a b =
        abs (a * b) / gcd a b

    // find next largest common multiple intger. Used for converting floats to rats
    let findNextLcmInteger f =
        let rec loop n =
            let x = f * n
            let xw = float (int x) // convert to int to remove decimal, then back to float
            if x = xw then
                int x, int n
            else
                loop (n + 1.)
        loop 1.

    // reduce to simplest form
    let reduceRational (n,d) =
        if d = 0 then
            raise denomZeroError
        else
            let common = gcd n d
            let cn = n / common
            let cd = d / common
            if cd < 0 then 
                let cd = abs cd
                let cn = cn * -1
                (cn, cd)
            else
                (cn, cd)

    // root finding functions
    let rtfunc x a nth =  float x ** float nth - float a
    let rtfunc' x nth =  nth * float x ** float (nth - 1.)
    let nthRoot (b) (nth) =
        let threashold = 0.0000000001  // value we can use to determine if we are close enough
        let rec findRoot i =
            let xn = i - (rtfunc i b nth) / (rtfunc' i nth)
            let min = abs (xn - i)
            if Math.Round(min,8) < threashold then xn
            else findRoot xn
        findRoot 1.

    let rec float2Rat f d =
        let n = int(f * float(d))
        let n2, d2 = reduceRational (n, d)

        if d < 999999 then
            float2Rat f (d + 1)
        else
            (n2, d2)

    // Number types, with operators overloaded
    type NumberType =
        | INTEGER of int
        | FLOAT of float
        | RATIONAL of int*int
        with
        
           static member getIntValue (f1:NumberType) =
                    match f1 with
                    | INTEGER x -> int x
                    | FLOAT x -> int x
                    | RATIONAL (x,y) -> int x / int y

           static member getFloatValue (f1:NumberType) =
                    match f1 with
                    | INTEGER x -> float x
                    | FLOAT x -> float x
                    | RATIONAL (x,y) -> float x / float y

           static member getRatValue (f1:NumberType) =
                    match f1 with
                    | INTEGER x -> (x, 1)
                    | FLOAT x -> (float2Rat x 2)
                    | RATIONAL (x,y) -> (x, y)

            static member Pow (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (int (float x ** y))
                    | FLOAT x, FLOAT y -> FLOAT (x ** y)
                    | INTEGER x, FLOAT y -> FLOAT (float x ** y)
                    | FLOAT x, INTEGER y -> FLOAT (x ** float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> RATIONAL (reduceRational(int(float n1 ** float n2), int(float d1 ** float (n2*d2))))

                    | RATIONAL (n1,d1), INTEGER i -> RATIONAL (reduceRational(int(float n1 ** float i), int(float d1 ** float i)))
                    | RATIONAL (n1,d1), FLOAT i -> RATIONAL (reduceRational(int(float n1 ** i), int(float d1 ** i)))
                
                    | INTEGER i, RATIONAL (n1,d1) -> 
                        let temp = float i ** float n1
                        let nrt = nthRoot temp d1
                        FLOAT nrt
                
                    | FLOAT i, RATIONAL (n1,d1) ->
                        let temp = float i ** float n1
                        let nrt = nthRoot temp d1
                        FLOAT nrt

            static member (+) (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (x + y)
                    | FLOAT x, FLOAT y -> FLOAT (x + y)
                    | INTEGER x, FLOAT y -> FLOAT (float x + y)
                    | FLOAT x, INTEGER y -> FLOAT (x + float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> RATIONAL (reduceRational (n1 * d2 + d1 * n2  ,d1*d2))

                    | RATIONAL (n1,d1), INTEGER i -> RATIONAL (reduceRational(n1 + (i*d1), d1))
                    | INTEGER i, RATIONAL (n1,d1) -> RATIONAL (reduceRational(n1 + (i*d1), d1))


                    | RATIONAL (n1,d1), FLOAT i -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1 * dx + d1 * nx  ,d1*dx))

                    | FLOAT i, RATIONAL (n1,d1) -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1 * dx + d1 * nx  ,d1*dx))


            static member (-) (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (x - y)
                    | FLOAT x, FLOAT y -> FLOAT (x - y)
                    | INTEGER x, FLOAT y -> FLOAT (float x - y)
                    | FLOAT x, INTEGER y -> FLOAT (x - float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> RATIONAL (reduceRational(n1 * d2 - d1 * n2  ,d1*d2))
                
                    | RATIONAL (n1,d1), INTEGER i -> RATIONAL (reduceRational(n1 - i * d1, d1))
                    | INTEGER i, RATIONAL (n1,d1) -> RATIONAL (reduceRational(i * d1 - n1, d1))

                    | RATIONAL (n1,d1), FLOAT i -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1 * dx - d1 * nx  ,d1*dx))
                    | FLOAT i, RATIONAL (n1,d1) ->
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(d1 * nx - n1 * dx ,d1*dx))


            static member (*) (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (x * y)
                    | FLOAT x, FLOAT y -> FLOAT (x * y)
                    | INTEGER x, FLOAT y -> FLOAT (float x * y)
                    | FLOAT x, INTEGER y -> FLOAT (x * float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> RATIONAL (reduceRational(n1 * n2  ,d1 * d2))

                    | RATIONAL (n1,d1), INTEGER i -> RATIONAL (reduceRational(n1 * i, d1))
                    | INTEGER i, RATIONAL (n1,d1) -> RATIONAL (reduceRational(n1 * i, d1))

                    | RATIONAL (n1,d1), FLOAT i -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1 * nx, d1 * dx))
                    | FLOAT i, RATIONAL (n1,d1) ->
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1 * nx, d1 * dx))

            static member (/) (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (x / y)
                    | FLOAT x, FLOAT y -> FLOAT (x / y)
                    | INTEGER x, FLOAT y -> FLOAT (float x / y)
                    | FLOAT x, INTEGER y -> FLOAT (x / float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> RATIONAL (reduceRational(n1 *d2,d1*n2))

                    | INTEGER i, RATIONAL (n2,d2) -> RATIONAL (reduceRational(n2, d2*i))
                    | RATIONAL (n1,d1), INTEGER i -> RATIONAL (reduceRational(n1,d1*i))
                    | FLOAT i, RATIONAL (n2,d2) -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(nx*d2,n2*dx))
                    | RATIONAL (n1,d1), FLOAT i -> 
                        let nx, dx = findNextLcmInteger i
                        RATIONAL (reduceRational(n1*dx,d1*nx))

            static member (%) (f1:NumberType, f2:NumberType) =
                    match f1, f2 with
                    | INTEGER x, INTEGER y -> INTEGER (x % y)
                    | FLOAT x, FLOAT y -> FLOAT (x % y)
                    | INTEGER x, FLOAT y -> FLOAT (float x % y)
                    | FLOAT x, INTEGER y -> FLOAT (x % float y)

                    | RATIONAL (n1,d1), RATIONAL (n2,d2) -> INTEGER 0
                        //let nq = (n1 * d2)
                        //let dq = (d1 * n2)
                        //RATIONAL (reduceRational(nq * n2, dq * d2)) // WIP

                    /// WIP
                    | INTEGER i, RATIONAL (n2,d2) -> INTEGER 0
                    | FLOAT i, RATIONAL (n2,d2) -> INTEGER 0
                    | RATIONAL (n1,d1), INTEGER i -> INTEGER 0
                    | RATIONAL (n1,d1), FLOAT i -> INTEGER 0

            static member (~-) (f1:NumberType) =
                    match f1 with
                    | INTEGER x -> INTEGER (-x)
                    | FLOAT x -> FLOAT (-x)
                    | RATIONAL (n,d) -> RATIONAL ((-n),d) // negate the numerator


    // list of token types - this is a I believe a discriminated union
    type terminal = 
        Add | Sub | Mul | Div | Pow | Mod | Lpar | Rpar | Equ | FloatNum of NumberType | Vid of string | IntNum of NumberType | RatNum of NumberType


    let str2lst s = [for c in s -> c]           // simple function to turn a string into a list of chars
    let isblank c = System.Char.IsWhiteSpace c  // simple function to check if char c is whitespace
    let isdigit c = System.Char.IsDigit c       // simple function to check if char c is a digit
    let isdot c =  c = '.'                      // simple function to check if char c is a dot
    let isratdash c = c = '\\'                  // indicator of a rational number
    let isletter c = System.Char.IsLetter c       // simple function to check if char c is a letter
    let intVal (c:char) = (int)((int)c - (int)'0') // convert char c into int
    let floatVal (c:char) = (float)((float)c - (float)'0') // convert char c into int



    // Checks if the first number in a string list is a float
    let isNextFloat(iStr) = 
        let rec recursiveloop(iStr) = 
            match iStr with
             | c :: tail when isdot c -> (true)
             | c :: tail when isdigit c -> recursiveloop(tail)
             | _ -> (false)

        recursiveloop(iStr)

    // same as above for rationals
    let isNextRat(iStr) = 
        let rec recursiveloop(iStr) = 
            match iStr with
             | c :: tail when isratdash c -> (true)
             | c :: tail when isdigit c -> recursiveloop(tail)
             | _ -> (false)

        recursiveloop(iStr)

    // function to get the value of a string number as in integer
    // for example, string '123' would be converted to int 123
    let rec scInt(iStr, iVal) = 
        match iStr with
        | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal) // return tuple of string and value



    // The same as the above, but instead of for converting a string into an integer, we convert
    // a string into a variable name
    let rec scChar(iStr, vName:string) =
        match iStr with
        | c :: tail when isletter c -> scChar(tail,(vName + c.ToString()))
        | _ -> (iStr, vName)


    // same as above but for floats. Slightly more complicated, as we need to check if we are passed the dot
    let rec scFloat(iStr, iVal, pastDotCount) = 
        match iStr with
            | c :: tail when isdigit c && pastDotCount <> 0 -> 
                                        scFloat(tail, float iVal+(floatVal c / (float 10 ** pastDotCount)), pastDotCount + 1)

            | c :: tail when isdot c -> scFloat(tail, float (iVal), 1)
            | c :: tail when isdigit c -> scFloat(tail, float 10*iVal+(floatVal c), 0)
            | _ -> (iStr, iVal) // return tuple of string and value

    // scan rationals, same as above but again slightly more complicated
    let rec scRational(iStr, iVal, firstDigit, pastDash, (iValn, iVald)) = 
        match firstDigit with
        | true -> match pastDash with
                    | true -> scRational(iStr, iVal, false, pastDash, (iValn, iVal))
                    | false -> scRational(iStr, iVal, false, pastDash, (iVal, iVald))
        | false -> match iStr with
                    | c :: tail when isdigit c && pastDash = false -> 
                                                scRational(tail, iVal, false, pastDash, (10*iValn+(intVal c), iVald))

                    | c :: tail when isratdash c -> scRational(tail, 0, true, true, (iValn, iVald))
                    | c :: tail when isdigit c && pastDash = true -> scRational(tail, iVal, false, pastDash, (iValn, 10*iVald+(intVal c)))
                    | _ -> (iStr, reduceRational (iValn,iVald) )// return tuple of string and value

    // Next we have our lexer function. It is made up of a recursive sub-function called scan.
    let lexer input = 
        let rec scan input =
            // 
            match input with
            | [] -> []
            | '+'::tail -> Add :: scan tail 
            | '-'::tail -> Sub :: scan tail
            | '*'::tail -> Mul :: scan tail
            | '/'::tail -> Div :: scan tail
            | '%'::tail -> Mod :: scan tail
            | '^'::tail -> Pow :: scan tail
            | '('::tail -> Lpar:: scan tail
            | ')'::tail -> Rpar:: scan tail
            | '='::tail -> Equ:: scan tail  // UPDATE
            | c :: tail when isblank c -> scan tail // in case where c is blank, just scan tail

            // if c is a digit, we define tuple (iStr, iVal) and assign it to output
            // of scInt function. We then return on the next line the value ival as a Num,
            // (remember Num is of int so it needs an int in it's constructor) then scan istr,
            // which is tail with the characters that make up ival removed
            | c :: tail when isdigit c -> 
                                        match isNextFloat (c :: tail) with
                                        | true -> let (iStr, iVal)  = scFloat(tail, intVal c, 0) 
                                                  FloatNum (FLOAT iVal) :: scan iStr
                                        | false -> match isNextRat (c :: tail) with
                                                    | true -> let (iStr, (iVal:int*int))  = scRational(tail, intVal c, true, false, (0, 0))
                                                              RatNum (RATIONAL iVal) :: scan iStr
                                                    | false -> let (iStr, iVal)  = scInt(tail, intVal c) 
                                                               IntNum (INTEGER iVal) :: scan iStr

               
            // same as above but for variable names
            | c :: tail when isletter c -> let (iStr, vName) = scChar(tail, c.ToString()) // UPDATE
                                           Vid vName :: scan iStr
            | _ -> raise lexError
    
        scan (str2lst input) // run the sub-function with input string as list


    // simple readline function
    let getInputString() : string = 
        Console.Write("Enter an expression: ")
        Console.ReadLine()

    // Grammar in (E)BNF:
    // <VA>       ::= <varID> "=" <E>

    // <E>        ::= <T> <Eopt>
    // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>

    // <T>        ::= <P> <Topt>
    // <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "(" <E> ")" | <varID> | <empty>

    // <P>        ::= <NR> <Popt>
    // <Popt>     ::= "^" <NR> <Popt> | <empty>

    // <NR>   	::= "-"["IntNum" | "FloatNum" | "RatNum" | "varVal" ] <value> | 
    //              ["IntNum" | "FloatNum" | "RatNum" | "varVal" ] <value> | "(" <E> ")" | "-(" <E> ")"

    // <varID>    ::= [a-z,A-Z]+     (* varVal is fetched from symbol table/list with key varID *)


    // tList is a list of tokens passed into the parser. 
    let parser tList = 
        // we define the recrusive function for E layer here.
        // this >> operator is the composition operator, kinda like piping
        let rec E tList = (T >> Eopt) tList 
        and Eopt tList = 
            match tList with
            | Add :: tail -> (T >> Eopt) tail
            | Sub :: tail -> (T >> Eopt) tail
            | _ -> tList

        // same as above for T, with NR being the input
        and T tList = (P >> Topt) tList
        and Topt tList =
            match tList with
            | Mul :: tail -> (P >> Topt) tail
            | Div :: tail -> (P >> Topt) tail
            | Mod :: tail -> (P >> Topt) tail
            | Vid vName :: tail -> tail
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | _ -> tList

        and P tList = (NR >> Popt) tList
        and Popt tList =
            match tList with
            | Pow :: tail -> (NR >> Popt) tail
            | _ -> tList

        // NR simply matches either a number or variable name
        // it also can match brackets
        and NR tList =
            match tList with 
            | IntNum value :: tail -> tail
            | Sub :: IntNum value :: tail -> tail
            | FloatNum value :: tail -> tail
            | Sub :: FloatNum value :: tail -> tail
            | RatNum value :: tail -> tail
            | Sub :: RatNum value :: tail -> tail
            | Vid vName :: tail -> tail
            | Sub :: Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | _ -> raise parseError


        // final sub-function, think VA stands for variable assignment.
        // VA takes a token list and matches 
        let VA tList =  // UPDATE
            match tList with
            | Vid vName :: tail -> match tail with
                                   | Equ :: tail -> E tail // inner tail (from this line)
                                   | _ -> E tList  // Need tList to keep Vid vName
            | _ -> E tList

        VA tList  // CHANGED FROM E tList
    
    let rec searchVName vName (symList:List<string*NumberType>) =
        match symList with
        | head :: tail -> if (fst head) = vName then (true, (snd head))
                          else searchVName vName tail
        | _ -> (false, INTEGER 0)

    // evaluation
    let parseNeval tList convertToFloat (symList:List<string*NumberType>) = // UPDATED

        // E section
        let rec E tList = (T >> Eopt) tList
        and Eopt (tList, (vID, value)) = 
            match tList with
            | Add :: tail -> let (tLst, (vID, tval)) = T tail
                             Eopt (tLst, (vID, value + tval))
            | Sub :: tail -> let (tLst, (vID, tval)) = T tail
                             Eopt (tLst, (vID, value - tval))
            | _ -> (tList, ("", value))

        // T section
        and T tList = (P >> Topt) tList
        and Topt (tList, (vID, value)) =
            match tList with
            | Mul :: tail -> let (tLst, (vID, tval)) = P tail
                             Topt (tLst, (vID, value * tval))
            | Div :: tail -> let (tLst, (vID, tval)) = P tail
                             if (tval <> tval * INTEGER 0) then Topt (tLst, (vID, value / tval))
                             else raise divisionByZeroError
            | Mod :: tail -> let (tLst, (vID, tval)) = P tail
                             if (tval <> tval * INTEGER 0) then Topt (tLst, (vID, value % tval))
                             else raise divisionByZeroError
            | Vid vName :: tail -> let res = searchVName vName symList
                                   if (fst res) then (tail, (vID, value * (snd res)))
                                   else raise symError
            | Lpar :: tail -> let (tLst, (vID, tval)) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail,  (vID, value * tval))
                              | _ -> raise parseError
            | _ -> (tList, ("", value))


        // P section
        and P tList = (NR >> Popt) tList
        and Popt (tList, (vID, value)) =
            match tList with
            | Pow :: tail -> let (tLst, (vID, tval)) = NR tail
                             Popt (tLst, (vID, value ** tval))
            | _ -> (tList, ("", value))


        // NR Section
        and NR tList: terminal list * (string * NumberType) =
            match tList with 
            | RatNum value :: tail -> match convertToFloat with
                                        | false -> (tail, ("", value))
                                        | true -> 
                                                    let n,d = match value with
                                                                | RATIONAL (x,y) -> x,y
                                                    (tail, ("", FLOAT (float n / float d)))                                                                 
            | Sub :: RatNum value :: tail -> match convertToFloat with
                                        | false -> (tail, ("", value))
                                        | true -> 
                                                    let n,d = match value with
                                                                | RATIONAL (x,y) -> x,y
                                                    (tail, ("", FLOAT (-(float n / float d))))   
            | IntNum value :: tail -> match convertToFloat with
                                        | false -> (tail, ("", value))
                                        | true -> 
                                                    let raw = match value with
                                                                | INTEGER i -> i
                                                    (tail, ("", FLOAT (float raw)))                                                                 
            | Sub :: IntNum value :: tail -> match convertToFloat with
                                        | false -> (tail, ("", -value))
                                        | true -> 
                                                    let raw = match value with
                                                                | INTEGER i -> i
                                                    (tail, ("", FLOAT (float -raw)))

            | FloatNum value :: tail -> (tail, ("", value))
            | Sub :: FloatNum value :: tail -> (tail, ("", -value))
            | Vid vName :: tail -> let res = searchVName vName symList
                                   if (fst res) then (tail, ("", (snd res)))
                                   else raise symError
            | Sub :: Lpar :: tail -> let (tLst, (vID, tval)) = E tail
                                     match tLst with 
                                      | Rpar :: tail -> (tail, ("", -tval))
                                      | _ -> raise parseError
            | Lpar :: tail -> let (tLst, (vID, tval)) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail, ("", tval))
                              | _ -> raise parseError
            | _ -> raise parseError

        // VA similar to original parser VA
        let VA tList = 
            match tList with 
            | Vid vName :: tail -> match tail with 
                                   | Equ :: tail -> let (tLst, (vID, tval)) = E tail
                                                    (tLst, (vName, tval))
                                   | _ -> E tList
            | _ -> E tList
        VA tList     // UPDATE from E tList 

    let rec printTList (lst:list<terminal>) : list<string> = 
        match lst with
        head::tail -> Console.Write("{0} ",head.ToString())
                      printTList tail
                  
        | [] -> Console.Write("EOL\n")
                []

    let rec check4vid sList vName value =  // added to update symbol table list if already existing vName is overwritten
        match sList with
        | head :: tail -> if (fst head) = vName then [(vName,value)]@(check4vid tail vName value) // replace original value
                          else [head]@(check4vid tail vName value) // copy original value
        | _ -> []

    let rec printSymTList (sList:List<string*NumberType>)  =
        match sList with
        | head :: [] -> Console.Write("{0}", head)
                        printSymTList []
        | head :: tail -> Console.Write("{0};", head)
                          printSymTList tail
        | [] -> Console.WriteLine("]")

    // main function run upon the execution of a standard operation
    let rec main_wpf (input, init:bool, symTList:List<string*NumberType>, output) : string*List<string*NumberType> = 
        if input <> null then
            let oList = lexer input
            let sList = printTList oList
            let pList = parser oList  // pList is the remaining token list and should be empty
            if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
            let Out = parseNeval oList false symTList
            let tempID = fst (snd Out)
            let tempVal = snd (snd Out)
            let parsedTempVal = 
                match tempVal with
                | INTEGER i -> string i + " (Integer Type)"
                | FLOAT f -> string f + " (Float Type)"
                | RATIONAL (x,y) -> string x + "\\" + string y + " (Rational Type)"

            Console.WriteLine("Variable name = " + tempID.ToString()) // UPDATE
            Console.WriteLine("Result = " + tempVal.ToString()) // UPDATED

            // Check whether variable name was already in symTList and if so replace with new value
            if tempID.Length > 0 then // update symbol table
                if symTList.IsEmpty then 
                    main_wpf (null, false, symTList@[tempID, tempVal], "Assigned new variable " + tempID + " value " + parsedTempVal)  // append new value if symbol table is empty
                else 
                    let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                    let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)

                    if check then main_wpf (null, false, symTList@[tempID, tempVal], "Assigned new variable " + tempID + " value " + parsedTempVal)  // if true pass old list with appended new tuple                 
                    else main_wpf (null, false, res, "Updated variable " + tempID + " value " + parsedTempVal)   // if false pass updated res list with updated tuple
            else 
                if tempVal.ToString() = null then
                    main_wpf (null, false, symTList, output)
                else
                    main_wpf (null, false, symTList, parsedTempVal)
        else 
            output,symTList

    // modified function for solving a graphing equation without textual ouptut
    let rec solveGequation (input:string, init:bool, reduceToFloat:bool, symTList:List<string*NumberType>, output) : string*List<string*NumberType> = 
            if input <> null then
                let oList = lexer input
                let sList = printTList oList
                let pList = parser oList  // pList is the remaining token list and should be empty
                if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
                let Out = parseNeval oList reduceToFloat symTList
                let tempID = fst (snd Out)
                let tempVal = snd (snd Out)

                Console.WriteLine("Variable name = " + tempID.ToString() ) // UPDATE
                Console.WriteLine("Result = {0}" + tempVal.ToString()  ) // UPDATED

                // Check whether variable name was already in symTList and if so replace with new value
                if tempID.Length > 0 then // update symbol table
                    if symTList.IsEmpty then 
                        solveGequation (null, false, reduceToFloat, symTList@[tempID, tempVal], tempVal)  // append new value if symbol table is empty
                    else 
                        let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                        let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)
                        if check then solveGequation (null, false, reduceToFloat, symTList@[tempID, tempVal], tempVal)  // if true pass old list with appended new tuple                 
                        else solveGequation (null, false, reduceToFloat, res, tempVal)   // if false pass updated res list with updated tuple
                else 
                    if tempVal.ToString() = null then
                        solveGequation (null, false, reduceToFloat, symTList, output)
                    else
                        solveGequation (null, false,  reduceToFloat, symTList,  tempVal)
            else
            let parsedOutput = 
                match output with
                | INTEGER i -> string i
                | FLOAT f -> string f
                | RATIONAL (x,y) -> if reduceToFloat then string (float x / float y)
                                    else
                                        string x + "\\" + string y

            parsedOutput,symTList

    // function used by C# plotting to find the value of x in ax + b = c
    let public graphingFunction (a:string) (b:string) (c:string) (symTList:List<string*NumberType>) = 
        let ga = solveGequation(a, true, true, symTList, INTEGER 0)
        let gb = solveGequation(b, true, true, symTList, INTEGER 0)
        let gc = solveGequation(c, true, true, symTList, INTEGER 0)
    
        let fa = float (fst ga)
        let fb = float (fst gb)
        let fc = float (fst gc)

        if fa = 0. then
            raise linearError
        else
            ((fc - fb) / fa)

    // run a standard test
    let runStandardTest (input:string) (expectedOutputString:string) (inputSymTList:List<string*NumberType>) (expectedSymTList:List<string*NumberType>): string*Boolean =
        let res = solveGequation(input, true, false, inputSymTList, INTEGER 0)
        if fst res <> expectedOutputString then 
            ("Expected output does not match. Expected: '" + expectedOutputString + "' for input '" + input + "', Actual: '" + fst res + "'",false)
        else 
            if snd res <> expectedSymTList then
                ("Expected symlist does not match",false)
            else
                ("OK - Input: " + input + ", Output: " + fst res,true)
 
    // run a graphing test
    let runGraphingTest (coef:string) (const1:string) (const2:string) (expectedXvalue:float) =
        let ans = graphingFunction coef const1 const2 List.empty
        let inputString = "'(" + coef + ")x + (" + const1 + ") = " + const2 + "'"
        if ans <> expectedXvalue then
            ("Expected x value does not match. Expected: '" + string expectedXvalue + "' for input " + inputString + ", Actual: '" + string ans + "'",false)
        else
            ("Test passed! Input: " + inputString + ", X value: " + string expectedXvalue,true)
    
    
    // run a collection of tests and return the results in a list
    let runTests =
   
        // Standard tests (first ones in report section 'Arithmetic expression testing'):
        let test1 = runStandardTest "5 * 3 + (2 * 3 - 2)/ 2 + 6" "23" List.empty List.empty
        let test2 = runStandardTest "9 - 3 - 2" "4" List.empty List.empty
        let test3 = runStandardTest "10 / 3" "3" List.empty List.empty
        let test4 = runStandardTest "10 / 3.0" "3.3333333333333335" List.empty List.empty
        let test5 = runStandardTest "10 % 3" "1" List.empty List.empty
        let test6 = runStandardTest "10 - -2" "12" List.empty List.empty
        let test7 = runStandardTest "-2 + 10" "8" List.empty List.empty
        let test8 = runStandardTest "3 * 5 ^(-1 + 3) - 2^2 * -3" "87" List.empty List.empty
        let test9 = runStandardTest "-3 ^ 2" "9" List.empty List.empty
        let test10 = runStandardTest "-7 % 3" "-1" List.empty List.empty
        let test11 = runStandardTest "3 * 5 ^(-1 + 3) - 2^ -2 * -3" "75" List.empty List.empty
        let test12 = runStandardTest "3 * 5 ^(-1 + 3) - 2.0 ^ -2 * -3" "75.75" List.empty List.empty
        let test13 = runStandardTest "((( 3*2 - -2)))" "8" List.empty List.empty
        let test14 = runStandardTest "-(( 3*5 - 2 * 3))" "-9" List.empty List.empty
    
        // second section: x = 3
        let assignmentTest = runStandardTest "x = 3" "3" List.empty [("x", INTEGER 3)]
        let xTest1 = runStandardTest "(2*x) -x^2 *5" "-39" [("x", INTEGER 3)] [("x", INTEGER 3)]
        let xTest2 = runStandardTest "(2*x) -x^2 *5 / 2" "-16" [("x", INTEGER 3)] [("x", INTEGER 3)]
        let xTest3 = runStandardTest "(2*x) -x^2 * (5 / 2)" "-12" [("x", INTEGER 3)] [("x", INTEGER 3)]
        let xTest4 = runStandardTest "(2*x) -x^2 *5 / 2.0" "-16.5" [("x", INTEGER 3)] [("x", INTEGER 3)]
        let xTest5 = runStandardTest "(2*x) -x^2 *5 % 2" "5" [("x", INTEGER 3)] [("x", INTEGER 3)]
        let xTest6 = runStandardTest "(2*x) -x^2 * (5 % 2)" "-3" [("x", INTEGER 3)] [("x", INTEGER 3)]

        // coef tests
        let coefTest1 = runStandardTest "3(2)" "6" List.empty List.empty
        let coefTest2 = runStandardTest "3(-2)" "-6" List.empty List.empty
        let coefTest3 = runStandardTest "3((3))" "9" List.empty List.empty
        let coefTest4 = runStandardTest "6(12/2^2)" "18" List.empty List.empty
        let coefTest5 = runStandardTest "3x" "12" [("x", INTEGER 4)] [("x", INTEGER 4)]
        let coefTest6 = runStandardTest "x(5)" "15" [("x", INTEGER 3)] [("x", INTEGER 3)]

        // rational tests

        // rational addition
        let rationalTest1 = runStandardTest "1\\6 + 2\\3" "5\\6" List.empty List.empty
        let rationalTest2 = runStandardTest "2\\6 + 2\\3" "1\\1" List.empty List.empty
        let rationalTest3 = runStandardTest "1 + 2\\3" "5\\3" List.empty List.empty
        let rationalTest4 = runStandardTest "1.2 + 2\\3" "28\\15" List.empty List.empty
        let rationalTest5 = runStandardTest "2\\3 + 2" "8\\3" List.empty List.empty
        let rationalTest6 = runStandardTest "2\\6 + 0.5" "5\\6" List.empty List.empty

        // rational subtraction
        let rationalTest7 = runStandardTest "2\\6 - 2\\3" "-1\\3" List.empty List.empty
        let rationalTest8 = runStandardTest "1\\6 - 2\\3" "-1\\2" List.empty List.empty
        let rationalTest9 = runStandardTest "1 - 2\\3" "1\\3" List.empty List.empty
        let rationalTest10 = runStandardTest "0.5 - 2\\3" "-1\\6" List.empty List.empty

        // rational multiplaction
        let rationalTest7 = runStandardTest "2\\6 * 2\\3" "2\\9" List.empty List.empty
        let rationalTest8 = runStandardTest "1\\6 * 2\\3" "1\\9" List.empty List.empty
        let rationalTest9 = runStandardTest "5 * 2\\3" "10\\3" List.empty List.empty
        let rationalTest10 = runStandardTest "0.75 * 13\\25" "39\\100" List.empty List.empty

        // rational divison
        let rationalTest11 = runStandardTest "2\\6 / 2\\3" "1\\2" List.empty List.empty
        let rationalTest12 = runStandardTest "1\\6 / 2\\3" "1\\4" List.empty List.empty
        let rationalTest13 = runStandardTest "5 / 2\\3" "2\\15" List.empty List.empty
        let rationalTest14 = runStandardTest "0.75 / 13\\25" "75\\52" List.empty List.empty

        // rational mod (should all pass with 0 as operation not implemented)
        let rationalTest15 = runStandardTest "2\\6 % 2\\3" "0" List.empty List.empty
        let rationalTest16 = runStandardTest "5 % 4\\5" "0" List.empty List.empty
        let rationalTest17 = runStandardTest "2.2 % 2\\3" "0" List.empty List.empty
        let rationalTest18 = runStandardTest "4\\5 % 2" "0" List.empty List.empty

        // rational exponent
        let rationalTest19 = runStandardTest "2\\6 ^ 2\\3" "1\\729" List.empty List.empty
        let rationalTest20 = runStandardTest "5 ^ 4\\5" "3.623898318388478" List.empty List.empty
        let rationalTest21 = runStandardTest "2.2 ^ 2\\3" "1.6915381116229844" List.empty List.empty
        let rationalTest22 = runStandardTest "4\\5 ^ 2" "16\\25" List.empty List.empty


        /// graphing tests
        let graphingTest1 = runGraphingTest "1" "2" "3" 1.
        let graphingTest2 = runGraphingTest "2 * 3" "1" "13" 2.
        let graphingTest3 = runGraphingTest "(2^2)" "10" "124" 28.5
        let graphingTest4 = runGraphingTest "41 - 12" "3" "32" 1.

        [test1; test2; test3; test4; test5; test6;
        test7; test8; test9; test10; test11; test12; 
        test13; test14; assignmentTest; xTest1; xTest2;
        xTest3; xTest4; xTest5; xTest6; coefTest1; coefTest2; 
        coefTest3; coefTest4; coefTest5; coefTest6; graphingTest1;
        graphingTest2; graphingTest3; graphingTest4; rationalTest1;
        rationalTest2; rationalTest3; rationalTest4; rationalTest5;
        rationalTest6; rationalTest7; rationalTest8; rationalTest9;
        rationalTest10; rationalTest11; rationalTest12; rationalTest13;
        rationalTest14; rationalTest15; rationalTest16; rationalTest17;
        rationalTest18; rationalTest19; rationalTest20; rationalTest21;
        rationalTest22]





