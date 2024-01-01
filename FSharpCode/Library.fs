namespace FSharpCodeLib

type Point = {X: int; Y: int; Z: int}

module Say =
    let hello name =
        printfn "Hello %s" name


// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report
open System

module lang =

    type NumberType =
    | NT_INT of int
    | NT_FLOAT of float
    with
       static member getIntValue (f1:NumberType) =
                match f1 with
                | NT_INT x -> int x
                | NT_FLOAT x -> int x

       static member getFloatValue (f1:NumberType) =
                match f1 with
                | NT_INT x -> float x
                | NT_FLOAT x -> float x

        static member Pow (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (int (float x ** y))
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x ** y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x ** y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x ** float y)

        static member (+) (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (x + y)
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x + y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x + y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x + float y)


        static member (-) (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (x - y)
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x - y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x - y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x - float y)


        static member (*) (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (x * y)
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x * y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x * y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x * float y)


        static member (/) (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (x / y)
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x / y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x / y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x / float y)

        static member (%) (f1:NumberType, f2:NumberType) =
                match f1, f2 with
                | NT_INT x, NT_INT y -> NT_INT (x % y)
                | NT_FLOAT x, NT_FLOAT y -> NT_FLOAT (x % y)
                | NT_INT x, NT_FLOAT y -> NT_FLOAT (float x % y)
                | NT_FLOAT x, NT_INT y -> NT_FLOAT (x % float y)

        static member (~-) (f1:NumberType) =
                match f1 with
                | NT_INT x -> NT_INT (-x)
                | NT_FLOAT x -> NT_FLOAT (-x)


    // list of token types - this is a I believe a discriminated union
    type terminal = 
        Add | Sub | Mul | Div | Pow | Mod | Lpar | Rpar | Equ | FloatNum of NumberType | Vid of string | IntNum of NumberType



    let str2lst s = [for c in s -> c]           // simple function to turn a string into a list of chars
    let isblank c = System.Char.IsWhiteSpace c  // simple function to check if char c is whitespace
    let isdigit c = System.Char.IsDigit c       // simple function to check if char c is a digit
    let isdot c =  c = '.'                      // simple function to check if char c is a dot
    let isletter c = System.Char.IsLetter c       // simple function to check if char c is a letter
    let intVal (c:char) = (int)((int)c - (int)'0') // convert char c into int
    let floatVal (c:char) = (float)((float)c - (float)'0') // convert char c into int

    // exception functions
    let lexError = System.Exception("Lexer error")
    let parseError = System.Exception("Parser error")
    let divisionByZeroError = System.Exception("Division by zero error!")
    let symError = System.Exception("No value associated to variable name")
    let linearError = System.Exception("The coefficient cannot be 0.")



    // Checks if the first number in a string list is a float
    let isNextFloat(iStr) = 
        let rec recursiveloop(iStr) = 
            match iStr with
             | c :: tail when isdot c -> (true)
             | c :: tail when isdigit c -> recursiveloop(tail)
             | _ -> (false)

        recursiveloop(iStr)


    // function to get the value of a string number as in integer
    // for example, string '123' would be converted to int 123
    let rec scFloat(iStr, iVal, pastDotCount) = 
        match iStr with
            | c :: tail when isdigit c && pastDotCount <> 0 -> 
                                        scFloat(tail, float iVal+(floatVal c / (float 10 ** pastDotCount)), pastDotCount + 1)

            | c :: tail when isdot c -> scFloat(tail, float (iVal), 1)
            | c :: tail when isdigit c -> scFloat(tail, float 10*iVal+(floatVal c), 0)
            | _ -> (iStr, iVal) // return tuple of string and value

    let rec scInt(iStr, iVal) = 
        // this function works recursively, so we match the input string with two options:
        //
        // option 1: c::tail, where c is a digit:
        // In this case, we re-call scInt with the tail, and 10*ival with the addition of c converted
        // into an integer. We times the old ival by 10 to increase it's base, which makes sense, as
        // we want to shift it up a decimal place and put the new value in the 'ones' position.
        // E.g. if the string was 34, first round we get 3, then we want to 3*10 to get 30, + next number 4

        // Option 2: _
        // In the case where the pattern above is not matched - that is istr is not a string with the first
        // char of the string being a digit - we at that point return the remaining istr along with the value
        // we calculated up to that point, ival
        match iStr with
        | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
        | _ -> (iStr, iVal) // return tuple of string and value



    // The same as the above, but instead of for converting a string into an integer, we convert
    // a string into a variable name. We check that the head 'c' is a letter, and if so add it onto
    // the end of the variable name we have so far vName. Once 'c' is no longer a letter, we return the
    // rest of the string + the vName in a tuple.
    let rec scChar(iStr, vName:string) =
        match iStr with
        | c :: tail when isletter c -> scChar(tail,(vName + c.ToString()))
        | _ -> (iStr, vName)

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
                                        | false -> let (iStr, iVal)  = scInt(tail, intVal c) 
                                                   IntNum (NT_INT iVal) :: scan iStr
                                        | true -> let (iStr, iVal)  = scFloat(tail, intVal c, 0) 
                                                  FloatNum (NT_FLOAT iVal) :: scan iStr

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
    // <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | <empty>

    // <P>        ::= <C> <Popt>
    // <Popt>     ::= "^" <NR> <Popt> | <empty>

    // <C>        ::= <NR> <Copt>
    // <Copt>     ::= "(" <E> ")" | <varID> | <empty>

    // <NR>       ::= ["IntNum" | "FloatNum" | "varVal" ] <value> | "(" <E> ")"
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
            | _ -> tList

        and P tList = (C >> Popt) tList
        and Popt tList =
            match tList with
            | Pow :: tail -> (C >> Popt) tail
            | _ -> tList

        and C tList = (NR >> Copt) tList
        and Copt tList =
            match tList with
            | Vid vName :: tail -> tail
            | Lpar :: tail -> match E tail with 
                              | Rpar :: tail -> tail
                              | _ -> raise parseError
            | _ -> tList

        // NR simply matches either a number or variable name
        // it also can match brackets
        and NR tList =
            match tList with 
            | IntNum value :: tail -> tail
            | Sub :: IntNum value :: tail -> tail
            | FloatNum value :: tail -> tail
            | Sub :: FloatNum value :: tail -> tail
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
        | _ -> (false, NT_INT 0)




    // Parse and evaluate
    let parseNeval tList (symList:List<string*NumberType>) = // UPDATED

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
                             if (tval <> tval * NT_INT 0) then Topt (tLst, (vID, value / tval))
                             else raise divisionByZeroError
            | Mod :: tail -> let (tLst, (vID, tval)) = P tail
                             if (tval <> tval * NT_INT 0) then Topt (tLst, (vID, value % tval))
                             else raise divisionByZeroError
            | _ -> (tList, ("", value))


        // T section
        and P tList = (C >> Popt) tList
        and Popt (tList, (vID, value)) =
            match tList with
            | Pow :: tail -> let (tLst, (vID, tval)) = C tail
                             Popt (tLst, (vID, value ** tval))
            | _ -> (tList, ("", value))


        // T section
        and C tList = (NR >> Copt) tList
        and Copt (tList, (vID, value)) =
            match tList with
            | Vid vName :: tail -> let res = searchVName vName symList
                                   if (fst res) then (tail, (vID, value * (snd res)))
                                   else raise symError
            | Lpar :: tail -> let (tLst, (vID, tval)) = E tail
                              match tLst with 
                              | Rpar :: tail -> (tail,  (vID, value * tval))
                              | _ -> raise parseError
            | _ -> (tList, ("", value))

        // NR Section
        and NR tList: terminal list * (string * NumberType) =
            match tList with 
            | IntNum value :: tail -> (tail, ("", value))                                                        
            | Sub :: IntNum value :: tail -> (tail, ("", -value))
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

    let rec main_wpf (input, init:bool, symTList:List<string*NumberType>, output) : string*List<string*NumberType> = 

        if input <> null then
            let oList = lexer input
            let sList = printTList oList
            let pList = parser oList  // pList is the remaining token list and should be empty
            if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
            let Out = parseNeval oList symTList
            let tempID = fst (snd Out)
            let tempVal = snd (snd Out)

            Console.WriteLine("Variable name = " + tempID.ToString() ) // UPDATE
            Console.WriteLine("Result = {0}" + tempVal.ToString()  ) // UPDATED

            // Check whether variable name was already in symTList and if so replace with new value
            if tempID.Length > 0 then // update symbol table
                if symTList.IsEmpty then 
                    main_wpf (null, false, symTList@[tempID, tempVal], "Assigned new variable " + tempID + " value " + tempVal.ToString())  // append new value if symbol table is empty
                else 
                    let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                    let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)
                    if check then main_wpf (null, false, symTList@[tempID, tempVal], "Assigned new variable " + tempID + " value " + tempVal.ToString())  // if true pass old list with appended new tuple                 
                    else main_wpf (null, false, res, "Updated variable " + tempID + " value " + tempVal.ToString())   // if false pass updated res list with updated tuple
            else 
                if tempVal.ToString() = null then
                    main_wpf (null, false, symTList, output)
                else
                    main_wpf (null, false, symTList,  tempVal.ToString())
        else 
            output,symTList


    let rec solveGequation (input:string, init:bool, symTList:List<string*NumberType>, output) : string*List<string*NumberType> = 
            if input <> null then
                let oList = lexer input
                let sList = printTList oList
                let pList = parser oList  // pList is the remaining token list and should be empty
                if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
                let Out = parseNeval oList symTList
                let tempID = fst (snd Out)
                let tempVal = snd (snd Out)

                Console.WriteLine("Variable name = " + tempID.ToString() ) // UPDATE
                Console.WriteLine("Result = {0}" + tempVal.ToString()  ) // UPDATED

                // Check whether variable name was already in symTList and if so replace with new value
                if tempID.Length > 0 then // update symbol table
                    if symTList.IsEmpty then 
                        solveGequation (null, false, symTList@[tempID, tempVal], tempVal)  // append new value if symbol table is empty
                    else 
                        let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                        let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)
                        if check then solveGequation (null, false, symTList@[tempID, tempVal], tempVal)  // if true pass old list with appended new tuple                 
                        else solveGequation (null, false, res, tempVal)   // if false pass updated res list with updated tuple
                else 
                    if tempVal.ToString() = null then
                        solveGequation (null, false, symTList, output)
                    else
                        solveGequation (null, false, symTList,  tempVal)
            else

                let parsedOutput = 
                    match output with
                    | NT_INT i -> float i
                    | NT_FLOAT f -> f

                string parsedOutput,symTList

    // ax + b = c
    // Function to find x:
    let findXinLinearEq a b c =
        // if the co-efficient a is 0, then we cannot map the linear function
        if a = 0. then
            None
        else
            Some ((c - b) / a)


    // ax + b = c
    let public graphingFunction (a:string) (b:string) (c:string) (symTList:List<string*NumberType>) = 
        let ga = solveGequation(a, true, symTList, NT_INT 0)
        let gb = solveGequation(b, true, symTList, NT_INT 0)
        let gc = solveGequation(c, true, symTList, NT_INT 0)
    
        let fa = float (fst ga)
        let fb = float (fst gb)
        let fc = float (fst gc)

        if fa = 0. then
            raise linearError
        else
            ((fc - fb) / fa)

    let runStandardTest (input:string) (expectedOutputString:string) (inputSymTList:List<string*NumberType>) (expectedSymTList:List<string*NumberType>): string*Boolean =
        let res = solveGequation(input, true, inputSymTList, NT_INT 0)
        if fst res <> expectedOutputString then 
            ("Expected output does not match. Expected: '" + expectedOutputString + "' for input '" + input + "', Actual: '" + fst res + "'",false)
        else 
            if snd res <> expectedSymTList then
                ("Expected symlist does not match",false)
            else
                ("Test passed! Input: " + input + ", Output: " + fst res,true)
 

    let runGraphingTest (coef:string) (const1:string) (const2:string) (expectedXvalue:float) =
        let ans = graphingFunction coef const1 const2 List.empty
        let inputString = "'(" + coef + ")x + (" + const1 + ") = " + const2 + "'"
        if ans <> expectedXvalue then
            ("Expected x value does not match. Expected: '" + string expectedXvalue + "' for input " + inputString + ", Actual: '" + string ans + "'",false)
        else
            ("Test passed! Input: " + inputString + ", X value: " + string expectedXvalue,true)
        
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
        let assignmentTest = runStandardTest "x = 3" "3" List.empty [("x", NT_INT 3)]
        let xTest1 = runStandardTest "(2*x) -x^2 *5" "-39" [("x", NT_INT 3)] [("x", NT_INT 3)]
        let xTest2 = runStandardTest "(2*x) -x^2 *5 / 2" "-16" [("x", NT_INT 3)] [("x", NT_INT 3)]
        let xTest3 = runStandardTest "(2*x) -x^2 * (5 / 2)" "-12" [("x", NT_INT 3)] [("x", NT_INT 3)]
        let xTest4 = runStandardTest "(2*x) -x^2 *5 / 2.0" "-16.5" [("x", NT_INT 3)] [("x", NT_INT 3)]
        let xTest5 = runStandardTest "(2*x) -x^2 *5 % 2" "5" [("x", NT_INT 3)] [("x", NT_INT 3)]
        let xTest6 = runStandardTest "(2*x) -x^2 * (5 % 2)" "-3" [("x", NT_INT 3)] [("x", NT_INT 3)]

        /// graphing tests
        let graphingTest1 = runGraphingTest "1" "2" "3" 1.
        let graphingTest2 = runGraphingTest "2 * 3" "1" "13" 2.
        let graphingTest3 = runGraphingTest "(2^2)" "10" "124" 28.5
        let graphingTest4 = runGraphingTest "41 - 12" "3" "32" 1.

        let tests = [test1; test2; test3; test4; test5; test6;
        test7; test8; test9; test10; test11; test12; 
        test13; test14; assignmentTest; xTest1; xTest2;
        xTest3; xTest4; xTest5; xTest6; graphingTest1;
        graphingTest2; graphingTest3; graphingTest4]

        for test in tests do
            match snd test with
            | true -> Console.WriteLine("ok - " + fst test)
            | false -> Console.WriteLine("FAILURE - " + fst test)
        



