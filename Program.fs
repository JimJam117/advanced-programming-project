// For more information see https://aka.ms/fsharp-console-apps

// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    | Add | Sub | Mul | Div | Lpar | Rpar | Num of int | NumF of float | Uminus | Uplus | Pow //unary minus and unary plus


let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isdot c = c = '.'
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")

let rec scFloat (iStr, iVal, afterDot, multiplier) =
    match iStr with
    | c :: tail when isdigit c -> 
        if afterDot then 
            scFloat(tail, iVal + (float (intVal c) * multiplier), true, multiplier * 0.1)
        else
            scFloat(tail, iVal * 10.0 + float (intVal c), false, multiplier)
    | '.' :: tail when not afterDot -> scFloat(tail, iVal, true, 0.1)
    | _ -> (iStr, iVal)

let rec scInt(iStr, iVal) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

let lexer input = 
    let rec scan (prev: char option) input =
        match input with
        | [] -> []
        | '^'::tail -> Pow :: scan (Some '^') tail
        | '+'::tail when (prev = None || prev = Some '(' || prev = Some '+' || prev = Some '-' || prev = Some '*' || prev = Some '/') -> Uplus :: scan (Some '+') tail
        | '+'::tail -> Add :: scan (Some '+') tail
        | '-'::tail when (prev = None || prev = Some '(' || prev = Some '+' || prev = Some '-' || prev = Some '*' || prev = Some '/') -> Uminus :: scan (Some '-') tail //unary minus
        | '-'::tail -> Sub :: scan (Some '-') tail
        | '*'::tail -> Mul :: scan (Some '*') tail
        | '/'::tail -> Div :: scan (Some '/') tail
        | '('::tail -> Lpar:: scan (Some '(') tail
        | ')'::tail -> Rpar:: scan (Some ')') tail
        | c :: tail when isblank c -> scan prev tail
        | c :: tail when isdigit c -> 
            let (iStr, iVal) = scFloat(tail, float (intVal c), false, 1.0)
            if iVal % 1.0 = 0.0 then
                Num (int iVal) :: scan (Some 'N') iStr //add Num iVal terminal to front of list
            else
                NumF iVal :: scan (Some 'N') iStr //add NumF iVal terminal to front of list
        | _ -> raise lexError
    scan None (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// BNF Grammar:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
// <P>        ::= <NR> <Popt>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")" | "-" <NR> | "+" <NR>  // unary minus and unary plus


let parser tList = 

    let rec E tList = (T >> Eopt) tList
    and Eopt tList =
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList

    and T tList = (P >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (NR >> Topt) tail
        | Div :: tail -> (NR >> Topt) tail
        | _ -> tList


    and P tList = (NR >> Popt) tList
    and Popt tList =
        match tList with
        | Pow :: tail -> (NR >> Popt) tail
        | _ -> tList

    and NR tList =
        match tList with
        | Uplus :: tail -> NR tail  // Handle unary plus
        | Uminus :: tail -> NR tail  // unary minus
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError

    E tList


let parseNeval tList = 

    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)

    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = NR tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = NR tail
                         Topt (tLst, value / tval)
        | _ -> (tList, value)

    and P tList = (NR >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pow :: tail -> let (tLst, tval) = NR tail
                         Popt (tLst, Math.Pow(value, tval))
        | _ -> (tList, value)

    and NR tList =
        match tList with
        | Uplus :: tail -> NR tail  // Handle unary plus (no change in value)
        | Uminus :: tail -> let (tLst, tval) = NR tail  // unary minus
                            (tLst, -tval)
        | Num value :: tail -> (tail, float value)
        | NumF value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError

    E tList


let rec printTList (lst:list<terminal>) : list<string> =
    match lst with
    | head::tail -> Console.Write("{0} ",head.ToString())
                    printTList tail
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    0
