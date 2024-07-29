﻿open System
open Microsoft.FSharp.Core
type ParseResult<'a> =
| Success of 'a
| Failure of string
type Parse<'a> = Parser of (string -> ParseResult<'a * string>)
let pchar charToMath =
    let innerFn str = 
        if String.IsNullOrWhiteSpace(str) |> not then
            let first = str.[0]
            if first = charToMath then
                let remaining = str.[1..]
                //let msg = sprintf "Found %c" charToMath
                Success (charToMath, remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMath first
                Failure msg        
        else
            let msg = "No more input"
            Failure msg
    Parser innerFn
let run parser input =
    // unwrap parser to get inner function
    let (Parser innerFn) = parser
    // call inner function with input
    innerFn input
let returnP x =
    let innerFn input =
        // ignore the input and return x
        Success (x, input)
    // return the inner function
    Parser innerFn
let bindP f p =
    let innerFn input =
        let result = run p input
        match result with
        | Failure err ->
            // return error from parser
            Failure err
        | Success (value, remainingInput) ->
            // apply f to get a new parser
            let p2 = f value
            // run parser with remaining input
            run p2 remainingInput
    Parser innerFn
    
// this is the operator that points to a bind function
let ( >>= ) p f = bindP f p
let andThen lhsParser rhsParser =
    lhsParser >>= (fun lhsResult ->
        rhsParser >>= (fun rhsResult ->
            returnP (lhsResult,rhsResult)))    
let orElse lParser rParser =
    let innerFn input =
        // run lParser with the input
        let lResult = run lParser input
        
        // test the result for Failure/Success
        match lResult with
        | Success result ->
            // if success, return the original result
            lResult
        | Failure err ->
            // if failed, run rParser with the input
            let rResult = run rParser input
            
            // return rParser result
            rResult
    // return the inner function
    Parser innerFn
let mapP f parser =
    let innerFn input =
        // run parser with the input
        let result = run parser input
        // test the resul for Failure/Success
        match result with
        | Success (value,remaining) ->
            // if success, return the value transformed by f
            let newValue = f value
            Success (newValue, remaining)
        | Failure err ->
            // if failed, return the error
            Failure err
    // return the inner function
    Parser innerFn

//I thought this was a bind, but it wasn't
let ( .>>. ) = andThen

let ( <|> ) = orElse
let ( <!> ) = mapP
let ( |>> ) x f = mapP f x
let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none
let choice listOfParsers = List.reduce ( <|> ) listOfParsers
let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice
let applyP fP xP =
    (fP .>>. xP)
    |> mapP (fun (f,x) -> f x)
let ( <*> ) = applyP
let lift2 f xP yP =
    returnP f <*> xP <*> yP
let rec sequence parserList =
    // define the "cons" functions, which is a two parameter function
    let cons head tail = head :: tail
    
    // lift it to Parser world(or context if you prefer)
    let consP = lift2 cons
    
    // process the list of parsers recursively
    match parserList with
    | [] -> returnP []
    | head::tail -> consP head (sequence tail)
let addP = lift2 (+)
/// Keep only the result of the left side parser
let (.>>) p1 p2 =
    // create a pair
    p1 .>>. p2
    |> mapP (fun (a,b) -> a)
/// Keep only the result of the right side parser
let (>>.) p1 p2 =
    // create a pair
    p1 .>>. p2
    |> mapP (fun (a,b) -> b)
let startsWith (str:string) (prefix:string) =
    str.Contains(prefix)
let startsWithP =
    lift2 startsWith
let parseLowercase =
    anyOf ['a'..'z']
let parseDigit =
    anyOf ['0'..'9']
let parseThreeDigitsAsStr =
    // more compact version
    // (parseDigit .>>. parseDigit .>>. parseDigit)
    // |>> fun ((c1,c2),c3) -> System.String [| c1;c2;c3 |]
    // create a parser that returns a tuple
    let tupleParser =
        parseDigit .>>. parseDigit .>>. parseDigit
    
    // create a function that turns the tuple into a string
    let transformTuple ((c1,c2), c3) =
        System.String [| c1; c2; c3 |]
    // use "map" to combine them
    mapP transformTuple tupleParser
let parseThreeDigitsAsInt =
    mapP int parseThreeDigitsAsStr
let parseA = pchar 'A'
let parseB = pchar 'B'
let parseC = pchar 'C'
let charListToStr charList =
    charList |> List.toArray |> System.String
// match a specific string
let pstring str =
    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = run parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure err ->
        // if parse fails, return empty list
        ([],input)
    | Success (firstValue, inputAfterFirstParse) ->
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)
/// match zero or more occurrences of the specified parser
let many parser =
    let innerFn input =
        Success (parseZeroOrMore parser input)
    Parser innerFn
let manyA = many (pchar 'A')
/// match one or more occurrences of the specified parser
let many1 parser =
    parser >>= (fun head ->
        many parser >>= (fun tail ->
            returnP (head :: tail)))

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
  let sepThenP = sep >>. p
  p .>>. many sepThenP
  |>> fun (p,pList) -> p::pList
/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

let pint =
    // helper
    let resultToInt (sign,charList) =
        let i = charList |> charListToStr |> int
        match sign with
        | Some ch -> -i // negate the int
        | None -> i        
                         
    // define parser for one or more digits
    let digits = many1 parseDigit
    
    // map the digits to an int
    opt (pchar '-') .>>. digits
    |>> resultToInt
let bOrElseC = parseB <|> parseC
let parseAThenB = parseA .>>. parseB
let parseAOrElseB = parseA <|> parseB
let parseABC = pstring "ABC"
let aAndThenBorC = parseA .>>. bOrElseC
let parseThreeDigits =
    parseDigit .>>. parseDigit .>>. parseDigit

[<EntryPoint>]
let main argv =
    printfn "%A " (run parseAThenB "ABC")
    printfn "%A " (run parseAThenB "ZBC")
    printfn "%A " (run parseAThenB "AZC")
    printfn "%A " (run parseAOrElseB "AZZ")
    printfn "%A " (run parseAOrElseB "BZZ")
    printfn "%A " (run parseAOrElseB "CZZ")
    printfn "%A " (run aAndThenBorC "ABZ")
    printfn "%A " (run aAndThenBorC "ACZ")
    printfn "%A " (run aAndThenBorC "QBZ")
    printfn "%A " (run aAndThenBorC "AQZ")
    printfn "%A " (run parseLowercase "aBC")
    printfn "%A " (run parseLowercase "ABC")
    printfn "%A " (run parseDigit "1ABC")
    printfn "%A " (run parseDigit "9ABC")
    printfn "%A " (run parseDigit "|ABC")
    printfn "%A" (run parseThreeDigits "123A")
    printfn "%A" (run parseThreeDigitsAsStr "123A")
    printfn "%A" (run parseThreeDigitsAsInt "123A")
    let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
    let combined = sequence parsers
    
    printfn "%A" (run combined "ABCD")
    
    let parseABC = pstring "ABC"
    printfn "%A" (run parseABC "ABCDE") 
    printfn "%A" (run parseABC "A|CDE") 
    printfn "%A" (run parseABC "AB|DE")
    // test some success cases
    printfn "%A" (run manyA "ABCD") 
    printfn "%A" (run manyA "AACD") 
    printfn "%A" (run manyA "AAAD")
    // test a case with no matches
    printfn "%A" (run manyA "|BCD")
    
    let manyAB = many (pstring "AB")

    printfn "%A" (run manyAB "ABCD")
    printfn "%A" (run manyAB "ABABCD")
    printfn "%A" (run manyAB "ZCD")
    printfn "%A" (run manyAB "AZCD")
    
    let whitespaceChar = anyOf [' ';'\t';'\n']
    let whitespace = many whitespaceChar
    
    printfn "%A" (run whitespace "ABC")
    printfn "%A" (run whitespace " ABC")
    printfn "%A" (run whitespace "\tABC")
    
    // define parser for one or more digits
    let digits = many1 parseDigit 
    
    printfn "%A" (run digits "1ABC")
    printfn "%A" (run digits "12BC")
    printfn "%A" (run digits "123C")
    printfn "%A" (run digits "1234")
    
    printfn "%A" (run digits "ABC")
    
    printfn "%A" (run pint "1ABC")
    printfn "%A" (run pint "12BC")
    printfn "%A" (run pint "123C")
    printfn "%A" (run pint "1234")    
    
    printfn "%A" (run pint "ABC")
    let parseDigitThenSemicolon = parseDigit .>> opt (pchar ';')
    printfn "%A" (run parseDigitThenSemicolon "1;")
    printfn "%A" (run parseDigitThenSemicolon "1")
    
    printfn "%A" (run pint "123C")
    printfn "%A" (run pint "-123C")
    
    let ab = pstring "AB"
    let cd = pstring "CD"
    let ab_cd = (ab .>> whitespace) .>>. cd
    
    printfn "%A" (run ab_cd "AB \t\nCD")
    
    /// Keep only the result of the middle parser
    let between p1 p2 p3 =
        p1 >>. p2 .>> p3
    
    let pdoublequote = pchar '"'
    let quotedInteger = between pdoublequote pint pdoublequote
    
    printfn "%A" (run quotedInteger "\"1234\"")
    printfn "%A" (run quotedInteger "1234")
    
    let comma = pchar ','    
    
    let zeroOrMoreDigitList = sepBy parseDigit comma
    let oneOrMoreDigitList = sepBy1 parseDigit comma
    
    printfn "%A" (run oneOrMoreDigitList "1;")
    printfn "%A" (run oneOrMoreDigitList "1,2")
    printfn "%A" (run oneOrMoreDigitList "1,2,3")
    printfn "%A" (run oneOrMoreDigitList "Z;")
    
    printfn "%A" (run zeroOrMoreDigitList "1;")
    printfn "%A" (run zeroOrMoreDigitList "1,2")
    printfn "%A" (run zeroOrMoreDigitList "1,2,3")
    printfn "%A" (run zeroOrMoreDigitList "Z;")
    0