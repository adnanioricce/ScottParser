open System
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
let andThen lhsParser rhsParser =
    let innerFn input =
        // Run left parser with the input
        let lhsResult = run lhsParser input
        
        // test the result for Failure/Success
        match lhsResult with
        | Failure err ->
            // return error from lhsParser
            Failure err
        | Success (lhsValue, lhsRemaining) ->
            // run rhsParser with the remaining input
            let rhsResult = run rhsParser lhsRemaining
            
            // test the result for Failure/Success
            match rhsResult with
            | Failure err ->
                // return error from rhsParser
                Failure err
            | Success (rhsValue,rhsRemaining) ->
                // combine both values as a pair
                let newValue = (lhsValue,rhsValue)
                // return remaining input after rhsParser
                Success (newValue,rhsRemaining)
    // return the inner function
    Parser innerFn
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
let returnP x =
    let innerFn input =
        // ignore the input and return x
        Success (x, input)
    // return the inner function
    Parser innerFn
let ( .>>. ) = andThen
let ( <|> ) = orElse
let ( <!> ) = mapP
let ( |>> ) x f = mapP f x
     
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
    0