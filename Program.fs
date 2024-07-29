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

let ( .>>. ) = andThen

let parseA = pchar 'A'
let parseB = pchar 'B'
let parseAThenB = parseA .>>. parseB
[<EntryPoint>]
let main argv =
    printfn "%A " (run parseAThenB "ABC")
    printfn "%A " (run parseAThenB "ZBC")
    printfn "%A " (run parseAThenB "AZC")
    0