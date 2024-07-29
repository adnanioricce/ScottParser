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
    
let parseA = pchar 'A'
[<EntryPoint>]
let main argv =
    printfn "%A " (run parseA "ABC")
    printfn "%A " (run parseA "ZBC")
    0