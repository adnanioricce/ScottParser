open System
type ParseResult<'a> =
| Success of 'a
| Failure of string

let pchar (charToMath,str) =
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
            
let parseA str = pchar ('A',str)
[<EntryPoint>]
let main argv =
    printfn "%A " (parseA "ABC")
    printfn "%A " (parseA "ZBC")
    0