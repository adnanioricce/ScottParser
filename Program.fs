open System
let pchar (charToMath,str) =
    if String.IsNullOrWhiteSpace(str) |> not then
        let first = str.[0]
        if first = charToMath then
            let remaining = str.[1..]
            let msg = sprintf "Found %c" charToMath
            (msg, remaining)
        else
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMath first
            (msg,str)        
    else
        let msg = "No more input"
        (msg,"")
            
let parseA str = pchar ('A',str)
[<EntryPoint>]
let main argv =
    printfn "%A " (parseA "ABC")
    printfn "%A " (parseA "ZBC")
    0