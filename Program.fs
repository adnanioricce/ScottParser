open System
let parseA str =
    if String.IsNullOrWhiteSpace(str) then
        (false,"")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true,remaining)
    else
        (false,str)
[<EntryPoint>]
let main argv =
    printfn "%A " (parseA "ABC")
    printfn "%A " (parseA "ZBC")
    0