open System
open Microsoft.FSharp.Core
type Position = { 
    line: int
    column: int    
}
/// define an initial position
let initialPos = {line = 0; column = 0}
/// increment the column number
let incrCol (pos: Position) =
    { pos with column=pos.column + 1}

/// increment the line number and set the column to 0
let incrLine pos =
    {line = pos.line + 1; column = 0}
type InputState = {
    lines : string []
    position: Position
}
module InputState =
    /// Create a new InputState from a string
    let fromStr str =
        if String.IsNullOrWhiteSpace(str) then
            { lines = [||]; position = initialPos}
        else
            let separators = [| "\r\n"; "\n"|]
            let lines = str.Split(separators, StringSplitOptions.None)
            { lines = lines; position = initialPos}
    // return the current line            
    let currentLine inputState =
        let linePos = inputState.position.line
        if linePos < inputState.lines.Length then
            inputState.lines.[linePos]
        else
            "end of file"
    
    /// Get the next character from the input, if any
    /// else return None. Also return the updated InputState
    let nextChar input =
        let linePos = input.position.line
        let colPos = input.position.column
        // three cases
        // 1) if line >= maxLine ->
        //      return EOF
        // 2) if col less than line length ->
        //      return char at colPos, increment colPos 
        // 3) if col at line length ->
        //      return NewLine, increment linePos
        if linePos >= input.lines.Length then
            input,None
        else
            let currentLine = currentLine input
            if colPos < currentLine.Length then
                let char = currentLine.[colPos]
                let newPos = incrCol input.position
                let newState = { input with position=newPos}
                newState, Some char
            else
                // end of line, so return LF and move to next line
                let char = '\n'
                let newPos = incrLine input.position
                let newState = { input with position = newPos }
                newState, Some char
let rec readAllChars input =
    [
        let remainingInput, charOpt = InputState.nextChar input
        match charOpt with
        | None ->
            // end of input
            ()
        | Some ch ->
            // return first character
            yield ch
            yield! readAllChars remainingInput
    ]
type ParserPosition = {
    currentLine : string 
    line : int
    column : int
}
let parserPositionFromInputState (inputState: InputState) = {
    currentLine = InputState.currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
}
type ParserLabel = string
type ParserError = string
type ParseResult<'a> =
| Success of 'a
| Failure of ParserLabel * ParserError * ParserPosition
let printResult (r) =
    match r with
    | Success (value,input) ->
        printfn "%A" value
    | Failure (label,error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error    
        printfn "Line %i Col: %i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret
// type Parse<'a> = Parser of (string -> ParseResult<'a * string>)
type Parser<'a> = {
    parseFn: (InputState -> ParseResult<'a * InputState>)
    label: ParserLabel
}
/// get the label from a parser
let getLabel parser =
    // get label
    parser.label 
// Update the label in the parser
let setLabel parser newLabel =
    // Change the inner function to use the new label
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            // if Success, do nothing
            Success s
        | Failure (oldLabel, err,pos) ->
            // if Failure, return new label
            Failure (newLabel, err, pos)
    // return the Parser
    {parseFn = newInnerFn; label = newLabel}
let ( <?> ) = setLabel
/// Stores information about the parser position for error messages



/// Match an input token if the predicate is satisfied
let satisfy predicate label =
    let innerFn input =
        let currentLine = input |> InputState.currentLine        
        if String.IsNullOrEmpty(currentLine) then
            Failure (label,"No more input", {column = 0;line = 0;currentLine = currentLine})
        else
            let newState,(Some first) = input |> InputState.nextChar
            if predicate first then                
                Success (first,newState)
            else
                let err = sprintf "Unexpected '%c'" first
                Failure (label, err, {column = 0;line = 0;currentLine = currentLine})

    {parseFn = innerFn;label = label}

let pchar charToMath =
    let predicate ch = (ch = charToMath)
    let label = sprintf "%c" charToMath
    satisfy predicate label

/// Run the parser on a InputState
let runOnInput parser input =
  // call inner function with input
  parser.parseFn input
// Run a parser with some input    
let run (parser:Parser<_>) input =    
    // call inner function with input
    runOnInput parser (InputState.fromStr input)
let returnP x =
    let label = "unknown"
    let innerFn input =
        // ignore the input and return x
        Success (x, input)
    // return the inner function
    {parseFn = innerFn; label = label}
let bindP f p =
    let label = "unknown"    
    let innerFn input =
        let result = runOnInput p input
        match result with
        | Failure (err,label, pos) ->
            // return error from parser
            Failure (err,label,pos)
        | Success (value, remainingInput) ->
            // apply f to get a new parser
            let p2 = f value
            // run parser with remaining input
            runOnInput p2 remainingInput
    {parseFn = innerFn;label = label}
    
// this is the operator that points to a bind function
let ( >>= ) p f = bindP f p
let andThen lhsParser rhsParser =
    let label = sprintf "%s andThen %s" (getLabel lhsParser) (getLabel rhsParser)
    lhsParser >>= (fun lhsResult ->
        rhsParser >>= (fun rhsResult ->
            returnP (lhsResult,rhsResult))) 
        <?> label
    
let orElse lParser rParser =
    let label =
        sprintf "%s orElse %s" (getLabel lParser) (getLabel rParser)
    let innerFn input =
        // run lParser with the input
        let lResult = runOnInput lParser input
        
        // test the result for Failure/Success
        match lResult with
        | Success result ->
            // if success, return the original result
            lResult
        | Failure (err,label, pos) ->
            // if failed, run rParser with the input
            let rResult = runOnInput rParser input
            
            // return rParser result
            rResult
    // return the inner function
    {parseFn = innerFn; label = label}
let mapP f parser =
    let label = "unknown"
    let innerFn input =
        // run parser with the input
        let result = runOnInput parser input
        // test the resul for Failure/Success
        match result with
        | Success (value,remaining) ->
            // if success, return the value transformed by f
            let newValue = f value
            Success (newValue, remaining)
        | Failure (label, err, pos) ->
            // if failed, return the error
            Failure (label,err, pos)
    // return the inner function
    {parseFn = innerFn;label = label}

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
    let label = sprintf "any of %A" listOfChars    
    listOfChars
    |> List.map pchar
    |> choice
    <?> label
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
    let predicate = Char.IsAsciiLetterLower
    let label = "lowercase"
    satisfy predicate label
let parseDigit =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label
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
    let firstResult = runOnInput parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure (label,err, pos) ->
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
    let label = "unknown"
    let innerFn input =
        Success (parseZeroOrMore parser input)
    {parseFn = innerFn;label = label}
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
    printfn "%A " (run parseAThenB ("ABC"))
    printfn "%A " (run parseAThenB ("ZBC"))
    printfn "%A " (run parseAThenB ("AZC"))
    printfn "%A " (run parseAOrElseB ("AZZ"))
    printfn "%A " (run parseAOrElseB ( "BZZ"))
    printfn "%A " (run parseAOrElseB ( "CZZ"))
    printfn "%A " (run aAndThenBorC ( "ABZ"))
    printfn "%A " (run aAndThenBorC ( "ACZ"))
    printfn "%A " (run aAndThenBorC ( "QBZ"))
    printfn "%A " (run aAndThenBorC ( "AQZ"))
    printfn "%A " (run parseLowercase ( "aBC"))
    printfn "%A " (run parseLowercase ( "ABC"))
    printfn "%A " (run parseDigit ( "1ABC"))
    printfn "%A " (run parseDigit ( "9ABC"))
    printfn "%A " (run parseDigit ( "|ABC"))
    printfn "%A" (run parseThreeDigits ( "123A"))
    printfn "%A" (run parseThreeDigitsAsStr ( "123A"))
    printfn "%A" (run parseThreeDigitsAsInt ( "123A"))
    let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
    let combined = sequence parsers
    
    printfn "%A" (run combined ("ABCD"))
    
    let parseABC = pstring "ABC"
    printfn "%A" (run parseABC ( "ABCDE"))
    printfn "%A" (run parseABC ( "A|CDE"))
    printfn "%A" (run parseABC ( "AB|DE"))
    // test some success cases
    printfn "%A" (run manyA ( "ABCD"))
    printfn "%A" (run manyA ( "AACD"))
    printfn "%A" (run manyA ( "AAAD"))
    // test a case with no matches
    printfn "%A" (run manyA ( "|BCD"))
    
    let manyAB = many (pstring "AB")

    printfn "%A" (run manyAB ( "ABCD"))
    printfn "%A" (run manyAB ( "ABABCD"))
    printfn "%A" (run manyAB ( "ZCD"))
    printfn "%A" (run manyAB ( "AZCD"))
    
    let whitespaceChar = 
        let predicate = Char.IsWhiteSpace
        let label = "whitespace"
        satisfy predicate label
    let whitespace = many whitespaceChar
    
    printfn "%A" (run whitespace ( "ABC"))
    printfn "%A" (run whitespace ( " ABC"))
    printfn "%A" (run whitespace ( "\tABC"))
    
    // define parser for one or more digits
    let digits = many1 parseDigit 
    
    printfn "%A" (run digits ( "1ABC"))
    printfn "%A" (run digits ( "12BC"))
    printfn "%A" (run digits ( "123C"))
    printfn "%A" (run digits ( "1234"))
    
    printfn "%A" (run digits ( "ABC"))
    
    printfn "%A" (run pint ( "1ABC"))
    printfn "%A" (run pint ( "12BC"))
    printfn "%A" (run pint ( "123C"))
    printfn "%A" (run pint ( "1234"))
    
    printfn "%A" (run pint ( "ABC"))
    let parseDigitThenSemicolon = parseDigit .>> opt (pchar ';')
    printfn "%A" (run parseDigitThenSemicolon ( "1;"))
    printfn "%A" (run parseDigitThenSemicolon ( "1"))
    
    printfn "%A" (run pint ( "123C"))
    printfn "%A" (run pint ( "-123C"))
    
    let ab = pstring "AB"
    let cd = pstring "CD"
    let ab_cd = (ab .>> whitespace) .>>. cd
    
    printfn "%A" (run ab_cd ( "AB \t\nCD"))
    
    /// Keep only the result of the middle parser
    let between p1 p2 p3 =
        p1 >>. p2 .>> p3
    
    let pdoublequote = pchar '"'
    let quotedInteger = between pdoublequote pint pdoublequote
    
    printfn "%A" (run quotedInteger ( "\"1234\""))
    printfn "%A" (run quotedInteger ( "1234"))
    
    let comma = pchar ','    
    
    let zeroOrMoreDigitList = sepBy parseDigit comma
    let oneOrMoreDigitList = sepBy1 parseDigit comma
    
    printfn "%A" (run oneOrMoreDigitList ( "1;"))
    printfn "%A" (run oneOrMoreDigitList ( "1,2"))
    printfn "%A" (run oneOrMoreDigitList ( "1,2,3"))
    printfn "%A" (run oneOrMoreDigitList ( "Z;"))
    
    printfn "%A" (run zeroOrMoreDigitList ( "1;"))
    printfn "%A" (run zeroOrMoreDigitList ( "1,2"))
    printfn "%A" (run zeroOrMoreDigitList ( "1,2,3"))
    printfn "%A" (run zeroOrMoreDigitList ( "Z;"))

    let parseDigit_WithLabel =
        anyOf ['0'..'9']
        <?> "digit"
    run parseDigit_WithLabel ( "|ABC")
    |> printResult

    printfn "%A" (InputState.fromStr "" |> readAllChars)
    printfn "%A" (InputState.fromStr "a" |> readAllChars)
    //=> ['a'; '\010']
    printfn "%A" (InputState.fromStr "ab" |> readAllChars)
    //=> ['a'; 'b'; '\010']
    printfn "%A" (InputState.fromStr "a\nb" |> readAllChars)
    let exampleError =
        Failure ("identifier", "unexpected |",
            {currentLine = "123 ab|cd"; line=1; column=6})

    printResult exampleError

    let parseAB =
        pchar 'A' .>>. pchar 'B'
        <?> "AB"
    run parseAB "A|C"
    |> printResult
    0