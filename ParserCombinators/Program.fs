open System

/// Type that represents Success/Failure in parsing
type ParseResult<'a> =
  | Success of 'a
  | Failure of string

/// Type that wraps a parsing function
type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

/// Parser which return result 'x' without changing input string
let constant x = Parser (fun s -> Success(x, s))

/// Parse a single character
let pchar charToMatch =
  // define a nested inner function
  let innerFn str =
    if String.IsNullOrEmpty(str) then
      Failure "No more input"
    else
      let first = str.[0]
      if first = charToMatch then
        let remaining = str.[1..]
        Success (charToMatch,remaining)
      else
        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
        Failure msg
  // return the "wrapped" inner function
  Parser innerFn



/// Run a parser with some input
let run parser input =
  // unwrap parser to get inner function
  let (Parser innerFn) = parser
  // call inner function with input
  innerFn input

/// Bind for parsers
let bind parser f =
  let innerFn input =
    // run parser with the input
    let result = run parser input

    // test the result for Failure/Success
    match result with
    | Failure err ->
      // return error from parser
      Failure err

    | Success (value, remaining) ->
      run (f value) remaining

  // return the inner function
  Parser innerFn

/// Infix version of bind
let (>>=) = bind

/// Combine two parsers as "A andThen B"
let andThen parser1 parser2 = 
  bind parser1 (
    fun r1 -> 
    bind parser2 (
      fun r2 -> 
      constant (r1, r2)))

/// Infix version of andThen
let (>>) = andThen

/// Combine two parsers as "A orElse B"
let orElse parser1 parser2 =
  let innerFn input =
    // run parser1 with the input
    let result1 = run parser1 input

    // test the result for Failure/Success
    match result1 with
    | Success result ->
      // if success, return the original result
      result1

    | Failure err ->
      // if failed, run parser2 with the input
      let result2 = run parser2 input

      // return parser2's result
      result2

  // return the inner function
  Parser innerFn

/// Infix version of orElse
let ( <|> ) = orElse

/// Repeat 0 or more times for parser
let rec many p = (p >>= fun hd -> many p >>= fun tl -> constant (hd :: tl)) <|> constant []

/// Repeat 1 or more times for parser
let many1 p = p >>= fun hd -> many p >>= fun tl -> constant (hd :: tl)

/// Choose any of a list of parsers
let choice listOfParsers =
  List.reduce ( <|> ) listOfParsers

/// Choose any of a list of characters
let anyOf listOfChars =
  listOfChars
  |> List.map pchar // convert into parsers
  |> choice

/// Parser of one digit
let digit = anyOf ['0' .. '9']

/// Parser of number
let number = 
  many1 digit >>= 
    fun ds -> constant <| String.Concat(Array.ofList(ds))

/// Parser of letter
let letter = anyOf <| ['a' .. 'z'] @ ['A' .. 'Z']

/// Parser of identifier
let identifier =
  letter >>= fun fst -> 
    many (digit <|> letter <|> pchar '_') >>= fun rest -> 
      constant <| String.Concat(Array.ofList(fst :: rest))
