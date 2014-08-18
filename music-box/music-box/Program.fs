open System.IO
open FParsec

(* Datatype *)
type Album = { Name: string; Year: int }
type Song  = { Artist: string; Song: string; TimeInSec: int; Album: Album }

(* Parse JSON *)
type Json = JString of string
          | JNumber of float
          | JBool   of bool
          | JNull
          | JList   of Json list
          | JObject of Map<string, Json>

let str s = pstring s
let ws = spaces
let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself
    let unicodeEscape =
        /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9
        str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )
    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
    between (str "\"") (str "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
            (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

let jstring = stringLiteral |>> JString
let jnull = stringReturn "null" JNull
let jbool =     (stringReturn "true"  (JBool true))
            <|> (stringReturn "false" (JBool false))
let jnumber = pfloat |>> JNumber
let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()
let jlist = listBetweenStrings "[" "]" jvalue JList
let keyValue = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring 
                        jnumber
                        jbool
                        jnull]

let json = ws >>. jvalue .>> ws .>> eof

(* Parse Playlist *)
type Attribute = AttrName of string
               | AttrYear of int 
               | AttrArtist of string
               | AttrSong of string
               | AttrTimeInSec of int
               | AttrAlbum of string
type SeOp = SeoLt
          | SeoEq
          | SeoGt
type SeSort = SesOrder of Attribute list
            | SesShuffle
type Sby = (SeOp * Attribute) list

let sop =     stringReturn "<" SeoLt
          <|> stringReturn "=" SeoEq
          <|> stringReturn ">" SeoGt
let sattributeop =     (str "Name"      >>. ws >>. (sop .>> ws) .>>. (stringLiteral |>> AttrName))
                   <|> (str "Year"      >>. ws >>. (sop .>> ws) .>>. (pint32        |>> AttrYear))
                   <|> (str "Artist"    >>. ws >>. (sop .>> ws) .>>. (stringLiteral |>> AttrArtist))
                   <|> (str "Song"      >>. ws >>. (sop .>> ws) .>>. (stringLiteral |>> AttrSong))
                   <|> (str "TimeInSec" >>. ws >>. (sop .>> ws) .>>. (pint32        |>> AttrTimeInSec))
                   <|> (str "Album"     >>. ws >>. (sop .>> ws) .>>. (stringLiteral |>> AttrAlbum))
let sattribute =     (stringReturn "Name"      (AttrName ""))
                 <|> (stringReturn "Year"      (AttrYear 0))
                 <|> (stringReturn "Artist"    (AttrArtist ""))
                 <|> (stringReturn "Song"      (AttrSong ""))
                 <|> (stringReturn "TimeInSec" (AttrTimeInSec 0))
                 <|> (stringReturn "Album"     (AttrAlbum ""))
let sby = str "by" >>. ws >>. (sepBy1 sattributeop (str ","))
let sorder = str "order" >>. ws >>. (sepBy1 sattribute (str ",") |>> SesOrder)
let sshuffle = stringReturn "shuffle" SesShuffle
let ssort = sorder <|> sshuffle
let stop = str "top" >>. ws >>. pint32
let sselect = str "select" >>. sby .>>. ssort .>>. stop

(* Main *)
let runinit file =
    let byte = File.ReadAllBytes(file)
    let str = System.Text.ASCIIEncoding.Default.GetString byte
    match run json str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0

let runload file =
    let byte = File.ReadAllBytes(file)
    let str = System.Text.ASCIIEncoding.Default.GetString byte
    match run sselect str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    0

let runplay =
    0

[<EntryPoint>]
let main argv = 
    match argv with
    | [| "--init" ; file |] -> runinit file
    | [| "--load" ; file |] -> runload file
    | [| "--play" |]        -> runplay
    | _                     -> printfn "*** Unknown command option."; 1
