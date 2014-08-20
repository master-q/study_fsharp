open System.IO
open FParsec

(* Datatype *)
type Album = { Name: string; Year: int }
type Song  = { Artist: string; Song: string; TimeInSec: int; Album: Album }
exception ParserError of string

(* Parse JSON *)
type Json = JString of string
          | JNumber of int
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
let jnumber = pint32 |>> JNumber
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

(* Map JSON to Album *)
let mapJAlbum json =
    let g name year =
        match name, year with
        | JString n, JNumber y -> { Name = n; Year = y }
        | _ -> raise (ParserError "Leaf should be the types.")
    let f a =
        match a with
        | JObject m -> g (Map.find "Name" m) (Map.find "Year" m)
        | _ -> raise (ParserError "Album list entry should be JObject.")
    match json with
    | JList l -> List.map f l
    | _ -> raise (ParserError "Json Album should be JList.")

let mapJSong albums json =
    let g artist song timeinsec album =
        match artist, song, timeinsec, album with
        | JString ar, JString so, JNumber ti, JString al ->
            { Artist = ar; Song = so; TimeInSec = ti; Album = List.find (fun x -> x.Name = al ) albums }
        | _ -> raise (ParserError "Leaf should be the types.")
    let f a =
        match a with
        | JObject m -> g (Map.find "Artist" m) (Map.find "Song" m) (Map.find "TimeInSec" m) (Map.find "Album" m)
        | _ -> raise (ParserError "Song list entry should be JObject.")
    match json with
    | JList l -> List.map f l
    | _ -> raise (ParserError "Json Song should be JList.")

let mapJsonToAlbum json =
    let f m =
         let a = Map.find "Album" m |> mapJAlbum
         let s = Map.find "Song" m |> mapJSong a
         (a, s)
    match json with
    | JObject m -> f m
    | _ -> raise (ParserError "Json top should be JObject.")

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
let sby = str "by" >>. ws >>. (sepBy1 (ws >>. sattributeop) (str ","))
let sorder = str "order" >>. ws >>. (sepBy1 (ws >>. sattribute) (str ",") |>> SesOrder)
let sshuffle = stringReturn "shuffle" SesShuffle
let ssort = sorder <|> sshuffle
let stop = str "top" >>. ws >>. pint32
let sselect = str "select" >>. ws >>. (sby .>> ws) .>>. (ssort .>> ws) .>>. stop

(* Main *)
let writeToFile (filename:string) (data:string list) =
  use sw = new StreamWriter(filename)
  data |> List.iter sw.WriteLine
  data.Length ;;

let lineOfFile (filename:string) =
  seq { use sr = new StreamReader(filename)
        while not sr.EndOfStream do
          yield sr.ReadLine()
      };;

let runinit file =
    let _ = writeToFile ".\init-music-box.cnf" [file]
    0

let runload file =
    let _ = writeToFile ".\load-music-box.cnf" [file]
    0

let getFirstLineOfFile file =
    let initSeq = lineOfFile file
    let byte = File.ReadAllBytes(Seq.head initSeq)
    System.Text.ASCIIEncoding.Default.GetString byte

let readInitConfig () =
    let str = getFirstLineOfFile ".\init-music-box.cnf"
    match run json str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> raise (ParserError errorMsg)

let readLoadConfig () =
    let str = getFirstLineOfFile ".\load-music-box.cnf"
    match run sselect str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> raise (ParserError errorMsg)

let rec filterBy selBy songs =
    let f by song =
        let ops = match fst by with
                  | SeoLt -> (<)
                  | SeoEq -> (=)
                  | _     -> (>)
        let opi = match fst by with
                  | SeoLt -> (<)
                  | SeoEq -> (=)
                  | _     -> (>)
        match snd by with
        | AttrName s      -> ops song.Album.Name s
        | AttrYear i      -> opi song.Album.Year i
        | AttrArtist s    -> ops song.Artist s
        | AttrSong s      -> ops song.Song s
        | AttrTimeInSec i -> opi song.TimeInSec i
    match selBy with
    | x :: xs -> List.filter (f x) songs |> filterBy xs
    | [] -> songs

let filterSongs sel songs =
    let selBy = fst sel |> fst
    let selSort = fst sel |> snd
    let selTop = snd sel
    filterBy selBy songs // |> filterSort selSort |> filterTop selTop

let runplay () =
    let dataJson = readInitConfig ()
    let dataAlbum = mapJsonToAlbum dataJson
    printfn "%A" dataAlbum
    let dataSelect = readLoadConfig ()
    printfn "%A" dataSelect
    let result = filterSongs dataSelect (snd dataAlbum)
    printfn "%A" result
    0

[<EntryPoint>]
let main argv = 
    match argv with
    | [| "--init" ; file |] -> runinit file
    | [| "--load" ; file |] -> runload file
    | [| "--play" |]        -> runplay ()
    | _                     -> printfn "*** Unknown command option."; 1
