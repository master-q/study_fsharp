open System.IO

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let byte = File.ReadAllBytes(argv.[0])
    let str = System.Text.ASCIIEncoding.Default.GetString byte
    printf "%s" str
    0 // return an integer exit code
