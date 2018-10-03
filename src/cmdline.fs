module cmdline

open System.Collections.Generic
open config

let isargname (a:string) = if a.Substring(0,2) = "--" then true else false
let ispresent (a:string) = if a <> "" then true else false

let getargs args =
    let out = new Dictionary<string,string>()
    let mutable prev = ""
    for a in args do
        match isargname a, ispresent prev with
        | true, false -> prev <- a
        | true, true -> out.Add(prev.Substring(2),""); prev <- a;
        | false, false -> out.Add("default",a);
        | false, true -> out.Add(prev.Substring(2),a); prev <- "";
    out

let printhelp =
    printfn "Use:"
    printfn "pseudo filename.fileext {--paramname paramvalue} {--paramname}"
    printfn "   filename : your pseudo-assembler file"
    printfn "   fileext  : file extention. It must be '.stm8s','stm8l','avr','msp430'"
    printfn "   parameters..."
    printfn "      --help : this information"
    printfn "      --sdk sdkpath : path to naivesdk directory - default value 'c:\'"
    printfn "      --inc incpath : addition path to ONE user include directory - default value absent,"
    printfn "                      if needs more include directories use: --inc first\ --inc second\ --inc third\ ..."
    printfn "Target assembler file will be 'filename.asm'"

let cmdlineparser args : string*string =
    let m = getargs args
    if m.Count = 0 then printhelp; failwith "Use right parameters"
    let mutable name = "";
    let mutable ext = "";
    for (KeyValue(k,v)) in m do
        match k with
        | "default" -> 
            let ptr = v.LastIndexOf('.');
            name <- v.Substring(0,ptr)
            ext <- v.Substring(ptr+1)
            target <- ext
        | "help" -> printhelp; failwith "Use right parameters"
        | "sdk" -> sdkroot <- v
        | "inc" -> inc <- v :: inc
        | "target" -> target <- v
        | _ -> printhelp; failwith "Use right parameters"
    (name,ext)