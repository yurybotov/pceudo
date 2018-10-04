module inputstrings

open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open config
open library
open filefinder

type Deftype = 
    | SIMPLE 
    | LINE 
    | FUNC

type Definition = { parameters : string; body : string; deftype: Deftype }

let definitions = new Dictionary<string,Definition>()

let replaceLocalLabels s n = Regex.Replace(s, @"\@(\d+)", "local_"+n+"_$1" )
     
let convertString (line : string, filename : string, linenum : int, code : string): string =  
    let mutable outline = line
    for item in definitions do
                    match item.Value.deftype with
                    | LINE ->   outline <- outline.Replace(item.Key,item.Value.body)
                    | FUNC ->   let mutable o = item.Value.body
                                let paramnames = safeSplit item.Value.parameters ","
                                let template = @"(.*)" + item.Key + @"\((.*)\)(.*)"
                                let m = Regex.Match(outline, template)
                                if m.Success then 
                                    let paramvalues = safeSplit (m.Groups.Item(2).ToString()) ","
                                    if paramvalues.Length <> paramnames.Length then 
                                        failwith ("Error: bad parameters count "+filename+" at line " + linenum.ToString() + " : '"+code+"'")
                                    if paramnames.[0].Length > 0 then
                                        for i in 0..paramvalues.Length-1 do
                                            o <- o.Replace(paramnames.[i],paramvalues.[i])
                                            outline <- replaceLocalLabels (m.Groups.Item(1).ToString() + o + (m.Groups.Item(3).ToString())) (i.ToString()+"_"+linenum.ToString())
                                    else outline <- replaceLocalLabels (m.Groups.Item(1).ToString() + o + (m.Groups.Item(3).ToString())) ("_"+linenum.ToString())
                    | SIMPLE -> ()
    outline.Trim()

let searchPairedEndif (reader : StreamReader) (filename : string) (line : int) (code : string) =
    let mutable counter = 1
    let mutable line = ""
    while counter > 0 do
        if reader.EndOfStream then failwith ("Error: no paired endif for "+filename+" at line "+line+" : '"+code+"'")
        line <- reader.ReadLine ()
        match line with
        | Regexp @"^\s*ifdef\s+[A-Za-z][A-Za-z0-9\.]*" []
        | Regexp @"^\s*ifndef\s+[A-Za-z][A-Za-z0-9\.]*" [] -> counter <- counter + 1
        | Regexp @"^\s*endif\s*" [] -> counter <- counter - 1
        | _ -> ()

let longDefineComposer (value : string) (reader : StreamReader) (filename : string) (line : int) (code : string) =
    let mutable notdone = true 
    let mutable line = ""
    let mutable outstring = value
    while notdone do
        if reader.EndOfStream then failwith ("Error: Strange EOF in long define "+filename+" at line "+line+" : '"+code+"'")
        line <- reader.ReadLine ()
        match line with
        | Regexp @"^(.+)\\\s*" [nextline] -> outstring <- outstring + nextline.Trim()
        | _ -> outstring <- outstring + line.Trim(); notdone <- false
    outstring

let addDef ( name : string, value : Definition) = 
    let _ = if definitions.ContainsKey(name) then definitions.[name] <- value else definitions.Add(name,value)
    match name with
    | "regstart" -> regstart <- value.body |> int
    | "regend" -> regend <- value.body |> int
    | "ramstart" -> ramstart <- value.body |> int
    | "ramend" -> ramend <- value.body |> int
    | "eepromstart" -> eepromstart <- value.body |> int
    | "eepromend" -> eepromend <- value.body |> int
    | "flashstart" -> flashstart <- value.body |> int
    | "flashend" -> flashend <- value.body |> int
    | "hiflashstart" -> hiflashstart <- value.body |> int
    | "hiflashend" -> hiflashend <- value.body |> int
    | _ -> ()

let rec readLines (filePath:string) = 
     [
        let mutable linenumber = 0
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            let line = sr.ReadLine ()
            match line with
            | Regexp @"^\s*\/\/.*$" [] -> ()
            | Regexp @"^\s*(.+)\/\/.*$" [beforecomment] -> 
                yield (filePath,linenumber,convertString(beforecomment.Trim(),filePath,linenumber,line).Trim())
            | Regexp @"^\s*define\s+([A-Za-z][A-Za-z0-9\.]*)\((.*)\)\s+(.+)\s*\\" [name;param;value] -> 
                addDef(name.Trim(), { parameters = param.Trim(); body = (longDefineComposer value sr filePath linenumber line); deftype = FUNC }); ()
            | Regexp @"^\s*define\s+([A-Za-z][A-Za-z0-9\.]*)\((.*)\)\s+(.+)" [name;param;value] -> 
                addDef(name.Trim(),{ parameters = param.Trim(); body = value.Trim(); deftype = FUNC }); ()
            | Regexp @"^\s*define\s+([A-Za-z][A-Za-z0-9\.]*)\s+(.+)\s*\\" [name;value] -> 
                addDef(name.Trim(), { parameters = ""; body = (longDefineComposer (value.Trim()) sr filePath linenumber (line.Trim())); deftype = LINE }); ()
            | Regexp @"^\s*define\s+([A-Za-z][A-Za-z0-9\.]*)\s+(.+)" [name;value] -> 
                addDef(name.Trim(), { parameters = ""; body = value.Trim(); deftype = LINE }); ()
            | Regexp @"^\s*define\s+([A-Za-z][A-Za-z0-9\.]*)" [name] -> 
                addDef(name.Trim(),{ parameters = ""; body = ""; deftype = SIMPLE }); ()
            | Regexp @"^\s*undefine\s+([A-Za-z][A-Za-z0-9\.]*)" [name] -> if name <> null then definitions.Remove(name.Trim()) |> ignore
            | Regexp @"^\s*ifdef\s+([A-Za-z][A-Za-z0-9\.]*)" [name] -> 
                if name <> null && not (definitions.ContainsKey(name.Trim())) then searchPairedEndif sr filePath linenumber line; ()
            | Regexp @"^\s*ifndef\s+([A-Za-z][A-Za-z0-9\.]*)" [name] -> 
                if name <> null && definitions.ContainsKey(name.Trim()) then searchPairedEndif sr filePath linenumber line; ()
            | Regexp @"^\s*endif\s*" [] -> ()
            | Regexp @"^\s*include\s+(.+)" [filename] -> yield! readLines (finder filename filePath linenumber line)
            | _ -> if line.Trim() <> "" then yield (filePath,linenumber,convertString(line.Trim(),filePath,linenumber,line)) else ()
            
            linenumber <- linenumber + 1
        printfn "done\t%i\tlines in\t%s" linenumber filePath
    ]
