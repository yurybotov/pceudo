module splitlines

open elem
open library

let splitLines ( lines : list<string*int*string> ) =
    [
        for line in lines do
            let f,n,l = line
            let sublines = safeSplit l ";"
            for sub in sublines do
                if sub.Trim() <> "" then
                    let pairs = safeSplit (sub.Trim()) ":"
                    for i in 0..pairs.Length-1 do
                        if i <> pairs.Length-1 then
                            yield new Elm(f,n,l, pairs.[i].Trim().Replace(" ","")+":")   
                        else
                            if pairs.[i].Replace(" ","").Replace("\t","") <> "" then 
                                if pairs.[i].IndexOf("inline") > -1 
                                then yield new Elm(f,n,l, pairs.[i].Trim())
                                else yield new Elm(f,n,l, (pairs.[i].Trim().Replace(" ","").Replace("\t",""))) 
    ]