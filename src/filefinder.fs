module filefinder

open System.IO
open config

let finder (filename:string) (f:string) (n:int) (l:string) : string =
    let ext = target
    let mutable res =
        if File.Exists(filename) then filename
        elif File.Exists(filename+"."+ext) then (filename+"."+ext)
        elif File.Exists(@"inc\"+filename) then (@"inc\"+filename)
        elif File.Exists(@"inc\"+filename+"."+ext) then (@"inc"+filename+"."+ext)
        elif File.Exists(@"include\"+filename) then (@"include\"+filename)
        elif File.Exists(@"include\"+filename+"."+ext) then (@"include"+filename+"."+ext)
        elif File.Exists(@"lib\"+filename) then (@"lib\"+filename)
        elif File.Exists(@"lib\"+filename+"."+ext) then (@"lib"+filename+"."+ext)
        elif File.Exists(@"library\"+filename) then (@"library\"+filename)
        elif File.Exists(@"library\"+filename+"."+ext) then (@"library"+filename+"."+ext)
        elif File.Exists(sdkroot+ @"naivesdk\"+ext+ @"\library\"+filename) then (sdkroot+ @"naivesdk\"+ext+ @"\library\"+filename)
        elif File.Exists(sdkroot+ @"naivesdk\"+ext+ @"\library\"+filename+"."+ext) then (sdkroot+ @"naivesdk\"+ext+ @"\library\"+filename+"."+ext)
        elif File.Exists(sdkroot+ @"naivesdk\"+ext+ @"\targets\"+filename) then (sdkroot+ @"naivesdk\"+ext+ @"\targets\"+filename)
        elif File.Exists(sdkroot+ @"naivesdk\"+ext+ @"\targets\"+filename+"."+ext) then (sdkroot+ @"naivesdk\"+ext+ @"\targets\"+filename+"."+ext)
        elif File.Exists(sdkroot+ @"naivesdk\common\library\"+filename) then (sdkroot+ @"naivesdk\common\library\"+filename)
        elif File.Exists(sdkroot+ @"naivesdk\common\library\"+filename+"."+ext) then (sdkroot+ @"naivesdk\common\library\"+filename+"."+ext)
        else ""
    if res = "" && inc.Length > 0 then for i in inc do if File.Exists(i+filename) then res <- (i+filename)
    if res = "" then failwith ("Error: cant find file: "+filename+" in '"+ l + "' line "+n.ToString()+" of "+ f)       
    res