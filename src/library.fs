module library

open System.Text.RegularExpressions

// реализация Regex
let (|Regexp|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (=~=) line template = Regex.IsMatch(line, template)

// разбор строки
let safeSplit (s : string) (d : string) : string[] = if s.IndexOf(d) > -1 then s.Split(d.[0]) else [| s |]


