module parse

open elem
open library
open config

let parseData (e : Elm) =
    match e.elem with
    | Regexp "^\((\w)(\d+)\)\s*\[(\d+)\](\".*\")" [place;size;width;values] -> 
        e.t <- DATA; e.s <- CONSTARRAY; e.sz <- (sizecvt size); e.p <- (placecvt place); e.v1 <- values
    | Regexp @"^\((\w)(\d+)\)\s*\[(\d+)\]({.*})" [place;size;width;values] -> 
        e.t <- DATA; e.s <- CONSTARRAY; e.sz <- (sizecvt size); e.p <- (placecvt place); e.v1 <- values
    | Regexp @"^\((\w)(\d+)\)\s*\[(\d+)\]" [place;size;width] -> 
        e.t <- DATA; e.s <- ARRAY; e.sz <- (sizecvt size); e.v1 <- width; e.p <- (placecvt place)
    | Regexp "^\((\w)(\d+)\)(\".*\")" [place;size;value] -> 
        e.t <- DATA; e.s <- CONST; e.sz <- (sizecvt size); e.v1 <- value; e.p <- (placecvt place)
    | Regexp @"^\((\w)(8)\)(\'.\'})" [place;size;value] -> 
        e.t <- DATA; e.s <- CONST; e.sz <- (sizecvt size); e.v1 <- (value.Chars(0) |> int) |> string; e.p <- (placecvt place)
    | Regexp @"^\((\w)(\d+)\)({.*})" [place;size;value] -> 
        e.t <- DATA; e.s <- CONST; e.sz <- (sizecvt size); e.v1 <- value; e.p <- (placecvt place)
    | Regexp @"^\((\w)(\d+)\)" [place;size] -> 
        e.t <- DATA; e.s <- VAR; e.sz <- (sizecvt size); e.p <- (placecvt place)
    | _ -> let a,b,c = e.source; 
           failwith ("Error: bad data syntax "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
    e

let parseLabel (e : Elm) =
    match e.elem with
    | Regexp @"^([A-Za-z][A-Za-z0-9\._]*)@([A-Fa-f0-9xX]+):$" [labelname;labeladdress] -> 
        e.t <- LABEL; e.s <- WITHADDRESS; e.v1 <- labelname; e.ev1 <- labeladdress 
    | Regexp @"^([A-Za-z][A-Za-z0-9\._]*):$" [labelname] -> 
        e.t <- LABEL; e.s <- SIMPLE; e.v1 <- labelname
    | _ -> let a,b,c = e.source; 
           failwith ("Error: bad label syntax "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
    e

let ParseParameter( e : Elm, p : string) : Addressing*string*string*string =
    if target = "stm8s" then stm8.ParseParameter(e,p)
    elif target = "stm8l" then stm8.ParseParameter(e,p)
    elif target = "avr" then avr.ParseParameter(e,p)
    elif target = "msp430" then msp430.ParseParameter(e,p)
    else failwith ("Error: unknown target '"+target+"'. Valid values stm8l, stm8s, avr, msp430")

let noParameterOp (e : Elm) (o : Op) : Elm = e.t <- CODE; e.p <- Place.FLASH; e.n <- o; e

let oneParameterOp (e : Elm) (s : string) (o : Op) : Elm = 
    let m,b,v,ev = ParseParameter(e,s)
    e.t <- CODE; e.p <- Place.FLASH; e.n <- o; e.m1 <- m; e.b1 <- b; e.v1 <- v; e.ev1 <- ev
    e

let twoParameterOp (e : Elm) (l : string) (r : string) (o : Op) : Elm = 
    let m1,b1,v1,ev1 = ParseParameter(e,l)
    let m2,b2,v2,ev2 = ParseParameter(e,r)
    e.t <- CODE; e.p <- Place.FLASH; e.n <- o; e.m1 <- m1; e.b1 <- b1; e.v1 <- v1; e.ev1 <- ev1; e.m2 <- m2; e.b2 <- b2; e.v2 <- v2; e.ev2 <- ev2
    e

let parseCode (e : Elm) =
    let o = match e.elem with
            | Regexp @"^iret$" [] -> noParameterOp e IRET
            | Regexp @"^ret$" [] -> noParameterOp e RET
            | Regexp @"^retfar$" [] -> noParameterOp e RETF
            | Regexp @"^trap$" [] -> noParameterOp e TRAP
            | Regexp @"^halt$" [] -> noParameterOp e HALT 
            | Regexp @"^waiti$" [] -> noParameterOp e WAITI 
            | Regexp @"^wait$" [] -> noParameterOp e WAIT
            | Regexp @"^nop" [] -> noParameterOp e NOP
            | Regexp @"^break" [] -> noParameterOp e BREAK
            | Regexp @"^ifc=1(.+)$" [variable] -> oneParameterOp e variable IFC1
            | Regexp @"^if=0(.+)$" [variable]
            | Regexp @"^ifz=1(.+)$" [variable] -> oneParameterOp e variable IFZ1
            | Regexp @"^iffalse(.+)$" [variable] -> oneParameterOp e variable IFF
            | Regexp @"^ifh=1(.+)$" [variable] -> oneParameterOp e variable IFH1
            | Regexp @"^ifint=1(.+)$" [variable] -> oneParameterOp e variable IFINT1
            | Regexp @"^ifint=0(.+)$" [variable] -> oneParameterOp e variable IFINT0
            | Regexp @"^ifi=1(.+)$" [variable] -> oneParameterOp e variable IFI1
            | Regexp @"^if<0(.+)$" [variable]
            | Regexp @"^ifn=1(.+)$" [variable] -> oneParameterOp e variable IFN1
            | Regexp @"^ifc=0(.+)$" [variable] -> oneParameterOp e variable IFC0
            | Regexp @"^if<>0(.+)$" [variable]
            | Regexp @"^ifz=0(.+)$" [variable] -> oneParameterOp e variable IFZ0
            | Regexp @"^ifh=0(.+)$" [variable] -> oneParameterOp e variable IFH0
            | Regexp @"^ifi=0(.+)$" [variable] -> oneParameterOp e variable IFI0
            | Regexp @"^ifv=0(.+)$" [variable] -> oneParameterOp e variable IFV0
            | Regexp @"^ifv=1(.+)$" [variable] -> oneParameterOp e variable IFV1
            | Regexp @"^ift=0(.+)$" [variable] -> oneParameterOp e variable IFT0
            | Regexp @"^ift=1(.+)$" [variable] -> oneParameterOp e variable IFT1
            | Regexp @"^if>=0(.+)$" [variable]
            | Regexp @"^ifn=0(.+)$" [variable] -> oneParameterOp e variable IFN0
            | Regexp @"^if>=(.+)$" [variable] -> oneParameterOp e variable IFSGE
            | Regexp @"^if>(.+)$" [variable] -> oneParameterOp e variable IFSG
            | Regexp @"^if<=(.+)$" [variable] -> oneParameterOp e variable IFSLE
            | Regexp @"^if<(.+)$" [variable] -> oneParameterOp e variable IFSL
            | Regexp @"^iftrue(.+)$" [variable] -> oneParameterOp e variable IFT
            | Regexp @"^ifu>=(.+)$" [variable] -> oneParameterOp e variable IFUGE
            | Regexp @"^ifu>(.+)$" [variable] -> oneParameterOp e variable IFUG
            | Regexp @"^ifu<=(.+)$" [variable] -> oneParameterOp e variable IFULE
            | Regexp @"^ifu<(.+)$" [variable] -> oneParameterOp e variable IFUL
            | Regexp @"^if(.+)\=1(.+)$" [left;right] ->  twoParameterOp e left right IFBT
            | Regexp @"^if(.+)\<\>0(.+)$" [left;right] ->  twoParameterOp e left right IFBTF
            | Regexp @"^if(.+)\=0(.+)$" [left;right] ->  twoParameterOp e left right IFBF
            | Regexp @"^skip(.+)\=(.+)$" [left;right] ->  twoParameterOp e left right SKIP
            | Regexp @"^push(.+)$" [variable] -> oneParameterOp e variable PUSH
            | Regexp @"^pop(.+)$" [variable] ->  oneParameterOp e variable POP
            | Regexp @"^swap(.+)$" [variable] -> oneParameterOp e variable SWAP
            | Regexp @"^\+\+(.+)$" [variable]
            | Regexp @"^(.+)\+\+$" [variable] -> oneParameterOp e variable INC
            | Regexp @"^\-\-(.+)$" [variable]
            | Regexp @"^(.+)\-\-$" [variable] -> oneParameterOp e variable DEC
            | Regexp @"^\-{1}(.+)$" [variable] -> oneParameterOp e variable NEG
            | Regexp @"^\!{1}(.+)$" [variable] -> oneParameterOp e variable NOT
            | Regexp @"^\?{1}(.+)$" [variable] -> oneParameterOp e variable TEST
            | Regexp @"^c\<(.+)\<c$" [variable] -> oneParameterOp e variable SHL
            | Regexp @"^c\>(.+)\>c$" [variable] -> oneParameterOp e variable SHR
            | Regexp @"^c\<(.+)\<0$" [variable] -> oneParameterOp e variable SHL0
            | Regexp @"^0\>(.+)\>c$" [variable] -> oneParameterOp e variable SHR0
            | Regexp @"^s\>(.+)\>c$" [variable] -> oneParameterOp e variable SHRS
            | Regexp @"^a\<(.+)\<a$" [variable] -> oneParameterOp e variable SHLA
            | Regexp @"^a\>(.+)\>a$" [variable] -> oneParameterOp e variable SHRA
            | Regexp @"^gonear([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable GON
            | Regexp @"^gofar([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable GOF
            | Regexp @"^go([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable GO
            | Regexp @"^callnear([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable CALLN
            | Regexp @"^callfar([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable CALLF
            | Regexp @"^call([A-Za-z0-9\._]+)$" [variable] -> oneParameterOp e variable CALL         
            | Regexp @"^(.+)\+\=c\+(.+)$" [left;right] ->  twoParameterOp e left right ADC
            | Regexp @"^(.+)\+\=(.+)$" [left;right] ->  twoParameterOp e left right ADD
            | Regexp @"^(.+)\-\=c\+(.+)$" [left;right] ->  twoParameterOp e left right SBC
            | Regexp @"^(.+)\-\=(.+)$" [left;right] ->  twoParameterOp e left right SUB
            | Regexp @"^(.+)\*\=(.+)$" [left;right] ->  twoParameterOp e left right MUL
            | Regexp @"^a=(.+)\*(.+)$" [left;right] ->  twoParameterOp e left right MUL
            | Regexp @"^(.+)\/\=(.+)$" [left;right] ->  twoParameterOp e left right DIV
            | Regexp @"^(.+)\&\=(.+)$" [left;right] ->  twoParameterOp e left right AND
            | Regexp @"^(.+)\|\=(.+)$" [left;right] ->  twoParameterOp e left right OR
            | Regexp @"^(.+)\^\=(.+)$" [left;right] ->  twoParameterOp e left right XOR
            | Regexp @"^(.+)\<\-\>(.+)$" [left;right] ->  twoParameterOp e left right CHANGE
            | Regexp @"^(.+)\=(.+)$" [left;right] ->  twoParameterOp e left right TO
            | Regexp @"^(.+)\&\?(.+)$" [left;right] -> twoParameterOp e left right ANDTEST
            | Regexp @"^(.+)\?(.+)$" [left;right] -> twoParameterOp e left right SUBTEST
            | _ ->  let a,b,c = e.source; 
                    failwith ("Error: Unknown assembler instruction "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
    e

let parseAsmCommand (e : Elm) = 
    if e.elem.IndexOf("inline") = 0 then 
        e.t <- CODE; e.p <- Place.FLASH; e.n <- INLINE; e.res <- e.elem.Substring(6).Trim(); e
    elif e.elem.IndexOf('(') = 0 then (parseData e) 
    elif e.elem.IndexOf(':') = e.elem.Length-1 then (parseLabel e)
    else (parseCode e)


let parse (elems : seq<Elm>) = [ for e in elems do yield parseAsmCommand e ]
