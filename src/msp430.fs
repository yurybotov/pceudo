module msp430

open library
open config
open elem


let drawmode (e: Elm) m b (vv : string) ev =
    let v = vv.Replace("0x","$").Replace("0X","$")
    match m,b,vv with
    | R1            ,_,_ -> b
    | R8            ,_,_ -> b
    | R16           ,_,_ -> b
    | IMMEDIATE     ,_,_ -> "#" + v
    | DIRECT        ,_,_ -> v
    | INDEXED       ,_,"0" -> "(" + b + ")"
    | INDEXED       ,_,_ -> "(" + v + "," + b + ")"
    | INDIRECT      ,_,"0" -> "[" + b + "]"
    | INDIRECT      ,_,_ -> "([" + v + "]," + b + ")"
    | BITOPERATION  ,_,_ -> v + ",#" + ev
    | _ -> let a,b,c = e.source; 
           failwith ("Error: bad addressing mode "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)

let noparam ( e : Elm ) : string = e.template
let oneparam ( e : Elm ) : string =  sprintf (Printf.StringFormat<string->string>(e.template)) (drawmode e e.m1 e.b1 e.v1 e.ev1)
let twoparam ( e : Elm ) : string =  sprintf (Printf.StringFormat<string->string->string>(e.template)) (drawmode e e.m1 e.b1 e.v1 e.ev1) (drawmode e e.m2 e.b2 e.v2 e.ev2)

let cmds : list<Elm>= [
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(NOP,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "nop",             noparam);
    new Elm(TRAP,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "trap",            noparam);
    new Elm(WAITI,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "wfi",             noparam);
    new Elm(WAIT,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "wfe",             noparam);
    new Elm(HALT,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "halt",            noparam);
    new Elm(RET,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "ret",             noparam);
    new Elm(RETF,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "retf",            noparam);
    new Elm(IRET,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "iret",            noparam);
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(NEG,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "negw %s",         oneparam);
    new Elm(NEG,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "neg %s",          oneparam);
    new Elm(DEC,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "decw %s",         oneparam);
    new Elm(DEC,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "dec %s",          oneparam);
    new Elm(INC,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "incw %s",         oneparam);
    new Elm(INC,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "inc %s",          oneparam);
    new Elm(PUSH,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "pushw %s",        oneparam);
    new Elm(PUSH,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "push %s",         oneparam);
    new Elm(POP,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "popw %s",         oneparam);
    new Elm(POP,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "pop %s",          oneparam);
    new Elm(SWAP,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "swapw %s",        oneparam);
    new Elm(SWAP,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "swap %s",         oneparam);
    new Elm(NEG,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "negw %s",         oneparam);
    new Elm(NEG,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "neg %s",          oneparam);
    new Elm(NOT,        R1,         "c",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "ccf",             noparam);
    new Elm(NOT,        BITOPERATION,"-1",  "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "bcpl %s",         oneparam);
    new Elm(NOT,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "cplw %s",         oneparam);
    new Elm(NOT,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "cpl %s",          oneparam);
    new Elm(TEST,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "tnzw %s",         oneparam);
    new Elm(TEST,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "tnz %s",          oneparam);
    new Elm(SHL,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rlcw %s",         oneparam);
    new Elm(SHL,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rlc %s",          oneparam);
    new Elm(SHR,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rrcw %s",         oneparam);
    new Elm(SHR,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rrc %s",          oneparam);
    new Elm(SHL0,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "sllw %s",         oneparam);
    new Elm(SHL0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "sll %s",          oneparam);
    new Elm(SHR0,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "srlw %s",         oneparam);
    new Elm(SHR0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "srl %s",          oneparam);
    new Elm(SHRS,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "sraw %s",         oneparam);
    new Elm(SHRS,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "sra %s",          oneparam);
    new Elm(SHLA,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rlwa %s,a",       oneparam);
    new Elm(SHRA,       R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rrwa %s,a",       oneparam);
    new Elm(GON,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jra %s",          oneparam);
    new Elm(GO,         XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jp %s",           oneparam);
    new Elm(GOF,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jpf %s",          oneparam);
    new Elm(CALLN,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "callr %s",        oneparam);
    new Elm(CALL,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "call %s",         oneparam);
    new Elm(CALLF,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "callf %s",        oneparam);
    new Elm(IFC1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrc %s",          oneparam);
    new Elm(IFZ1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jreq %s",         oneparam);
    new Elm(IFF,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrf %s",          oneparam);
    new Elm(IFH1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrh %s",          oneparam);
    new Elm(IFINT1,     XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrih %s",         oneparam);
    new Elm(IFINT0,     XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jril %s",         oneparam);
    new Elm(IFI1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrm %s",          oneparam);
    new Elm(IFN1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrmi %s",         oneparam);
    new Elm(IFC0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrnc %s",         oneparam);
    new Elm(IFZ0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrne %s",         oneparam);
    new Elm(IFH0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrnh %s",         oneparam);
    new Elm(IFI1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrnm %s",         oneparam);
    new Elm(IFV0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrnv %s",         oneparam);
    new Elm(IFV1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrv %s",          oneparam);
    new Elm(IFN0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrpl %s",         oneparam);
    new Elm(IFSGE,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrsge %s",        oneparam);
    new Elm(IFSG,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrsgt %s",        oneparam);
    new Elm(IFSLE,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrsle %s",        oneparam);
    new Elm(IFSL,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrslt %s",        oneparam);
    new Elm(IFT,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrt %s",          oneparam);
    new Elm(IFUGE,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jruge %s",        oneparam);
    new Elm(IFUG,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrugt %s",        oneparam);
    new Elm(IFULE,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrule %s",        oneparam);
    new Elm(IFUL,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jrilt %s",        oneparam);
    new Elm(IFBT,       BITOPERATION,"-1",  "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "btjt %s,%s",      twoparam);
    new Elm(IFBF,       BITOPERATION,"-1",  "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "btjf %s,%s",      twoparam);
    new Elm(IFBTF,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "tnz %s\n\tjreq %s", twoparam);
    new Elm(IFBF,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "tnz %s\n\tjrne %s", twoparam);
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(SUB,        R16,        "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "decw %s",        oneparam);
    new Elm(SUB,        XX,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "dec %s",         oneparam);
    new Elm(SUB,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "subw %s,%s",     twoparam);
    new Elm(SUB,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "sub %s,%s",      twoparam);
    new Elm(SBC,        R8,         "a",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "sbc %s,%s",      twoparam);
    new Elm(ADD,        R16,        "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "incw %s",        oneparam);
    new Elm(ADD,        XX,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "inc %s",         oneparam);
    new Elm(ADD,        R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "addw %s,%s",     twoparam);
    new Elm(ADD,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "add %s,%s",      twoparam);
    new Elm(ADC,        R8,         "a",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "adc %s,%s",      twoparam);
    new Elm(MUL,        R16,        "-1",   "-1",   "-1",   -1,   R8,       "a",    "-1",   "-1",   1,    "mul %s,%s",      twoparam);
    new Elm(DIV,        R16,        "-1",   "-1",   "-1",   -1,   R8,       "a",    "-1",   "-1",   1,    "div %s,%s",      twoparam);
    new Elm(DIV,        R16,        "x",    "-1",   "-1",   -1,   R16,      "y",    "-1",   "-1",   1,    "divw %s,%s",     twoparam);
    new Elm(AND,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "and %s,%s",      twoparam);
    new Elm(OR,         XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "or %s,%s",       twoparam);
    new Elm(XOR,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "xor %s,%s",      twoparam);
    new Elm(CHANGE,     R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "exgw %s,%s",     twoparam);
    new Elm(CHANGE,     XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "exg %s,%s",      twoparam);
    new Elm(TO,         BITOPERATION,"-1",  "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,    "bres %s",        oneparam);
    new Elm(TO,         BITOPERATION,"-1",  "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "bset %s",        oneparam);
    new Elm(TO,         R1,         "c",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,    "rcf",            noparam);
    new Elm(TO,         R1,         "c",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "scf",            noparam);
    new Elm(TO,         R1,         "i",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,    "rim",            noparam);
    new Elm(TO,         R1,         "i",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,    "sim",            noparam);
    new Elm(TO,         R1,         "v",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,    "rvf",            noparam);
    new Elm(TO,         XX,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,    "clr %s",         oneparam);
    new Elm(TO,         DIRECT,     "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",   "-1",   1,    "mov %s,%s",      twoparam);
    new Elm(TO,         DIRECT,     "-1",   "-1",   "-1",   -1,   DIRECT,   "-1",   "-1",   "-1",   1,    "mov %s,%s",      twoparam);
    new Elm(TO,         R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "ldw %s,%s",      twoparam);
    new Elm(TO,         R8,         "a",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "ld %s,%s",       twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,    "ld %s,%s",       twoparam);
    new Elm(TO,         XX,         "-1",   "-1",   "-1",   -1,   R16,      "-1",   "-1",   "-1",   1,    "ldw %s,%s",      twoparam);
    new Elm(TO,         XX,         "-1",   "-1",   "-1",   -1,   R8,       "a",    "-1",   "-1",   1,    "ld %s,%s",       twoparam);
    new Elm(ANDTEST,    XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "bcp %s,%s",      twoparam);
    new Elm(SUBTEST,    R16,        "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "cpw %s,%s",      twoparam);
    new Elm(SUBTEST,    XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,    "cp %s,%s",       twoparam);
]

let code (opcode : Elm) : string  =
    let mutable o : string = ""
    for op in cmds do
        if  op.n = opcode.n &&
            (op.m1 = opcode.m1 || op.m1 = XX) &&
            (op.b1 = opcode.b1 || op.b1 = "-1") &&
            (op.v1 = opcode.v1 || op.v1 = "-1") &&
            (op.ev1 = opcode.ev1 || op.ev1 = "-1") &&
            (op.m2 = opcode.m2 || op.m2 = XX) &&
            (op.b2 = opcode.b2 || op.b2 = "-1") &&
            (op.v2 = opcode.v2 || op.v2 = "-1") &&
            (op.ev2 = opcode.ev2 || op.ev2 = "-1")
        then
            let op2 : Elm = new Elm(opcode.source,opcode.elem,op.t,opcode.s,opcode.p,opcode.sz,op.n,opcode.m1,opcode.b1,opcode.v1,opcode.ev1,opcode.l1,
                                    opcode.m2,opcode.b2,opcode.v2,opcode.ev2,opcode.l2,op.template,op.prn,"")
            if o = "" then o <- op2.prn op2
    if o = "" then  let a,b,c = opcode.source; 
                    failwith ("Error: bad opcode "+opcode.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
    o

let ParseParameter( e : Elm, p : string) : Addressing*string*string*string =
    match p with
    | Regexp @"^(v|i|h|n|z|c)$" [b] -> (R1,b,"","")
    | Regexp @"^(a|xl|xh|yl|yh|cc)$" [r] -> (R8,r,"","")
    | Regexp @"^(x|y|sp)$" [r] -> (R16,r,"","")
    | Regexp @"^(0x[0-9a-fA-F]+|0X[0-9a-fA-F]+)$" [d] -> (IMMEDIATE,"",d,"")
    | Regexp @"^([0-9]+)$" [d] -> (IMMEDIATE,"",d,"")
    | Regexp @"^([A-Za-z0-9\.]+)\#([0-8])$" [a;b] -> (BITOPERATION,"",a,b)    
    | Regexp @"^\[([A-Za-z0-9\.]+)\]\[(x|y|sp)\]\]$" [e;r] -> (INDIRECT,r,e,"")
    | Regexp @"^\[(x|y|sp)\]$" [r] -> (INDEXED,r,"0","")
    | Regexp @"^([A-Za-z0-9\.]+)\[(x|y|sp)\]$" [e;r] -> (INDEXED,r,e,"")
    | Regexp @"^\[([A-Za-z0-9\.]+)\]$" [e] -> (INDIRECT,"",e,"")
    | Regexp @"^([A-Za-z0-9\.]+)$" [i] -> (DIRECT,"",i,"")
    | _ ->  let a,b,c = e.source; 
            failwith ("Error: Unknown addressing mode in '"+c+"' at "+(b |> string)+" line of "+a);

let prefix : string =
   ""

let postfix  =
    "\n\tEND\n\nFile composed by PCEUDO"

let mutable currentseg = ""

let p2s ( p : Place) = match p with | REGRAM -> "reg" | RAM -> "ram" | EEPROM -> "eeprom" | FLASH -> "flash" | _ -> "unify"
let segtype seg = match seg with | "reg" -> "DATA" | "ram" -> "DATA" | "eeprom" -> "DATA" | "flash" -> "CODE" | _ -> "UNIFY"
let lentype len = match len with | 1 -> ".b" | 2 -> ".w" | 4 -> ".l" | _ -> failwith "Error: bad variable size"
let countifneed cnt = if cnt < 2 then "" else cnt |> string
let i2hex i = sprintf "%X" i

let generateSimpleLabel seg name = 
    if currentseg <> seg then currentseg <- seg; seg+"\tsegment\t'"+(segtype seg)+"'\n" else "" + name + ":\n"
let generateAddressLabel seg name addr = 
    currentseg <- seg; seg+"\tsegment byte at: "+(i2hex addr)+"\t'"+(segtype seg)+ "'\n" + name + ":\n"

let generateDataPlace seg width count = 
    if currentseg <> seg then currentseg <- seg; seg+"\tsegment  '"+(segtype seg)+"'\n" else "" + "\tds"+(lentype width)+" "+(countifneed count)+"\n"
let generateData seg width (data : string) = 
    if currentseg <> seg then currentseg <- seg; seg+"\tsegment  '"+(segtype seg)+"'\n" else "" + "\tdc"+(lentype width)+"\t"+(data.Replace("0x","$").Replace("0X","$"))+"\n"

let generateCode cmd = if currentseg <> "flash" then currentseg <- "flash"; "flash\tsegment\t'CODE'\n\t" else "\t" + cmd+"\n"

let mutable haslabel = false
let mutable labelname = ""
let mutable labeladdress = ""

let generate (elem : Elm list) : Elm list =
    [
    for e in elem do
        match e.t with
        | LABEL -> match e.s with
                   | SIMPLE -> haslabel <- true; labelname <- e.v1; labeladdress <- ""
                   | WITHADDRESS -> haslabel <- true; labelname <- e.v1; labeladdress <- e.ev1
                   | _ -> failwith "Error: Bad record subtype"
        | DATA ->  match e.s with
                   | CONST ->      if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateData (p2s e.p) e.sz e.v1;
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateData (p2s e.p) e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateData (p2s e.p) e.sz e.v1
                   | CONSTARRAY -> if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateData (p2s e.p) e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; ()
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateData (p2s e.p) e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateData (p2s e.p) e.sz e.v1
                   | VAR ->        if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateDataPlace (p2s e.p) e.sz 1; 
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateDataPlace (p2s e.p) e.sz 1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <-generateDataPlace (p2s e.p) e.sz 1
                   | ARRAY ->      if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateDataPlace (p2s e.p) e.sz (e.v1|>int); 
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateDataPlace (p2s e.p) e.sz (e.v1|>int); 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateDataPlace (p2s e.p) e.sz (e.v1|>int)
                   | _ -> failwith "Error: Bad record subtype"
        | CODE ->   if haslabel 
                    then if labeladdress = "" 
                         then e.res <- generateSimpleLabel (p2s e.p) labelname + generateCode (code e);
                                       haslabel <- false; labelname <- "";
                         else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateCode (code e);
                                       haslabel <- false; labelname <- ""; labeladdress <- ""
                    else e.res <- generateCode (code e);
        | _ -> failwith "Error: Bad record type"
        yield e
    ]

let printcode (e : Elm list) =
    let mutable o = prefix
    for i in e do 
        o <- o + i.res
    o <- o + postfix
    o