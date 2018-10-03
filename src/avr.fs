module avr

open library
open config
open elem


let drawmode (e:Elm) m b (vv : string) ev =
    let v = vv
    match m,b,vv with
    | R1            ,_,_ -> b
    | R8            ,_,_ -> b
    | R16           ,_,_ -> b
    | IMMEDIATE     ,_,_ -> v
    | DIRECT        ,_,_ -> v
    | INDEXED       ,_,"0" -> b
    | INDEXED       ,_,_ -> b + "+" + v 
    | INDIRECT      ,_,"0" -> b
    | INDIRECT      ,_,_ -> b + "+" + v
    | BITOPERATION  ,_,_ -> v + "," + ev
    | REGBIT       ,_,_ -> v + "," + ev
    | PORTBIT       ,_,_ -> v + "," + ev
    | PORT          ,_,_ -> v
    | PREDEC        ,_,_ -> "-" + b
    | POSTINC       ,_,_ -> b + "+"
    | _ -> let a,b,c = e.source; 
           failwith ("Error: bad addressing mode "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)

let noparam ( e : Elm ) : string = e.template
let oneparam ( e : Elm ) : string =  sprintf (Printf.StringFormat<string->string>(e.template)) (drawmode e e.m1 e.b1 e.v1 e.ev1)
let twoparam ( e : Elm ) : string =  sprintf (Printf.StringFormat<string->string->string>(e.template)) (drawmode e e.m1 e.b1 e.v1 e.ev1) (drawmode e e.m2 e.b2 e.v2 e.ev2)
let inlineop ( e : Elm ) : string = e.res

/// ????  mul(other types), cpc,  elpm, wdr, bset,bclr - use inline!

let cmds : list<Elm>= [
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(NOP,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "nop",             noparam);
    new Elm(BREAK,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "break",           noparam);
    new Elm(WAIT,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "sleep",           noparam);
    new Elm(RET,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "ret",             noparam);
    new Elm(IRET,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",  -1,   "reti",            noparam);
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(DEC,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "dec %s",          oneparam);
    new Elm(INC,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "inc %s",          oneparam);
    new Elm(PUSH,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "push %s",         oneparam);
    new Elm(POP,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "pop %s",          oneparam);
    new Elm(SWAP,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "swap %s",         oneparam);
    new Elm(NEG,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "neg %s",          oneparam);
    new Elm(NOT,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "com %s",          oneparam);
    new Elm(TEST,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "tst %s",          oneparam);
    new Elm(SHL,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rol %s",          oneparam);
    new Elm(SHR,        R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "ror %s",          oneparam);
    new Elm(SHL0,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "lsl %s",          oneparam);
    new Elm(SHR0,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "lsr %s",          oneparam);
    new Elm(SHRS,       R8,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "asr %s",          oneparam);
    new Elm(GON,        XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rjmp %s",         oneparam);
    new Elm(GO,         INDIRECT,   "z",    "0",    "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "ijmp",            noparam);
    new Elm(GO,         INDIRECT,   "z",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "eijmp",           noparam);
    new Elm(GO,         XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "jmp %s",          oneparam);
    new Elm(CALLN,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "rcall %s",        oneparam);
    new Elm(CALL,       INDIRECT,   "z",    "0",    "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "icall",           noparam);
    new Elm(CALL,       INDIRECT,   "z",    "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "eicall",          noparam);
    new Elm(CALL,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "call %s",         oneparam);
    new Elm(IFC1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brcs %s",         oneparam);
    new Elm(IFZ1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "breq %s",         oneparam);
    new Elm(IFH1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brhs %s",         oneparam);
    new Elm(IFI0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brid %s",         oneparam);
    new Elm(IFN1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brmi %s",         oneparam);
    new Elm(IFC0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brcc %s",         oneparam);
    new Elm(IFZ0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brne %s",         oneparam);
    new Elm(IFH0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brhc %s",         oneparam);
    new Elm(IFI1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brie %s",         oneparam);
    new Elm(IFV0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brvc %s",         oneparam);
    new Elm(IFV1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brvs %s",         oneparam);
    new Elm(IFT0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brtc %s",         oneparam);
    new Elm(IFT1,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brts %s",         oneparam);
    new Elm(IFN0,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brpl %s",         oneparam);
    new Elm(IFSGE,      XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brge %s",         oneparam);
    new Elm(IFSL,       XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brlt %s",         oneparam);
    new Elm(IFBT,       REGBIT,     "sreg", "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brbs %s",         oneparam);
    new Elm(IFBF,       REGBIT,     "sreg", "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "brbc %s",         oneparam);
    new Elm(SKIP,       BITOPERATION,"-1",  "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,   "sbrc %s",         oneparam);
    new Elm(SKIP,       BITOPERATION,"-1",  "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,   "sbrs %s",         oneparam);
    new Elm(SKIP,       PORTBIT,    "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,   "sbic %s",         oneparam);
    new Elm(SKIP,       PORTBIT,    "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,   "sbis %s",         oneparam);
    new Elm(SKIP,       R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "1",    "-1",   1,   "spse %s,%s",      twoparam);
//          n           m1          b1      v1      ev1     l1    m2        b2      v2      ev2    l2    template           prn
    new Elm(SUB,        R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",     "-1",   1,    "dec %s",        oneparam);
    new Elm(SUB,        R16,        "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "sbiw %s,%s",    twoparam);
    new Elm(SUB,        R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "subi %s,%s",    twoparam);
    new Elm(SUB,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "sub %s,%s",     twoparam);
    new Elm(SBC,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "sbc %s,%s",     twoparam);
    new Elm(SBC,        R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "sbci %s,%s",    twoparam);
    new Elm(ADD,        R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",     "-1",   1,    "inc %s",        oneparam);
    new Elm(ADD,        R16,        "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "adiw %s,%s",    twoparam);
    new Elm(ADD,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "add %s,%s",     twoparam);
    new Elm(ADC,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "adc %s,%s",     twoparam);
    new Elm(MUL,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "a",    "-1",   "-1",   1,     "mul %s,%s",      twoparam);
    new Elm(AND,        R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "andi %s,%s",    twoparam);
    new Elm(AND,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "and %s,%s",     twoparam);
    new Elm(OR,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",    "-1",   1,    "ori %s,%s",     twoparam);
    new Elm(OR,         R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "or %s,%s",      twoparam);
    new Elm(XOR,        R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",    "-1",   1,    "eor %s,%s",     twoparam);
    new Elm(TO,         REGBIT,     "sreg", "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",     "-1",   1,    "bset %s",       oneparam);
    new Elm(TO,         REGBIT,     "sreg", "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",     "-1",   1,    "bclr %s",       oneparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clr %s",        oneparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "255",  "-1",   1,     "ser %s",        oneparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0xff", "-1",   1,     "ser %s",        oneparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0xFF", "-1",   1,     "ser %s",        oneparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",   "-1",   1,     "ldi %s,%s",     twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   DIRECT,   "-1",   "-1",   "-1",   1,     "lds %s,%s",     twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   INDEXED,  "y",    "-1",   "-1",   1,     "ldd %s,%s",     twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   INDEXED,  "z",    "-1",   "-1",   1,     "ldd %s,%s",     twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   INDEXED,  "-1",   "0",    "-1",   1,     "ld %s,%s",      twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   PREDEC,   "-1",   "0",    "-1",   1,     "ld %s,%s",      twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   POSTINC,  "-1",   "0",    "-1",   1,     "ld %s,%s",      twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "mov %s,%s",     twoparam);
    new Elm(TO,         R16,        "-1",   "-1",   "-1",   -1,   R16,      "-1",   "-1",   "-1",   1,     "movw %s,%s",    twoparam);
    new Elm(TO,         DIRECT,     "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "sts %s,%s",     twoparam);
    new Elm(TO,         INDEXED,    "y",    "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "std %s,%s",     twoparam);
    new Elm(TO,         INDEXED,    "z",    "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "std %s,%s",     twoparam);
    new Elm(TO,         INDEXED,    "-1",   "0",    "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "st %s,%s",      twoparam);
    new Elm(TO,         PREDEC,     "-1",   "0",    "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "st %s,%s",      twoparam);
    new Elm(TO,         POSTINC,    "-1",   "0",    "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "st %s,%s",      twoparam);
    new Elm(TO,         R1,         "t",    "-1",   "-1",   -1,   BITOPERATION,"-1","-1",   "-1",   1,     "bst %s,%s",     twoparam);
    new Elm(TO,         BITOPERATION,"-1",  "-1",   "-1",   -1,   R1,       "t",    "-1",   "-1",   1,     "bld %s,%s",     twoparam);
    new Elm(TO,         INDEXED,    "z",    "0",    "-1",   -1,   R16,      "a",    "-1",   "-1",   1,     "spm",           noparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   INDEXED,  "z",    "0",    "-1",   1,     "lpm %s,%s",     twoparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   POSTINC,  "z",    "0",    "-1",   1,     "lpm %s,%s",     twoparam);
    new Elm(TO,         R1,         "c",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sec",           noparam);
    new Elm(TO,         R1,         "c",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clc",           noparam);
    new Elm(TO,         R1,         "n",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sen",           noparam);
    new Elm(TO,         R1,         "n",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "cln",           noparam);
    new Elm(TO,         R1,         "z",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sez",           noparam);
    new Elm(TO,         R1,         "z",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clz",           noparam);
    new Elm(TO,         R1,         "i",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sei",           noparam);
    new Elm(TO,         R1,         "i",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "cli",           noparam);
    new Elm(TO,         R1,         "s",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "ses",           noparam);
    new Elm(TO,         R1,         "s",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "cls",           noparam);
    new Elm(TO,         R1,         "v",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sev",           noparam);
    new Elm(TO,         R1,         "v",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clv",           noparam);
    new Elm(TO,         R1,         "t",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "set",           noparam);
    new Elm(TO,         R1,         "t",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clt",           noparam);
    new Elm(TO,         R1,         "h",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "seh",           noparam);
    new Elm(TO,         R1,         "h",    "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "clh",           noparam);
    new Elm(TO,         R8,         "-1",   "-1",   "-1",   -1,   PORT,     "-1",   "-1",   "-1",   1,     "in %s,%s",      twoparam);
    new Elm(TO,         PORT,       "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "out %s,%s",     twoparam);
    new Elm(TO,         PORTBIT,    "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sbi %s",        oneparam);
    new Elm(TO,         PORTBIT,    "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "cbi %s",        oneparam);
    new Elm(TO,         REGBIT,     "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "1",    "-1",   1,     "sbr %s",        oneparam);
    new Elm(TO,         REGBIT,     "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "0",    "-1",   1,     "cbr %s",        oneparam);
    new Elm(SUBTEST,    R8,         "-1",   "-1",   "-1",   -1,   IMMEDIATE,"-1",   "-1",   "-1",   1,     "cpi %s,%s",     twoparam);
    new Elm(SUBTEST,    R8,         "-1",   "-1",   "-1",   -1,   R8,       "-1",   "-1",   "-1",   1,     "cp %s,%s",      twoparam);
    new Elm(INLINE,     XX,         "-1",   "-1",   "-1",   -1,   XX,       "-1",   "-1",   "-1",   1,   "ld %s,%s",        inlineop);
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
                                    opcode.m2,opcode.b2,opcode.v2,opcode.ev2,opcode.l2,op.template,op.prn,opcode.res)
            if o = "" then o <- op2.prn op2
    if o = "" then  let a,b,c = opcode.source; 
                    failwith ("Error: bad opcode "+opcode.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
    o

let ParseParameter( e : Elm, p : string) : Addressing*string*string*string =
    match p with
    | Regexp @"^(i|t|h|s|v|n|z|c)$" [r] -> (R1,r,"","")
    | Regexp @"^(sreg|r0|r1|r2|r3|r4|r5|r6|r7|r8|r9|r10|r11|r12|r13|r14|r15|r16|r17|r18|r19|r20|r21|r22|r23|r24|r25|r26|r27|r28|r29|r30|r31|xl|xh|yl|yh|zl|zh|spl|sph)$" [b] -> (R8,b,"","")
    | Regexp @"^(a|x|y|z|sp)$" [r] -> (R16,r,"","")
    | Regexp @"^(0x[0-9a-fA-F]+|0X[0-9a-fA-F]+)$" [d] -> (IMMEDIATE,"",d,"")
    | Regexp @"^([0-9]+)$" [d] -> (IMMEDIATE,"",d,"")
    | Regexp @"^((sreg|r0|r1|r2|r3|r4|r5|r6|r7|r8|r9|r10|r11|r12|r13|r14|r15|r16|r17|r18|r19|r20|r21|r22|r23|r24|r25|r26|r27|r28|r29|r30|r31|xl|xh|yl|yh|zl|zh))\#([0-8])$" [a;b] -> (REGBIT,"",a,b) 
    | Regexp @"^([A-Za-z0-9\._]+)\#([0-8])$" [a;b] -> (BITOPERATION,"",a,b)
    | Regexp @"^\<([A-Za-z0-9\._]+)\>\#([0-8])$" [a;b] -> (PORTBIT,"",a,b)
    | Regexp @"^\[([A-Za-z0-9\._]+)\]\[(x|y|sp)\]\]$" [e;r] -> (INDIRECT,r,e,"")
    | Regexp @"^\[(x|y|z)\+\+\]$" [r] -> (POSTINC,r,"0","")
    | Regexp @"^\[\-\-(x|y|z)\]$" [r] -> (PREDEC,r,"0","")
    | Regexp @"^\[(x|y|z|sp)\]$" [r] -> (INDEXED,r,"0","")
    | Regexp @"^\<([A-Za-z0-9\._]+)\>$" [r] -> (PORT,r,"0","")
    | Regexp @"^([A-Za-z0-9\._]+)\[(x|y|sp)\]$" [e;r] -> (INDEXED,r,e,"")
    | Regexp @"^\[([A-Za-z0-9\._]+)\]$" [e] -> (INDIRECT,"",e,"")
    | Regexp @"^([A-Za-z0-9\._]+)$" [i] -> (DIRECT,"",i,"")
    | _ ->  let a,b,c = e.source; 
            failwith ("Error: unknown addressing mode "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)

let prefix : string =
    sprintf ".LIST\n.text\n.org 0x00\n"

let postfix  =
    "\n\n;File composed by PCEUDO\n"

let mutable currentseg = ""

let p2s ( p : Place) = match p with | REGRAM -> "reg" | RAM -> "ram" | EEPROM -> "eeprom" | FLASH -> "flash" | _ -> "unify"
let segtype seg = match seg with | "reg" -> "DATA" | "ram" -> "DATA" | "eeprom" -> "DATA" | "flash" -> "CODE" | _ -> "UNIFY"
let lentype len = match len with | 1 -> ".b" | 2 -> ".w" | 4 -> ".l" | _ -> failwith "Error: bad variable size"
let countifneed cnt = if cnt < 2 then "" else cnt |> string
let i2hex i = sprintf "%X" i

let seg2cmd seg =
    match seg with
    | "reg" -> ".data"
    | "ram" -> ".data"
    | "flash" -> ".text"
    | "eeprom" -> ".data"
    | _ -> ""

let generateSeg seg =
    if currentseg <> seg
    then
        currentseg <- seg
        (seg2cmd seg) + "\n"
    else
        ""

let generateSimpleLabel seg name = 
    ((generateSeg seg) + name + ":\n")

let generateAddressLabel seg name addr = 
    ((generateSeg seg) + ".ORG 0x" + (i2hex addr) + "\n" + name + ":\n")

let generateDataPlace width count =
    ("\tds"+(lentype width)+" "+(countifneed count)+"\n")

let generateData width (data : string) = 
    ("\tdc"+(lentype width)+"\t"+(data.Replace("0x","$").Replace("0X","$"))+"\n")

let generateCode cmd = 
    if currentseg <> "flash" 
    then 
        currentseg <- "flash"; 
        ((seg2cmd "flash") + "\n\t" + cmd + "\n") 

    else 
        "\t" + cmd + "\n"

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
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateData e.sz e.v1;
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateData e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateData e.sz e.v1
                   | CONSTARRAY -> if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateData e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; ()
                                        else e.res <- generateAddressLabel labelname (p2s e.p) (labeladdress|>int) + generateData e.sz e.v1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateData e.sz e.v1
                   | VAR ->        if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateDataPlace e.sz 1; 
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateDataPlace e.sz 1; 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <-generateDataPlace e.sz 1
                   | ARRAY ->      if haslabel 
                                   then if labeladdress = "" 
                                        then e.res <- generateSimpleLabel (p2s e.p) labelname + generateDataPlace e.sz (e.v1|>int); 
                                                      haslabel <- false; labelname <- ""
                                        else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateDataPlace e.sz (e.v1|>int); 
                                                      haslabel <- false; labelname <- ""; labeladdress <- "";
                                   else e.res <- generateDataPlace e.sz (e.v1|>int)
                   | _ -> failwith "Error: Bad record subtype"
        | CODE ->   if haslabel 
                    then if labeladdress = "" 
                         then e.res <- generateSimpleLabel (p2s e.p) labelname + generateCode (code e);
                                       haslabel <- false; labelname <- "";
                         else e.res <- generateAddressLabel (p2s e.p) labelname (labeladdress|>int) + generateCode (code e);
                                       haslabel <- false; labelname <- ""; labeladdress <- ""
                    else e.res <- generateCode (code e);
        | _ -> let a,b,c = e.source; 
               failwith ("Error: bad record type "+e.elem+" in '"+c+"' at "+(b |> string)+" line of "+a)
        yield e
    ]

let printcode (e : Elm list) =
    let mutable o = prefix
    for i in e do 
        o <- o + i.res
    o <- o + postfix
    o