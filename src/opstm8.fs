module opstm8

open elem


let xhi v = (v >>> 16) &&& 0xff
let hi v = (v >>> 8) &&& 0xff
let lo v = v &&& 0xff

let drawmode m b v ev =
    match m,b,v with
    | R1            ,_,_ -> b
    | R8            ,_,_ -> b
    | R16           ,_,_ -> b
    | IMMEDIATE     ,_,_ -> ("#" + ( v |> string))
    | DIRECT        ,_,_ -> ( v |> string)
    | INDEXED       ,_,0 -> ("(" + b + ")")
    | INDEXED       ,_,_ -> ("(" + (v |> string) + "," + b + ")")
    | INDIRECT      ,_,0 -> ("[" + b + "]")
    | INDIRECT      ,_,_ -> ("([" + (v |> string) + "]," + b + ")")
    | BITOPERATION  ,_,_ -> ( v |> string) + ",#" + ( ev |> string)
    | _ -> failwith "Error: incorrect addressing mode"

let drawcode ps m1 b1 v1 ev1 l1 m2 b2 v2 l2 ev2 : list<int> = // todo must add for call/jmp
    let compose (l : int) (v : int) : list<int> = if l = 65536 then [hi v;lo v] elif l = 256 then [lo v] else failwith "Error: incorrect max len"
    match ps with
    | 1 ->  match m1,b1,v1 with
            | R1            ,_,_ -> []
            | R8            ,_,_ -> []
            | R16           ,_,_ -> []
            | IMMEDIATE     ,_,_ -> compose l1 v1
            | DIRECT        ,_,_ -> compose l1 v1
            | INDEXED       ,_,0 -> []
            | INDEXED       ,_,_ -> compose l1 v1
            | INDIRECT      ,_,_ -> compose l1 v1
            | BITOPERATION  ,_,_ -> [(1+2*ev1);hi v1;lo v1]
            | _ -> failwith "Error: incorrect command-parameter-addressing combination";
    | 2 ->  match m1,b1,v1,m2,b2,v2 with
            | R8        ,_,_,R8,_,_ ->  []
            | R8        ,_,_,IMMEDIATE,_,_ -> [lo v2]
            | R8        ,_,_,DIRECT,_,_ ->  compose l2 v2
            | R8        ,_,_,INDEXED,_,0 -> []
            | R8        ,_,_,INDEXED,_,_ -> compose l2 v2
            | R8        ,_,_,INDIRECT,_,_ -> compose l2 v2
            | DIRECT    ,_,_,R8,_,_ -> compose l1 v1
            | INDEXED   ,_,0,R8,_,_ -> []
            | INDEXED   ,_,_,R8,_,_ -> compose l1 v1
            | INDIRECT  ,_,_,R8,_,_ -> compose l1 v1
            | R16       ,_,_,R16,_,_ -> []
            | R16       ,_,_,R8,_,_ -> []
            | DIRECT    ,_,_,IMMEDIATE,_,_ -> [lo v2;hi v1;lo v1]
            | DIRECT    ,_,_,DIRECT,_,_ -> if l1 = 256 && l2 = 256 then [lo v2; lo v1] else [hi v2;lo v2;hi v1;lo v1]
            | _ -> failwith "Error: incorrect command-parameter-addressing combination";
    | _ -> failwith "Error: incorrect parameters count"

//let noparam ( o : Elm) = (o.code, o.template)
//let oneparam ( o : Elm) = (o.code @ (drawcode 1 o.m1 o.b1 o.v1 o.ev1 o.l1 o.m2 o.b2 o.v2 o.ev2 o.l2), o.template+" "+(drawmode o.m1 o.b1 o.v1 o.ev1))
//let twoparam ( o : Elm) = (o.code @ (drawcode 2 o.m1 o.b1 o.v1 o.ev1 o.l1 o.m2 o.b2 o.v2 o.ev2 o.l2), o.template+" "+(drawmode o.m1 o.b1 o.v1 o.ev1)+","+(drawmode o.m2 o.b2 o.v2 o.ev2))

//let ops : list<Elm>= [
//// no param
//    new Elm(Subtype.UNKNOWN,NOP,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"nop",[157],noparam);
//    new Elm(Subtype.UNKNOWN,TRAP,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"trap",[131],noparam);
//    new Elm(Subtype.UNKNOWN,WAITI,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"wfi",[143],noparam);
//    new Elm(Subtype.UNKNOWN,WAIT,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"wfe",[114; 143],noparam);
//    new Elm(Subtype.UNKNOWN,HALT,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"halt",[142],noparam);
//    new Elm(Subtype.UNKNOWN,RET,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"ret",[129],noparam);
//    new Elm(Subtype.UNKNOWN,RETF,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"retf",[135],noparam);
//    new Elm(Subtype.UNKNOWN,IRET,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"iret",[128],noparam);
//// one
//    new Elm(Subtype.UNKNOWN,TO,R1,UNKNOWN,"i",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",1,-1,1,"sim",[155],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R1,UNKNOWN,"i",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"rim",[154],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R1,UNKNOWN,"c",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",1,-1,1,"scf",[153],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R1,UNKNOWN,"c",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"rcf",[152],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R1,UNKNOWN,"v",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"rvf",[156],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,R1,UNKNOWN,"c",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,1,"ccf",[140],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,BITOPERATION,UNKNOWN,"-1",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"bcpl",[114],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,BITOPERATION,UNKNOWN,"-1",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,65536,"bres",[114],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,BITOPERATION,UNKNOWN,"-1",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",1,-1,65536,"bset",[114],oneparam);
//    new Elm(Subtype.UNKNOWN,POP,R8,UNKNOWN,"a",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"pop",[132],oneparam);
//    new Elm(Subtype.UNKNOWN,POP,R8,UNKNOWN,"cc",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"pop",[134],oneparam);
//    new Elm(Subtype.UNKNOWN,POP,R16,UNKNOWN,"x",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"popw",[133],oneparam);
//    new Elm(Subtype.UNKNOWN,POP,R16,UNKNOWN,"y",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"popw",[144; 133],oneparam);
//    new Elm(Subtype.UNKNOWN,POP,DIRECT,UNKNOWN,"-1",-1,-1,65536,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"pop",[50],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,R8,UNKNOWN,"a",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"push",[136],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,R8,UNKNOWN,"cc",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"push",[138],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,R16,UNKNOWN,"x",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"pushw",[137],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,R16,UNKNOWN,"y",-1,-1,-1,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"pushw",[144; 137],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,DIRECT,UNKNOWN,"-1",-1,-1,65536,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"push",[50],oneparam);
//    new Elm(Subtype.UNKNOWN,PUSH,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,UNDEFINED,UNKNOWN,"-1",-1,-1,-1,"push",[50],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[67],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cplw",[83],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cplw",[144; 83],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[51],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[114; 83],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[115],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[99],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[114; 67],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[144; 115],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[144; 99],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[145; 67],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[3],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[146; 51],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[114; 51],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[146; 99],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[114; 99],oneparam);
//    new Elm(Subtype.UNKNOWN,NOT,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"cpl",[145; 99],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[74],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"decw",[90],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"decw",[144; 90],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[58],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[114; 90],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[122],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[106],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[114; 74],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[144; 122],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[144; 106],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[145; 74],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[10],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[146; 58],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[114; 58],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[146; 106],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[114; 106],oneparam);
//    new Elm(Subtype.UNKNOWN,DEC,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"dec",[145; 106],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[76],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"incw",[92],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"incw",[144; 92],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[60],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[114; 92],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[124],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[108],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[114; 76],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[144; 124],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[144; 108],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[145; 76],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[12],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[146; 60],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[114; 60],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[146; 108],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[114; 108],oneparam);
//    new Elm(Subtype.UNKNOWN,INC,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"inc",[145; 108],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[64],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"negw",[80],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"negw",[144; 80],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[48],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[114; 80],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[112],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[96],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[114; 64],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[144; 112],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[144; 96],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[145; 64],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[0],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[146; 48],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[114; 48],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[146; 96],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[114; 96],oneparam);
//    new Elm(Subtype.UNKNOWN,NEG,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"neg",[145; 96],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[78],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swapw",[94],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swapw",[144; 94],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[62],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[114; 94],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[126],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[110],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[114; 78],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[144; 126],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[144; 110],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[145; 78],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[14],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[146; 62],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[114; 62],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[146; 110],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[114; 110],oneparam);
//    new Elm(Subtype.UNKNOWN,SWAP,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"swap",[145; 110],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[77],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnzw",[93],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnzw",[144; 93],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[61],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[114; 93],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[125],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[109],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[114; 77],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[144; 125],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[144; 109],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[145; 77],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[13],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[146; 61],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[114; 61],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[146; 109],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[114; 109],oneparam);
//    new Elm(Subtype.UNKNOWN,TEST,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"tnz",[145; 109],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[79],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[95],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,R16,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[144; 95],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,DIRECT,UNKNOWN,"-1",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[63],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,DIRECT,UNKNOWN,"-1",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[114; 95],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"x",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[127],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"x",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[111],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"x",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[114; 79],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"y",0,-1,-1,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[144; 127],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"y",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[144; 111],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"y",-1,-1,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[145; 79],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDEXED,UNKNOWN,"sp",-1,-1,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[15],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDIRECT,UNKNOWN,"",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[146; 63],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDIRECT,UNKNOWN,"",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[114; 63],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDIRECT,UNKNOWN,"x",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[146; 111],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDIRECT,UNKNOWN,"x",-1,2,65536,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[114; 111],oneparam);
//    new Elm(Subtype.UNKNOWN,TO,INDIRECT,UNKNOWN,"y",-1,2,256,IMMEDIATE,UNKNOWN,"-1",0,-1,1,"clr",[145; 111],oneparam);
//// two
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"cp",[161],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"cp",[177],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"cp",[193],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"cp",[241],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"cp",[225],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"cp",[209],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"cp",[144; 241],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"cp",[144; 225],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"cp",[144; 209],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"cp",[17],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"cp",[146; 193],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"cp",[114; 193],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"cp",[146; 209],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"cp",[146; 209],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"cp",[145; 209],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"cpw",[163],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"cpw",[179],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"cpw",[195],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"cpw",[144; 243],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"cpw",[144; 227],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,65536,"cpw",[144; 211],twoparam);
//    new Elm(Subtype.UNKNOWN,SUBTEST,R16,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"cpw",[114; 251],twoparam);
//    new Elm(Subtype.UNKNOWN,MUL,R16,UNKNOWN,"x",-1,-1,-1,R8,UNKNOWN,"a",-1,-1,-1,"mul",[66],twoparam);
//    new Elm(Subtype.UNKNOWN,MUL,R16,UNKNOWN,"y",-1,-1,-1,R8,UNKNOWN,"a",-1,-1,-1,"mul",[144; 66],twoparam);
//    new Elm(Subtype.UNKNOWN,DIV,R16,UNKNOWN,"x",-1,-1,-1,R8,UNKNOWN,"a",-1,-1,-1,"div",[98],twoparam);
//    new Elm(Subtype.UNKNOWN,DIV,R16,UNKNOWN,"y",-1,-1,-1,R8,UNKNOWN,"a",-1,-1,-1,"div",[144; 98],twoparam);
//    new Elm(Subtype.UNKNOWN,DIV,R16,UNKNOWN,"x",-1,-1,-1,R16,UNKNOWN,"y",-1,-1,-1,"divw",[144; 90],twoparam);
//    new Elm(Subtype.UNKNOWN,CHANGE,R8,UNKNOWN,"a",-1,-1,-1,R8,UNKNOWN,"xl",-1,-1,-1,"exg",[65],twoparam);
//    new Elm(Subtype.UNKNOWN,CHANGE,R8,UNKNOWN,"a",-1,-1,-1,R8,UNKNOWN,"xh",-1,-1,-1,"exg",[97],twoparam);
//    new Elm(Subtype.UNKNOWN,CHANGE,R16,UNKNOWN,"x",-1,-1,-1,R16,UNKNOWN,"y",-1,-1,-1,"exg",[81],twoparam);
//    new Elm(Subtype.UNKNOWN,CHANGE,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"exg",[49],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"and",[164],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"and",[180],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"and",[196],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"and",[244],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"and",[228],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"and",[212],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"and",[144; 244],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"and",[144; 228],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"and",[144; 212],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"and",[20],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"and",[146; 196],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"and",[114; 196],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"and",[146; 212],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"and",[146; 212],twoparam);
//    new Elm(Subtype.UNKNOWN,AND,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"and",[145; 212],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"or",[170],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"or",[186],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"or",[202],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"or",[250],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"or",[234],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"or",[218],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"or",[144; 250],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"or",[144; 234],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"or",[144; 218],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"or",[26],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"or",[146; 202],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"or",[114; 202],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"or",[146; 218],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"or",[146; 218],twoparam);
//    new Elm(Subtype.UNKNOWN,OR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"or",[145; 218],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"xor",[168],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"xor",[184],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"xor",[200],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"xor",[248],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"xor",[232],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"xor",[216],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"xor",[144; 248],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"xor",[144; 232],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"xor",[144; 216],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"xor",[24],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"xor",[146; 200],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"xor",[114; 200],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"xor",[146; 216],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"xor",[146; 216],twoparam);
//    new Elm(Subtype.UNKNOWN,XOR,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"xor",[145; 216],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"adc",[169],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"adc",[185],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"adc",[201],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"adc",[249],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"adc",[233],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"adc",[217],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"adc",[144; 249],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"adc",[144; 233],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"adc",[144; 217],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"adc",[25],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"adc",[146; 201],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"adc",[114; 201],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"adc",[146; 217],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"adc",[146; 217],twoparam);
//    new Elm(Subtype.UNKNOWN,ADC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"adc",[145; 217],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"sbc",[162],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"sbc",[178],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"sbc",[194],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"sbc",[242],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"sbc",[226],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"sbc",[210],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"sbc",[144; 242],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"sbc",[144; 226],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"sbc",[144; 210],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"sbc",[18],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"sbc",[146; 194],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"sbc",[114; 194],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"sbc",[146; 210],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"sbc",[146; 210],twoparam);
//    new Elm(Subtype.UNKNOWN,SBC,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"sbc",[145; 210],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"bcp",[165],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"bcp",[181],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"bcp",[197],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"bcp",[245],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"bcp",[229],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"bcp",[213],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"bcp",[144; 245],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"bcp",[144; 229],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"bcp",[144; 213],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"bcp",[21],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"bcp",[146; 197],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"bcp",[114; 197],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"bcp",[146; 213],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"bcp",[146; 213],twoparam);
//    new Elm(Subtype.UNKNOWN,ANDTEST,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"bcp",[145; 213],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"add",[171],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"add",[187],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"add",[203],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"add",[251],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"add",[235],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"add",[219],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"add",[144; 251],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"add",[144; 235],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"add",[144; 219],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"add",[27],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"add",[146; 203],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"add",[114; 203],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"add",[146; 219],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"add",[146; 219],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"add",[145; 219],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"add",[28],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"x",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"add",[114; 187],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"sp",0,-1,-1,"add",[114; 251],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"add",[114; 169],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"y",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"add",[114; 185],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"y",-1,-1,-1,INDEXED,UNKNOWN,"sp",0,-1,-1,"add",[114; 249],twoparam);
//    new Elm(Subtype.UNKNOWN,ADD,R8,UNKNOWN,"sp",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"add",[91],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,256,"sub",[160],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,256,"sub",[176],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"sub",[192],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",0,-1,-1,"sub",[240],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,256,"sub",[224],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"x",-1,-1,65536,"sub",[208],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",0,-1,-1,"sub",[144; 240],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,256,"sub",[144; 224],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"y",-1,-1,65536,"sub",[144; 208],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDEXED,UNKNOWN,"sp",-1,-1,256,"sub",[16],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,256,"sub",[146; 192],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"",-1,-1,65536,"sub",[114; 192],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,256,"sub",[146; 208],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"x",-1,-1,65536,"sub",[146; 208],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"a",-1,-1,-1,INDIRECT,UNKNOWN,"y",-1,-1,256,"sub",[145; 208],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"x",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"sub",[29],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"x",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"sub",[114; 176],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"x",-1,-1,-1,INDEXED,UNKNOWN,"sp",0,-1,-1,"sub",[114; 240],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"y",-1,-1,-1,IMMEDIATE,UNKNOWN,"-1",-1,-1,65536,"sub",[114; 162],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"y",-1,-1,-1,DIRECT,UNKNOWN,"-1",-1,-1,65536,"sub",[114; 178],twoparam);
//    new Elm(Subtype.UNKNOWN,SUB,R8,UNKNOWN,"y",-1,-1,-1,INDEXED,UNKNOWN,"sp",0,-1,-1,"sub",[114; 242],twoparam);
(*
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOP;     m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "nop" ;code = [0x9d]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TRAP;    m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "trap" ;code = [0x83]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = WAITI;   m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "wfi" ;code = [0x8f]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = WAIT;    m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "wfe" ;code = [0x72;0x8f]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = HALT;    m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "halt" ;code = [0x8e]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = RET;     m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "ret" ;code = [0x81]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = RETF;    m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "retf" ;code = [0x87]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = IRET;    m1 = UNDEFINED;p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "iret" ;code = [0x80]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R1;       p1 = UNKNOWN;  b1 = "i";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 1;    ev2 = -1;  l2 = 1;    template = "sim" ;code = [0x9b]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R1;       p1 = UNKNOWN;  b1 = "i";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "rim" ;code = [0x9a]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R1;       p1 = UNKNOWN;  b1 = "c";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 1;    ev2 = -1;  l2 = 1;    template = "scf" ;code = [0x99]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R1;       p1 = UNKNOWN;  b1 = "c";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "rcf" ;code = [0x98]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R1;       p1 = UNKNOWN;  b1 = "v";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "rvf" ;code = [0x9c]; prn = noparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = R1;       p1 = UNKNOWN;  b1 = "c";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 1;    template = "ccf" ;code = [0x8c]; prn = noparam};
// 1 param
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;  m1 = BITOPERATION; p1 = UNKNOWN; b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536;template = "bcpl" ;code = [0x72]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;   m1 = BITOPERATION; p1 = UNKNOWN; b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 65536;template = "bres" ;code = [0x72]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;   m1 = BITOPERATION; p1 = UNKNOWN; b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 1;    ev2 = -1;  l2 = 65536;template = "bset" ;code = [0x72]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = POP;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "pop" ; code = [0x84]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = POP;     m1 = R8;       p1 = UNKNOWN;  b1 = "cc";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "pop" ; code = [0x86]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = POP;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "popw" ; code = [0x85]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = POP;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "popw" ; code = [0x90;0x85]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = POP;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "pop" ; code = [0x32]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "push" ; code = [0x88]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = R8;       p1 = UNKNOWN;  b1 = "cc";  v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "push" ; code = [0x8a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "pushw" ; code = [0x89]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "pushw" ; code = [0x90;0x89]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "push" ; code = [0x32]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = PUSH;    m1 = IMMEDIATE; p1 = UNKNOWN; b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = UNDEFINED;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = -1;   template = "push" ; code = [0x32]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x43]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cplw" ;code = [0x53]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cplw" ;code = [0x90;0x53]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x33]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x72;0x53]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x73]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x63]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x72;0x43]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x90;0x73]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x90;0x63]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x91;0x43]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x03]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x92;0x33]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x72;0x33]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x92;0x63]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x72;0x63]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NOT;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "cpl" ;code = [0x91;0x63]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x4a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "decw" ;code = [0x5a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "decw" ;code = [0x90;0x5a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x3a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x72;0x5a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x7a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x6a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x72;0x4a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x90;0x7a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x90;0x6a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x91;0x4a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x0a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x92;0x3a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x72;0x3a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x92;0x6a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x72;0x6a]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DEC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "dec" ;code = [0x91;0x6a]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x4c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "incw" ;code = [0x5c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "incw" ;code = [0x90;0x5c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x3c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x72;0x5c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x7c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x6c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x72;0x4c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x90;0x7c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x90;0x6c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x91;0x4c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x0c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x92;0x3c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x72;0x3c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x92;0x6c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x72;0x6c]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = INC;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "inc" ;code = [0x91;0x6c]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x40]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "negw" ;code = [0x50]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "negw" ;code = [0x90;0x50]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x30]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x72;0x50]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x70]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x60]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x72;0x40]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x90;0x70]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x90;0x60]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x91;0x40]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x00]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x92;0x30]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x72;0x30]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x92;0x60]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x72;0x60]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = NEG;     m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "neg" ;code = [0x91;0x60]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x4e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swapw" ;code = [0x5e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swapw" ;code = [0x90;0x5e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x3e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x72;0x5e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x7e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x6e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x72;0x4e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x90;0x7e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x90;0x6e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x91;0x4e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x0e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x92;0x3e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x72;0x3e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x92;0x6e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x72;0x6e]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SWAP;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "swap" ;code = [0x91;0x6e]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x4d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnzw" ;code = [0x5d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnzw" ;code = [0x90;0x5d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x3d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x72;0x5d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x7d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x6d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x72;0x4d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x90;0x7d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x90;0x6d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x91;0x4d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x0d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x92;0x3d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x72;0x3d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x92;0x6d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x72;0x6d]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TEST;    m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "tnz" ;code = [0x91;0x6d]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x4f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x5f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x90;0x5f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x3f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = DIRECT;   p1 = UNKNOWN;  b1 = "-1";  v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x72;0x5f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x7f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x6f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x72;0x4f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = 0;    ev1 = -1;  l1 = -1;     m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x90;0x7f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x90;0x6f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x91;0x4f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDEXED;  p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x0f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x92;0x3f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDIRECT; p1 = UNKNOWN;  b1 = "";    v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x72;0x3f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x92;0x6f]; prn = oneparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDIRECT; p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = 2;   l1 = 65536;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x72;0x6f]; prn = oneparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = TO;      m1 = INDIRECT; p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = 2;   l1 = 256;    m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = 0;    ev2 = -1;  l2 = 1;    template = "clr" ;code = [0x91;0x6f]; prn = oneparam};

// 2 param
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0xa1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0xb1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "cp" ;code = [0xc1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "cp" ;code = [0xf1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0xe1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "cp" ;code = [0xd1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "cp" ;code = [0x90;0xf1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0x90;0xe1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "cp" ;code = [0x90;0xd1]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0x11]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0x92;0xc1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "cp" ;code = [0x72;0xc1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0x92;0xd1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "cp" ;code = [0x92;0xd1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "cp" ;code = [0x91;0xd1]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "cpw" ;code = [0xa3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "cpw" ;code = [0xb3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "cpw" ;code = [0xc3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "cpw" ;code = [0x90;0xf3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "cpw" ;code = [0x90;0xe3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = 65536; template = "cpw" ;code = [0x90;0xd3]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ASUBTEST;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "cpw" ;code = [0x72;0xfb]; prn = twoparam};


        //elif f = "x" && a = INDIRECT && p = REGRAM && b = "" && v < 256 && m = "w" then ([| 0x92; 0xc3; lo v |],"cpw x,["+ (v |> string)+"]")
        //elif f = "x" && a = INDIRECT && p = RAM && b = "" && v < 65536 && m = "w" then ([| 0x72; 0xc3; hi v; lo v |],"cpw x,["+ (v |> string)+"]")
        //elif f = "x" && a = INDIRECT && p = REGRAM && b = "y" && v < 256 && m = "w" then ([| 0x91; 0xd3; lo v |],"cpw x,(["+ (v |> string)+"],y)")
        //elif f = "y" && a = IMMEDIATE && v < 65536  then ([| 0x90; 0xa3; hi v; lo v |], "cpw y,#"+ (v |> string))
        //elif f = "y" && a = DIRECT && p = REGRAM && v < 256 then ([| 0x90; 0xb3; lo v |], "cpw y,"+ (v |> string))
        //elif f = "y" && a = DIRECT && p = RAM && v < 65536 then ([| 0x90; 0xc3; hi v; lo v |], "cpw y,"+ (v |> string))
        //elif f = "y" && a = INDEXED && b = "x" && v = 0  then ([| 0xf3 |],"cpw y,(x)")
        //elif f = "y" && a = INDEXED && b = "x" && v < 256  then ([| 0xe3; lo v |],"cpw y,("+ (lo v |> string)+",x)")
        //elif f = "y" && a = INDEXED && b = "x" && v < 65535  then ([| 0xd3; hi v; lo v |],"cpw y,("+ (v |> string)+",x)")
        //elif f = "x" && a = INDEXED && b = "sp" && v < 256  then ([| 0x51; 0x72; 0xfb; lo v; 0x51 |],"exgw x,y\ncpw x,("+ (lo v |> string)+",sp)\nexgw x,y")
        //elif f = "y" && a = INDIRECT && p = REGRAM && b = "" && v < 256 && m = "w" then ([| 0x91; 0xc3; lo v |],"cpw y,["+ (v |> string)+"]")
        //elif f = "y" && a = INDIRECT && p = REGRAM && b = "x" && v < 256 && m = "w" then ([| 0x92; 0xd3; lo v |],"cpw y,(["+ (lo v |> string)+"],x)")
        //elif f = "y" && a = INDIRECT && p = REGRAM && b = "x" && v < 65535 && m = "w" then ([| 0x72; 0xd3; hi v; lo v |],"cpw y,(["+ (v |> string)+"],x)")
        //else failwith "Error: incorrect command-parameter-addressing combination"

    
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = MUL;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "a";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "mul" ;code = [0x42]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = MUL;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "a";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "mul" ;code = [0x90;0x42]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DIV;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "a";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "div" ;code = [0x62]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DIV;     m1 = R16;      p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "a";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "div" ;code = [0x90;0x62]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = DIV;     m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R16;        p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "divw" ;code = [0x90;0x5a]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = CHANGE;  m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "xl";  v2 = -1;   ev2 = -1;  l2 = -1;    template = "exg" ;code = [0x41]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = CHANGE;  m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R8;         p2 = UNKNOWN;  b2 = "xh";  v2 = -1;   ev2 = -1;  l2 = -1;    template = "exg" ;code = [0x61]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = CHANGE;  m1 = R16;      p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = R16;        p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = -1;    template = "exg" ;code = [0x51]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = CHANGE;  m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "exg" ;code = [0x31]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0xa4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0xb4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "and" ;code = [0xc4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "and" ;code = [0xf4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0xe4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "and" ;code = [0xd4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "and" ;code = [0x90;0xf4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0x90;0xe4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "and" ;code = [0x90;0xd4]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0x14]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0x92;0xc4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "and" ;code = [0x72;0xc4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0x92;0xd4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "and" ;code = [0x92;0xd4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AND;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "and" ;code = [0x91;0xd4]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0xaa]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0xba]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "or" ;code = [0xca]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "or" ;code = [0xfa]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0xea]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "or" ;code = [0xda]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "or" ;code = [0x90;0xfa]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0x90;0xea]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "or" ;code = [0x90;0xda]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0x1a]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0x92;0xca]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "or" ;code = [0x72;0xca]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0x92;0xda]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "or" ;code = [0x92;0xda]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = OR;      m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "or" ;code = [0x91;0xda]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0xa8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0xb8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "xor" ;code = [0xc8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "xor" ;code = [0xf8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0xe8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "xor" ;code = [0xd8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "xor" ;code = [0x90;0xf8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0x90;0xe8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "xor" ;code = [0x90;0xd8]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0x18]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0x92;0xc8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "xor" ;code = [0x72;0xc8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0x92;0xd8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "xor" ;code = [0x92;0xd8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = XOR;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "xor" ;code = [0x91;0xd8]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0xa9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0xb9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "adc" ;code = [0xc9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "adc" ;code = [0xf9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0xe9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "adc" ;code = [0xd9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "adc" ;code = [0x90;0xf9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0x90;0xe9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "adc" ;code = [0x90;0xd9]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0x19]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0x92;0xc9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "adc" ;code = [0x72;0xc9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0x92;0xd9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "adc" ;code = [0x92;0xd9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "adc" ;code = [0x91;0xd9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0xa2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0xb2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sbc" ;code = [0xc2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "sbc" ;code = [0xf2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0xe2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sbc" ;code = [0xd2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "sbc" ;code = [0x90;0xf2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0x90;0xe2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sbc" ;code = [0x90;0xd2]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0x12]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0x92;0xc2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "sbc" ;code = [0x72;0xc2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0x92;0xd2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sbc" ;code = [0x92;0xd2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SBC;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sbc" ;code = [0x91;0xd2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0xa5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0xb5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "bcp" ;code = [0xc5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "bcp" ;code = [0xf5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0xe5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "bcp" ;code = [0xd5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "bcp" ;code = [0x90;0xf5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0x90;0xe5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "bcp" ;code = [0x90;0xd5]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0x15]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0x92;0xc5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "bcp" ;code = [0x72;0xc5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0x92;0xd5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "bcp" ;code = [0x92;0xd5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = AANDTEST;m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "bcp" ;code = [0x91;0xd5]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0xab]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0xbb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0xcb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "add" ;code = [0xfb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0xeb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0xdb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "add" ;code = [0x90;0xfb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0x90;0xeb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x90;0xdb]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0x1b]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0x92;0xcb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x72;0xcb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0x92;0xdb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x92;0xdb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "add" ;code = [0x91;0xdb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x1c]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x72;0xbb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = 0;    ev2 = -1;  l2 = -1;    template = "add" ;code = [0x72;0xfb]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x72;0xa9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "add" ;code = [0x72;0xb9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = 0;    ev2 = -1;  l2 = -1;    template = "add" ;code = [0x72;0xf9]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = ADD;     m1 = R8;       p1 = UNKNOWN;  b1 = "sp";  v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;  template = "add" ;code = [0x5b]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0xa0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0xb0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0xc0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "sub" ;code = [0xf0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0xe0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0xd0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = 0;    ev2 = -1;  l2 = -1;    template = "sub" ;code = [0x90;0xf0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0x90;0xe0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x90;0xd0]; prn = twoparam}
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0x10]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0x92;0xc0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "";    v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x72;0xc0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0x92;0xd0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "x";   v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x92;0xd0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "a";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDIRECT;   p2 = UNKNOWN;  b2 = "y";   v2 = -1;   ev2 = -1;  l2 = 256;   template = "sub" ;code = [0x91;0xd0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x1d]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x72;0xb0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "x";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = 0;    ev2 = -1;  l2 = -1;    template = "sub" ;code = [0x72;0xf0]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = IMMEDIATE;  p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x72;0xa2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = DIRECT;     p2 = UNKNOWN;  b2 = "-1";  v2 = -1;   ev2 = -1;  l2 = 65536; template = "sub" ;code = [0x72;0xb2]; prn = twoparam};
    { source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = UNDEFINED; sz = "-1"; n = SUB;     m1 = R8;       p1 = UNKNOWN;  b1 = "y";   v1 = -1;   ev1 = -1;  l1 = -1;  m2 = INDEXED;    p2 = UNKNOWN;  b2 = "sp";  v2 = 0;    ev2 = -1;  l2 = -1;    template = "sub" ;code = [0x72;0xf2]; prn = twoparam};
    *)
 //   ]

//let code (opcode : Elm) : int list*string  =
//    let mutable o : int list*string = ([],"")
//    for op in ops do
//        if  op.n = opcode.n &&
//            (op.m1 = opcode.m1 || op.m1 = UNDEFINED) &&
//            (op.p1 = opcode.p1 || op.p1 = UNKNOWN) &&
//            (op.b1 = opcode.b1 || op.b1 = "-1") &&
//            (op.v1 = opcode.v1 || op.v1 = -1) &&
//            (op.ev1 = opcode.ev1 || op.ev1 = -1) &&
//            ((op.l1 = opcode.l1 && opcode.v1 < op.l1)|| op.l1 = -1 )&&
//            (op.m2 = opcode.m2 || op.m2 = UNDEFINED) &&
//            (op.p2 = opcode.p2 || op.p2 = UNKNOWN) &&
//            (op.b2 = opcode.b2 || op.b2 = "-1") &&
//            (op.v2 = opcode.v2 || op.v2 = -1) &&
//            (op.ev2 = opcode.ev2 || op.ev2 = -1) &&
//            ((op.l2 = opcode.l2 && opcode.v2 < op.l2)|| op.l2 = -1 )
//        then
//            let op2 : Elm = new Elm(opcode.source,opcode.elem,op.t,opcode.s,opcode.sz,op.n,opcode.m1,opcode.p1,opcode.b1,opcode.v1,opcode.ev1,opcode.l1,
//                                    opcode.m2,opcode.p2,opcode.b2,opcode.v2,opcode.ev2,opcode.l2,op.template,op.code,op.prn)
//            o <- opcode.prn op2
//    o



//type Op =

//    new () = {}

    //member this.op_adc( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xa9; lo v |], "adc a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xb9; lo v |],  "adc a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xc9; hi v; lo v |],  "adc a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xf9 |], "adc a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xe9; lo v |], "adc a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xd9; hi v; lo v |], "adc a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf9 |], "adc a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe9; lo v |], "adc a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd9; hi v; lo v |], "adc a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x19; lo v |], "adc a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc9; lo v |],"adc a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc9; hi v; lo v |],"adc a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd9; lo v |],"adc a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd9; hi v; lo v |],"adc a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd9; lo v |],"adc a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_add( f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   f = "a" && a = IMMEDIATE && v < 256 then ([| 0xab; lo v |], "add a,#"+ (lo v |> string))
    //    elif f = "a" && a = DIRECT && p = REGRAM then ([| 0xbb; lo v |],  "add a,"+ (lo v |> string))
    //    elif f = "a" && a = DIRECT && p = RAM then ([| 0xcb; hi v; lo v |],  "add a,"+ (v |> string))
    //    elif f = "a" && a = INDEXED && b = "x" && v = 0 then ([| 0xfb |], "add a,(x)")
    //    elif f = "a" && a = INDEXED && b = "x" && v < 256 then ([| 0xeb; lo v |], "add a,("+ (lo v |> string)+",x)")
    //    elif f = "a" && a = INDEXED && b = "x" && v < 65536 then ([|0xdb; hi v; lo v |], "add a,("+ (v |> string)+",x)")
    //    elif f = "a" && a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xfb |], "add a,(y)")
    //    elif f = "a" && a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xeb; lo v |], "add a,("+ (lo v |> string)+",y)")
    //    elif f = "a" && a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xdb; hi v; lo v |], "add a,("+ (v |> string)+",y)")
    //    elif f = "a" && a = INDEXED && b = "sp" && v < 256 then ([| 0x1b; lo v |], "add a,("+ (lo v |> string)+",sp)")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xcb; lo v |],"add a,["+ (lo v |> string)+"]")
    //    elif f = "a" && a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xcb; hi v; lo v |],"add a,["+ (v |> string)+"]")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xdb; lo v |],"add a,(["+ (lo v |> string)+"],x)")
    //    elif f = "a" && a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xdb; hi v; lo v |],"add a,(["+ (v |> string)+"],x)")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xdb; lo v |],"add a,(["+ (lo v |> string)+"],y)")
    //    elif f = "x" && a = IMMEDIATE && v < 65536  then ([| 0x1c; hi v; lo v |], "addw x,#"+ (v |> string))
    //    elif f = "x" && a = DIRECT then ([| 0x72; 0xbb; hi v; lo v |], "addw x,"+ (v |> string))
    //    elif f = "x" && a = INDEXED && b = "sp" && v < 256  then ([| 0x72; 0xfb; lo v |],"addw x,("+ (lo v |> string)+",sp)")
    //    elif f = "y" && a = IMMEDIATE && v < 65536 then ([| 0x72; 0xa9; hi v; lo v |], "addw y,#"+ (v |> string))
    //    elif f = "y" && a = DIRECT then ([| 0x72; 0xb9; hi v; lo v |], "addw y,"+ (v |> string))
    //    elif f = "y" && a = INDEXED && b = "sp" && v < 256 then ([| 0x72; 0xf9; lo v |],"addw y,("+ (lo v |> string)+",sp)")
    //    elif f = "sp" && a = IMMEDIATE && v < 256 then ([| 0x5b; lo v |], "addw sp,"+ ( lo v |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"
    
    //member this.op_and( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xa4; lo v |], "and a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xb4; lo v |],  "and a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xc4; hi v; lo v |],  "and a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xf4 |], "and a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xe4; lo v |], "and a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xd4; hi v; lo v |], "and a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf4 |], "and a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe4; lo v |], "and a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd4; hi v; lo v |], "and a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x14; lo v |], "and a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc4; lo v |],"and a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc4; hi v; lo v |],"and a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd4; lo v |],"and a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd4; hi v; lo v |],"and a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd4; lo v |],"and a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

// bccm

    //member this.op_bcp( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xa5; lo v |], "bcp a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xb5; lo v |],  "bcp a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xc5; hi v; lo v |],  "bcp a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xf5 |], "bcp a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xe5; lo v |], "bcp a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xd5; hi v; lo v |], "bcp a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf5 |], "bcp a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe5; lo v |], "bcp a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd5; hi v; lo v |], "bcp a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x15; lo v |], "bcp a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc5; lo v |],"bcp a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc5; hi v; lo v |],"bcp a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd5; lo v |],"bcp a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd5; hi v; lo v |],"bcp a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd5; lo v |],"bcp a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_bcpl( a : Addressing, p : Place, b : string, v : int, m : int) =
    //    if   a = BITOPERATION && v < 65536 && m < 8 then ([| 0x90; (0x10+2*m); hi v; lo v |], "bcpl "+ (lo v |> string)+",#"+ (m |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_break = ([0x8b],"break")

    //member this.op_bres( a : Addressing, p : Place, b : string, v : int, m : int) =
    //    if   a = BITOPERATION && v < 65536 && m < 8 then ([| 0x72; (0x11+2*m); hi v; lo v |], "bres "+ (lo v |> string)+",#"+ (m |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_bset( a : Addressing, p : Place, b : string, v : int, m : int) =
    //    if   a = BITOPERATION && v < 65536 && m < 8 then ([| 0x72; (0x10+2*m); hi v; lo v |], "bset "+ (lo v |> string)+",#"+ (m |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

// btjf - cond jmp
// btjt - cond jmp

// call/callf/callr

    //member this.op_ccf = ([| 0x8c |],"ccf")

    //member this.op_clr( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if a = BITOPERATION then this.op_bres(a,p,b,v,m |> int)
    //    elif a = R8 && b = "a" then ([| 0x4f |], "clr a")
    //    elif a = R16 && b = "x" then ([| 0x5f |], "clrw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x5f |], "clrw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x3f; lo v |],  "clr "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x5f; hi v; lo v |],  "clr "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x7f |], "clr (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x6f; lo v |], "clr ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x4f; hi v; lo v |], "clr ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x7f |], "clr (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x6f; lo v |], "clr ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x4f; hi v; lo v |], "clr ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x0f; lo v |], "clr ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x3f; lo v |],"clr ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x3f; hi v; lo v |],"clr ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x6f; lo v |],"clr (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x6f; hi v; lo v |],"clr (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x6f; lo v |],"clr (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"
    (*
    member this.op_cp( f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
        if   f = "a" && a = IMMEDIATE && v < 256 then ([| 0xa1; lo v |], "cp a,#"+ (lo v |> string))
        elif f = "a" && a = DIRECT && p = REGRAM then ([| 0xb1; lo v |],  "cp a,"+ (lo v |> string))
        elif f = "a" && a = DIRECT && p = RAM then ([| 0xc1; hi v; lo v |],  "cp a,"+ (v |> string))
        elif f = "a" && a = INDEXED && b = "x" && v = 0 then ([| 0xf1 |], "cp a,(x)")
        elif f = "a" && a = INDEXED && b = "x" && v < 256 then ([| 0xe1; lo v |], "cp a,("+ (lo v |> string)+",x)")
        elif f = "a" && a = INDEXED && b = "x" && v < 65536 then ([|0xd1; hi v; lo v |], "cp a,("+ (v |> string)+",x)")
        elif f = "a" && a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf1 |], "cp a,(y)")
        elif f = "a" && a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe1; lo v |], "cp a,("+ (lo v |> string)+",y)")
        elif f = "a" && a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd1; hi v; lo v |], "cp a,("+ (v |> string)+",y)")
        elif f = "a" && a = INDEXED && b = "sp" && v < 256 then ([| 0x11; lo v |], "cp a,("+ (lo v |> string)+",sp)")
        elif f = "a" && a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc1; lo v |],"cp a,["+ (lo v |> string)+"]")
        elif f = "a" && a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc1; hi v; lo v |],"cp a,["+ (v |> string)+"]")
        elif f = "a" && a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd1; lo v |],"cp a,(["+ (lo v |> string)+"],x)")
        elif f = "a" && a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd1; hi v; lo v |],"cp a,(["+ (v |> string)+"],x)")
        elif f = "a" && a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd1; lo v |],"cp a,(["+ (lo v |> string)+"],y)")
        elif f = "x" && a = IMMEDIATE && v < 65536  then ([| 0xa3; hi v; lo v |], "cpw x,#"+ (v |> string))
        elif f = "x" && a = DIRECT && p = REGRAM && v < 256 then ([| 0xb3; lo v |], "cpw x,"+ (v |> string))
        elif f = "x" && a = DIRECT && p = RAM && v < 65536 then ([| 0xc3; hi v; lo v |], "cpw x,"+ (v |> string))
        elif f = "x" && a = INDEXED && b = "y" && v = 0  then ([| 0x90; 0xf3 |],"cpw x,(y)")
        elif f = "x" && a = INDEXED && b = "y" && v < 256  then ([| 0x90; 0xe3; lo v |],"cpw x,("+ (lo v |> string)+",y)")
        elif f = "x" && a = INDEXED && b = "y" && v < 65535  then ([| 0x90; 0xd3; hi v; lo v |],"cpw x,("+ (v |> string)+",y)")
        elif f = "x" && a = INDEXED && b = "sp" && v < 256  then ([| 0x72; 0xfb; lo v |],"cpw x,("+ (lo v |> string)+",sp)")
        elif f = "x" && a = INDIRECT && p = REGRAM && b = "" && v < 256 && m = "w" then ([| 0x92; 0xc3; lo v |],"cpw x,["+ (v |> string)+"]")
        elif f = "x" && a = INDIRECT && p = RAM && b = "" && v < 65536 && m = "w" then ([| 0x72; 0xc3; hi v; lo v |],"cpw x,["+ (v |> string)+"]")
        elif f = "x" && a = INDIRECT && p = REGRAM && b = "y" && v < 256 && m = "w" then ([| 0x91; 0xd3; lo v |],"cpw x,(["+ (v |> string)+"],y)")
        elif f = "y" && a = IMMEDIATE && v < 65536  then ([| 0x90; 0xa3; hi v; lo v |], "cpw y,#"+ (v |> string))
        elif f = "y" && a = DIRECT && p = REGRAM && v < 256 then ([| 0x90; 0xb3; lo v |], "cpw y,"+ (v |> string))
        elif f = "y" && a = DIRECT && p = RAM && v < 65536 then ([| 0x90; 0xc3; hi v; lo v |], "cpw y,"+ (v |> string))
        elif f = "y" && a = INDEXED && b = "x" && v = 0  then ([| 0xf3 |],"cpw y,(x)")
        elif f = "y" && a = INDEXED && b = "x" && v < 256  then ([| 0xe3; lo v |],"cpw y,("+ (lo v |> string)+",x)")
        elif f = "y" && a = INDEXED && b = "x" && v < 65535  then ([| 0xd3; hi v; lo v |],"cpw y,("+ (v |> string)+",x)")
        elif f = "x" && a = INDEXED && b = "sp" && v < 256  then ([| 0x51; 0x72; 0xfb; lo v; 0x51 |],"exgw x,y\ncpw x,("+ (lo v |> string)+",sp)\nexgw x,y")
        elif f = "y" && a = INDIRECT && p = REGRAM && b = "" && v < 256 && m = "w" then ([| 0x91; 0xc3; lo v |],"cpw y,["+ (v |> string)+"]")
        elif f = "y" && a = INDIRECT && p = REGRAM && b = "x" && v < 256 && m = "w" then ([| 0x92; 0xd3; lo v |],"cpw y,(["+ (lo v |> string)+"],x)")
        elif f = "y" && a = INDIRECT && p = REGRAM && b = "x" && v < 65535 && m = "w" then ([| 0x72; 0xd3; hi v; lo v |],"cpw y,(["+ (v |> string)+"],x)")
        else failwith "Error: incorrect command-parameter-addressing combination"
*)
    //member this.op_cpl( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x43 |], "cpl a")
    //    elif a = R16 && b = "x" then ([| 0x53 |], "cplw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x53 |], "cplw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x33; lo v |],  "cpl "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x53; hi v; lo v |],  "cpl "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x73 |], "cpl (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x63; lo v |], "cpl ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x43; hi v; lo v |], "cpl ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x73 |], "cpl (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x63; lo v |], "cpl ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x43; hi v; lo v |], "cpl ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x03; lo v |], "cpl ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x33; lo v |],"cpl ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x33; hi v; lo v |],"cpl ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x63; lo v |],"cpl (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x63; hi v; lo v |],"cpl (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x63; lo v |],"cpl (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_dec( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x4a |], "dec a")
    //    elif a = R16 && b = "x" then ([| 0x5a |], "decw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x5a |], "decw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x3a; lo v |],  "dec "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x5a; hi v; lo v |],  "dec "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x7a |], "dec (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x6a; lo v |], "dec ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x4a; hi v; lo v |], "dec ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x7a |], "dec (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x6a; lo v |], "dec ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x4a; hi v; lo v |], "dec ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x0a; lo v |], "dec ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x3a; lo v |],"dec ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x3a; hi v; lo v |],"dec ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x6a; lo v |],"dec (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x6a; hi v; lo v |],"dec (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x6a; lo v |],"dec (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_div(  f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   f = "x" && b = "a" then ([| 0x62 |], "div x,a")
    //    elif f = "y" && b = "a" then ([| 0x90; 0x62 |], "div y,a")
    //    elif f = "x" && b = "y" then ([| 0x90; 0x5a |], "divx x,y")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_exg(  f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   f = "a" && b = "xl" then ([| 0x41 |], "exg a,xl")
    //    elif f = "a" && b = "yl" then ([| 0x61; |], "exg a,yl")
    //    elif (f = "x" && b = "y") || (f = "y" && b = "x")  then ([| 0x51; |], "exgw x,y")
    //    elif a = DIRECT then ([| 0x31; hi v; lo v |],  "exg a,"+ (v |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_halt = ([| 0x8e |],"halt")

    //member this.op_inc( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x4c |], "inc a")
    //    elif a = R16 && b = "x" then ([| 0x5c |], "incw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x5c |], "incw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x3c; lo v |],  "inc "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x5c; hi v; lo v |],  "inc "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x7c |], "inc (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x6c; lo v |], "inc ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x4c; hi v; lo v |], "inc ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x7c |], "inc (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x6c; lo v |], "inc ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x4c; hi v; lo v |], "inc ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x0c; lo v |], "inc ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x3c; lo v |],"inc ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x3c; hi v; lo v |],"inc ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x6c; lo v |],"inc (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x6c; hi v; lo v |],"inc (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x6c; lo v |],"inc (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

// int

    //member this.op_iret = ([| 0x80 |],"iret")

// jp/jpf/jra

// jrxx

//ld/ldf/ldw/mov


(*
    member this.op_to(  a1 : Addressing, p1 : Place, b1 : string, v1 : int, m1 : string, a2 : Addressing, p2 : Place, b2 : string, v2 : int, m2 : string) =
        if a1 = BITOPERATION && a2 = IMMEDIATE && v2 = 1 then this.op_bset(a1,p1,b1,v1,m1 |> int)
        elif a2 = IMMEDIATE && v2 = 0 then this.op_clr(a1,p1,b1,v1,m1)
        elif a1 = DIRECT && a2 = IMMEDIATE then ([| 0x35; lo v1; hi v2; lo v2 |],  "mov "+ ( v1 |> string)+",#"+( lo v2 |> string))
        elif a1 = DIRECT && p1 = REGRAM && a2 = DIRECT && p2 = REGRAM then ([| 0x45; lo v2; lo v1 |],  "mov "+ ( lo v1 |> string)+","+( lo v2 |> string))
        elif a1 = DIRECT && a2 = DIRECT then ([| 0x55; hi v2; lo v2; hi v1; lo v1 |],  "mov "+ ( v1 |> string)+","+( v2 |> string))
        elif a1 = R8 && b1 = "a" && a2 = IMMEDIATE then ([| 0xa6; lo v2 |],  "ld a,#" + ( lo v2 |> string))
        elif a1 = R8 && b1 = "a" && a2 = DIRECT && p2 = REGRAM then ([| 0xb6; lo v2 |],  "ld a," + ( lo v2 |> string))
        elif a1 = R8 && b1 = "a" && a2 = DIRECT && v2 < 65536 then ([| 0xc6; hi v2; lo v2 |],  "ld a," + ( v2 |> string))
        elif a1 = R8 && b1 = "a" && a2 = DIRECT then ([| 0xbc; xhi v2; hi v2; lo v2 |],  "ldf a," + ( v2 |> string))
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "x" && v2 = 0 then ([| 0xf6 |],  "ld a,(x)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "x" && v2 < 256 then ([| 0xe6; lo v2 |], "ld  a,("+ (lo v2 |> string)+",x)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "x" && v2 < 65536 then ([| 0xd6; hi v2; lo v2 |], "ld a,("+ (v2 |> string)+",x)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "x" then ([| 0xaf; xhi v2; hi v2; lo v2 |], "ldf a,("+ (v2 |> string)+",x)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "y" && v2 = 0 then ([| 0x90; 0xf6 |],  "ld a,(y)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "y" && v2 < 256 then ([| 0x90; 0xe6; lo v2 |], "ild a,("+ (lo v2 |> string)+",y)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "y" && v2 < 65536 then ([| 0x90; 0xd6; hi v2; lo v2 |], "ld a,("+ (v2 |> string)+",y)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "y" then ([| 0x90; 0xaf; xhi v2; xhi v2; hi v2; lo v2 |], "ldf a,("+ (v2 |> string)+",y)")
        elif a1 = R8 && b1 = "a" && a2 = INDEXED && b2 = "sp" && v2 < 256 then ([| 0x7b; lo v2 |], "ld a,("+ (lo v2 |> string)+",sp)")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && p2 = REGRAM && b2 = "" && m2 = "w" then ([| 0x92; 0xc6; lo v2 |],"ld a,["+ (lo v2 |> string)+"]")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && p2 = RAM && b2 = "" && m2 = "w"  then ([| 0x72; 0xc6; hi v2; lo v2 |],"ld a,["+ ( v2 |> string)+"]")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && p2 = REGRAM && b2 = "x" && m2 = "w" then ([| 0x92; 0xd6; lo v2 |],"ld a,(["+ (lo v2 |> string)+"],x)")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && p2 = RAM && b2 = "x" && m2 = "w" then ([| 0x72; 0xd6; hi v2; lo v2 |],"ld a,(["+ ( v2 |> string)+"],x)")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && p2 = REGRAM && b2 = "y" && m2 = "w" then ([| 0x91; 0xd6; lo v2 |],"ld a,(["+ ( lo v2 |> string)+"],y)")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && b2 = "" && m2 = "e" then ([| 0x92; 0xbc; hi v2; lo v2 |],"ldf a,["+ ( v2 |> string)+"]")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && b2 = "x" && m2 = "e" then ([| 0x92; 0xaf; hi v2; lo v2 |],"ldf a,(["+ ( v2 |> string)+"],x)")
        elif a1 = R8 && b1 = "a" && a2 = INDIRECT && b2 = "y" && m2 = "e" then ([| 0x91; 0xaf; hi v2; lo v2 |],"ldf a,(["+ ( v2 |> string)+"],y)")
        elif a1 = R8 && b1 = "a" && a2 = R8 && b2 = "xl" then ([| 0x9f |], "ld a,xl")
        elif a1 = R8 && b1 = "a" && a2 = R8 && b2 = "yl" then ([| 0x90; 0x9f |], "ld a,yl")
        elif a1 = R8 && b1 = "a" && a2 = R8 && b2 = "xh" then ([| 0x9e |], "ld a,xh")
        elif a1 = R8 && b1 = "a" && a2 = R8 && b2 = "yh" then ([| 0x90; 0x9e |], "ld a,yh")
        elif a1 = R8 && b1 = "xl" && a2 = R8 && b2 = "a" then ([| 0x97 |], "ld xl,a")
        elif a1 = R8 && b1 = "yl" && a2 = R8 && b2 = "a" then ([| 0x90; 0x97 |], "ld yl,a")
        elif a1 = R8 && b1 = "xh" && a2 = R8 && b2 = "a" then ([| 0x95 |], "ld xh,a")
        elif a1 = R8 && b1 = "yh" && a2 = R8 && b2 = "a" then ([| 0x90; 0x95 |], "ld yh,a")
        elif a2 = R8 && b2 = "a" && a1 = DIRECT && p1 = REGRAM then ([| 0xb6; lo v1 |],  "ld " + ( lo v1 |> string) + ",a")
        elif a2 = R8 && b2 = "a" && a1 = DIRECT && v2 < 65535 then ([| 0xc6; hi v1; lo v1 |],  "ld " + ( v1 |> string)+",a")
        elif a2 = R8 && b2 = "a" && a1 = DIRECT then ([| 0xbd; xhi v1; hi v1; lo v1 |],  "ldf " + ( v1 |> string) + ",a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "x" && v1 = 0 then ([| 0xf6 |],  "ld (x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "x" && v1 < 256 then ([| 0xe6; lo v1 |], "ld ("+ (lo v1 |> string)+",x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "x" && v1 < 65536 then ([| 0xd6; hi v1; lo v1 |], "ld ("+ (v1 |> string)+",x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "x" then ([| 0xa7; xhi v1; hi v1; lo v1 |], "ldf ("+ (v1 |> string)+",x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "y" && v1 = 0 then ([| 0x90; 0xf6 |],  "ld (y),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "y" && v1 < 256 then ([| 0x90; 0xe6; lo v1 |], "ild ("+ (lo v1 |> string)+",y),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "y" && v1 < 65536 then ([| 0x90; 0xd6; hi v1; lo v1 |], "ld ("+ (v1 |> string)+",y),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "y" then ([| 0x90; 0xa7; xhi v1; hi v1; lo v1 |], "ldf ("+ (v1 |> string)+",y),a")
        elif a2 = R8 && b2 = "a" && a1 = INDEXED && b1 = "sp" && v1 < 256 then ([| 0x7b; lo v1 |], "ld ("+ (lo v1 |> string)+",sp),a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && p1 = REGRAM && b1 = "" && m1 = "w" then ([| 0x92; 0xc6; lo v1 |],"ld ["+ (lo v1 |> string)+"],a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && p1 = RAM && b1 = "" && m1 = "w"  then ([| 0x72; 0xc6; hi v1; lo v1 |],"ld ["+ (v1 |> string)+"],a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && p1 = REGRAM && b1 = "x" && m1 = "w" then ([| 0x92; 0xd6; lo v1 |],"ld (["+ (lo v1 |> string)+"],x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && p1 = RAM && b1 = "x" && m1 = "w" then ([| 0x72; 0xd6; hi v1; lo v1 |],"ld (["+ (v1 |> string)+"],x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && p1 = REGRAM && b1 = "y" && m1 = "w" then ([| 0x91; 0xd6; lo v1 |],"ld (["+ (lo v1 |> string)+"],y),a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && b1 = "" && m1 = "e" then ([| 0x92; 0xbd; hi v1; lo v1 |],"ldf ["+ ( v1 |> string)+"],a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && b1 = "x" && m1 = "e" then ([| 0x92; 0xa7; hi v1; lo v1 |],"ldf (["+ ( v1 |> string)+"],x),a")
        elif a2 = R8 && b2 = "a" && a1 = INDIRECT && b1 = "y" && m1 = "e" then ([| 0x91; 0xa7; hi v1; lo v1 |],"ldf (["+ ( v1 |> string)+"],y),a")      
        elif a1 = R16 && b1 = "x" && a2 = R16 && b2 = "y" then ([| 0x93 |], "ldw x,y")
        elif a1 = R16 && b1 = "x" && a2 = R16 && b2 = "sp" then ([| 0x96 |], "ldw x,sp")
        elif a1 = R16 && b1 = "y" && a2 = R16 && b2 = "x" then ([| 0x90; 0x93 |], "ldw y,x")
        elif a1 = R16 && b1 = "y" && a2 = R16 && b2 = "sp" then ([| 0x90; 0x96 |], "ldw y,sp")
        elif a1 = R16 && b1 = "sp" && a2 = R16 && b2 = "x" then ([| 0x94 |], "ldw sp,x")
        elif a1 = R16 && b1 = "sp" && a2 = R16 && b2 = "y" then ([| 0x90; 0x94 |], "ldw sp,y")
        elif a1 = R16 && b1 = "x" && a2 = IMMEDIATE then ([| 0xae; hi v2; lo v2 |],  "ldw x,#" + ( v2 |> string))
        elif a1 = R16 && b1 = "x" && a2 = DIRECT && p2 = REGRAM then ([| 0xbe; lo v2 |],  "ldw x," + ( lo v2 |> string))
        elif a1 = R16 && b1 = "x" && a2 = DIRECT && v2 < 65536 then ([| 0xce; hi v2; lo v2 |],  "ldw x," + ( v2 |> string))
        elif a1 = R16 && b1 = "x" && a2 = INDEXED && b2 = "x" && v2 = 0 then ([| 0xfe |],  "ldw x,(x)")
        elif a1 = R16 && b1 = "x" && a2 = INDEXED && b2 = "x" && v2 < 256 then ([| 0xee; lo v2 |],  "ldw x,("+( lo v2 |> string)+",x)")
        elif a1 = R16 && b1 = "x" && a2 = INDEXED && b2 = "x" && v2 < 65536 then ([| 0xde; hi v2; lo v2 |],  "ldw x,("+( v2 |> string)+",x)")
        elif a1 = R16 && b1 = "x" && a2 = INDEXED && b2 = "sp" && v2 < 256 then ([| 0x1e; lo v2 |],  "ldw x,("+( lo v2 |> string)+",sp)")
        elif a1 = R16 && b1 = "x" && a2 = INDIRECT && p2 = REGRAM && b2 = "" && m2 = "w" then ([| 0x92; 0xce; lo v2 |],"ldw x,["+ (lo v2 |> string)+"]")
        elif a1 = R16 && b1 = "x" && a2 = INDIRECT && p2 = RAM && b2 = "" && m2 = "w"  then ([| 0x72; 0xce; hi v2; lo v2 |],"ldw x,["+ ( v2 |> string)+"]")
        elif a1 = R16 && b1 = "x" && a2 = INDIRECT && p2 = REGRAM && b2 = "x" && m2 = "w" then ([| 0x92; 0xde; lo v2 |],"ldw x,(["+ (lo v2 |> string)+"],x)")
        elif a1 = R16 && b1 = "x" && a2 = INDIRECT && p2 = RAM && b2 = "x" && m2 = "w" then ([| 0x72; 0xde; hi v2; lo v2 |],"ldw x,(["+ (v2 |> string)+"],x)")
        elif a2 = R16 && b2 = "x" && a1 = DIRECT && p1 = REGRAM then ([| 0xbf; lo v2 |],  "ldw " + ( lo v2 |> string) + ",x")
        elif a2 = R16 && b2 = "x" && a1 = DIRECT && v1 < 65536 then ([| 0xcf; hi v2; lo v2 |],  "ldw " + ( v2 |> string) + ",x")
        elif a2 = R16 && b2 = "y" && a1 = INDEXED && b1 = "x" && v1 = 0 then ([| 0xff |],  "ldw (x),y")
        elif a2 = R16 && b2 = "y" && a1 = INDEXED && b1 = "x" && v1 < 256 then ([| 0xef; lo v2 |],  "ldw ("+( lo v2 |> string)+",x),y")
        elif a2 = R16 && b2 = "y" && a1 = INDEXED && b1 = "x" && v1 < 65536 then ([| 0xdf; hi v2; lo v2 |],  "ldw ("+( v2 |> string)+",x),y")
        elif a2 = R16 && b2 = "x" && a1 = INDEXED && b1 = "sp" && v1 < 256 then ([| 0x1f; lo v2 |],  "ldw ("+( lo v2 |> string)+",sp),x")
        elif a2 = R16 && b2 = "x" && a1 = INDIRECT && p1 = REGRAM && b1 = "" && m1 = "w" then ([| 0x92; 0xcf; lo v2 |],"ldw ["+ (lo v2 |> string)+"],x")
        elif a2 = R16 && b2 = "x" && a1 = INDIRECT && p1 = RAM && b1 = "" && m1 = "w"  then ([| 0x72; 0xcf; hi v2; lo v2 |],"ldw ["+ ( v2 |> string)+"],x")
        elif a2 = R16 && b2 = "y" && a1 = INDIRECT && p1 = REGRAM && b1 = "x" && m1 = "w" then ([| 0x92; 0xdf; lo v2 |],"ldw (["+ (lo v2 |> string)+"],x),y")
        elif a2 = R16 && b2 = "y" && a1 = INDIRECT && p1 = RAM && b1 = "x" && m1 = "w" then ([| 0x72; 0xdf; hi v2; lo v2 |],"ldw (["+ (v2 |> string)+"],x),y")

        elif a1 = R16 && b1 = "y" && a2 = IMMEDIATE then ([| 0x90; 0xae; hi v2; lo v2 |],  "ldw y,#" + ( v2 |> string))
        elif a1 = R16 && b1 = "y" && a2 = DIRECT && p2 = REGRAM then ([| 0x90; 0xbe; lo v2 |],  "ldw y," + ( lo v2 |> string))
        elif a1 = R16 && b1 = "y" && a2 = DIRECT && v2 < 65536 then ([| 0x90; 0xce; hi v2; lo v2 |],  "ldw y," + ( v2 |> string))
        elif a1 = R16 && b1 = "y" && a2 = INDEXED && b2 = "x" && v2 = 0 then ([| 0x90; 0xfe |],  "ldw y,(x)")
        elif a1 = R16 && b1 = "y" && a2 = INDEXED && b2 = "x" && v2 < 256 then ([| 0x90; 0xee; lo v2 |],  "ldw y,("+( lo v2 |> string)+",x)")
        elif a1 = R16 && b1 = "y" && a2 = INDEXED && b2 = "x" && v2 < 65536 then ([| 0x90; 0xde; hi v2; lo v2 |],  "ldw y,("+( v2 |> string)+",x)")
        elif a1 = R16 && b1 = "y" && a2 = INDEXED && b2 = "sp" && v2 < 256 then ([| 0x16; lo v2 |],  "ldw y,("+( lo v2 |> string)+",sp)")
        elif a1 = R16 && b1 = "y" && a2 = INDIRECT && p2 = REGRAM && b2 = "" && m2 = "w" then ([| 0x91; 0xce; lo v2 |],"ldw y,["+ (lo v2 |> string)+"]")
        elif a1 = R16 && b1 = "y" && a2 = INDIRECT && p2 = RAM && b2 = "" && m2 = "w"  then ([| 0x51; 0x72; 0xce; hi v2; lo v2; 0x51 |],"exgw x,y\nldw x,["+ ( v2 |> string)+"]\nexgw x,y")
        elif a1 = R16 && b1 = "y" && a2 = INDIRECT && p2 = REGRAM && b2 = "y" && m2 = "w" then ([| 0x91; 0xde; lo v2 |],"ldw y,(["+ (lo v2 |> string)+"],y)")
        //elif a1 = R16 && b1 = "y" && a2 = INDIRECT && p2 = RAM && b2 = "y" && m2 = "w" then ([| 0x72; 0xde; hi v2; lo v2 |],"ldw y,(["+ (v2 |> string)+"],y)")
        elif a2 = R16 && b2 = "y" && a1 = DIRECT && p1 = REGRAM then ([| 0x90; 0xbf; lo v2 |],  "ldw " + ( lo v2 |> string) + ",y")
        elif a2 = R16 && b2 = "y" && a1 = DIRECT && v1 < 65536 then ([| 0x90; 0xcf; hi v2; lo v2 |],  "ldw " + ( v2 |> string) + ",y")
        elif a2 = R16 && b2 = "x" && a1 = INDEXED && b1 = "y" && v1 = 0 then ([| 0x90; 0xff |],  "ldw (y),x")
        elif a2 = R16 && b2 = "x" && a1 = INDEXED && b1 = "y" && v1 < 256 then ([| 0x90; 0xef; lo v2 |],  "ldw ("+( lo v2 |> string)+",y),x")
        elif a2 = R16 && b2 = "x" && a1 = INDEXED && b1 = "y" && v1 < 65536 then ([| 0x90; 0xdf; hi v2; lo v2 |],  "ldw ("+( v2 |> string)+",y),x")
        elif a2 = R16 && b2 = "y" && a1 = INDEXED && b1 = "sp" && v1 < 256 then ([| 0x17; lo v2 |],  "ldw ("+( lo v2 |> string)+",sp),y")
        elif a2 = R16 && b2 = "y" && a1 = INDIRECT && p1 = REGRAM && b1 = "" && m1 = "w" then ([| 0x91; 0xcf; lo v2 |],"ldw ["+ (lo v2 |> string)+"],y")
        elif a2 = R16 && b2 = "y" && a1 = INDIRECT && p1 = RAM && b1 = "" && m1 = "w"  then ([| 0x51; 0x72; 0xcf; hi v2; lo v2; 0x51 |],"exgw x,y\nldw ["+ ( v2 |> string)+"],x\nexgw x,y")
        elif a2 = R16 && b2 = "x" && a1 = INDIRECT && p1 = REGRAM && b1 = "y" && m1 = "w" then ([| 0x91; 0xdf; lo v2 |],"ldw (["+ (lo v2 |> string)+"],y),x")
        //elif a2 = R16 && b2 = "x" && a1 = INDIRECT && p1 = RAM && b1 = "y" && m1 = "w" then ([| 0x91; 0xdf; hi v2; lo v2 |],"ldw (["+ (v2 |> string)+"],x),y")
        /// ldw continued
        else failwith "Error: incorrect command-parameter-addressing combination"


    //member this.op_mul(  f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   f = "x" && b = "a" then ([| 0x42 |], "mul x,a")
    //    elif f = "y" && b = "a" then ([| 0x90; 0x42 |], "mul y,a")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_neg( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x40 |], "neg a")
    //    elif a = R16 && b = "x" then ([| 0x50 |], "negw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x50 |], "negw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x30; lo v |],  "neg "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x50; hi v; lo v |],  "neg "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x70 |], "neg (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x60; lo v |], "neg ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x40; hi v; lo v |], "neg ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x70 |], "neg (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x60; lo v |], "neg ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x40; hi v; lo v |], "neg ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x00; lo v |], "neg ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x30; lo v |],"neg ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x30; hi v; lo v |],"neg ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x60; lo v |],"neg (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x60; hi v; lo v |],"neg (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x60; lo v |],"neg (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_nop = ([| 0x9d |],"nop")

    //member this.op_or( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xaa; lo v |], "or a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xba; lo v |],  "or a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xca; hi v; lo v |],  "or a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xfa |], "or a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xea; lo v |], "or a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xda; hi v; lo v |], "or a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xfa |], "or a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xea; lo v |], "or a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xda; hi v; lo v |], "or a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x1a; lo v |], "or a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xca; lo v |],"or a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xca; hi v; lo v |],"or a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xda; lo v |],"or a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xda; hi v; lo v |],"or a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xda; lo v |],"or a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_pop(  a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x84 |], "pop a")
    //    elif a = R8 && b = "cc" then ([| 0x86 |], "pop cc")
    //    elif a = R16 && b = "x" then ([| 0x85 |], "popw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x85 |], "popw y")
    //    elif a = DIRECT then ([| 0x32; hi v; lo v |],  "pop "+ (v |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_push(  a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x88 |], "push a")
    //    elif a = R8 && b = "cc" then ([| 0x8a |], "push cc")
    //    elif a = R16 && b = "x" then ([| 0x89 |], "pushw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x89 |], "pushw y")
    //    elif a = IMMEDIATE && v < 256  then ([| 0x3b; lo v |],  "push #"+ (lo v |> string))
    //    elif a = DIRECT then ([| 0x3b; hi v; lo v |],  "push "+ (v |> string))
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_rcf = ([| 0x98 |],"rcf")

    //member this.op_ret = ([| 0x81 |],"ret")

    //member this.op_retf = ([|0x87 |],"retf")

    //member this.op_rim = ([| 0x9a |],"rim")

    member this.op_rlc( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   a = R8 && b = "a" then ([| 0x49 |], "rlc a")
        elif a = R16 && b = "x" then ([| 0x59 |], "rlcw x")
        elif a = R16 && b = "y" then ([| 0x90; 0x59 |], "rlcw y")
        elif a = DIRECT && p = REGRAM then ([| 0x39; lo v |],  "rlc "+ (lo v |> string))
        elif a = DIRECT && p = RAM then ([| 0x72; 0x59; hi v; lo v |],  "rlc "+ (v |> string))
        elif a = INDEXED && b = "x" && v = 0 then ([| 0x79 |], "rlc (x)")
        elif a = INDEXED && b = "x" && v < 256 then ([| 0x69; lo v |], "rlc ("+ (lo v |> string)+",x)")
        elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x49; hi v; lo v |], "rlc ("+ (v |> string)+",x)")
        elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x79 |], "rlc (y)")
        elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x69; lo v |], "rlc ("+ (lo v |> string)+",y)")
        elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x49; hi v; lo v |], "rlc ("+ (v |> string)+",y)")
        elif a = INDEXED && b = "sp" && v < 256 then ([| 0x09; lo v |], "rlc ("+ (lo v |> string)+",sp)")
        elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x39; lo v |],"rlc ["+ (lo v |> string)+"]")
        elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x39; hi v; lo v |],"rlc ["+ (v |> string)+"]")
        elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x69; lo v |],"rlc (["+ (lo v |> string)+"],x)")
        elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x69; hi v; lo v |],"rlc (["+ (v |> string)+"],x)")
        elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x69; lo v |],"rlc (["+ (lo v |> string)+"],y)")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_rlwa(  f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
        if   f = "x" && b = "a" then ([| 0x02 |], "rlwa x,a")
        elif f = "y" && b = "a" then ([| 0x90; 0x02 |], "rlwa y,a")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_rrc( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   a = R8 && b = "a" then ([| 0x46 |], "rrc a")
        elif a = R16 && b = "x" then ([| 0x56 |], "rrcw x")
        elif a = R16 && b = "y" then ([| 0x90; 0x56 |], "rrcw y")
        elif a = DIRECT && p = REGRAM then ([| 0x36; lo v |],  "rrc "+ (lo v |> string))
        elif a = DIRECT && p = RAM then ([| 0x72; 0x56; hi v; lo v |],  "rrc "+ (v |> string))
        elif a = INDEXED && b = "x" && v = 0 then ([| 0x76 |], "rrc (x)")
        elif a = INDEXED && b = "x" && v < 256 then ([| 0x66; lo v |], "rrc ("+ (lo v |> string)+",x)")
        elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x46; hi v; lo v |], "rrc ("+ (v |> string)+",x)")
        elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x76 |], "rrc (y)")
        elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x66; lo v |], "rrc ("+ (lo v |> string)+",y)")
        elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x46; hi v; lo v |], "rrc ("+ (v |> string)+",y)")
        elif a = INDEXED && b = "sp" && v < 256 then ([| 0x06; lo v |], "rrc ("+ (lo v |> string)+",sp)")
        elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x36; lo v |],"rrc ["+ (lo v |> string)+"]")
        elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x36; hi v; lo v |],"rrc ["+ (v |> string)+"]")
        elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x66; lo v |],"rrc (["+ (lo v |> string)+"],x)")
        elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x66; hi v; lo v |],"rrc (["+ (v |> string)+"],x)")
        elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x66; lo v |],"rrc (["+ (lo v |> string)+"],y)")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_rrwa(  f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
        if   f = "x" && b = "a" then ([| 0x01 |], "rrwa x,a")
        elif f = "y" && b = "a" then ([| 0x90; 0x01 |], "rrwa y,a")
        else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_rvf= ([| 0x9c |],"rvf")

    //member this.op_sbc( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xa2; lo v |], "sbc a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xb2; lo v |],  "sbc a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xc2; hi v; lo v |],  "sbc a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xf2 |], "sbc a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xe2; lo v |], "sbc a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xd2; hi v; lo v |], "sbc a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf2 |], "sbc a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe2; lo v |], "sbc a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd2; hi v; lo v |], "sbc a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x12; lo v |], "sbc a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc2; lo v |],"sbc a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc2; hi v; lo v |],"sbc a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd2; lo v |],"sbc a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd2; hi v; lo v |],"sbc a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd2; lo v |],"sbc a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_scf = ([| 0x99 |],"scf")

    //member this.op_sim = ([| 0x9b |],"sim")

    member this.op_sll( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   a = R8 && b = "a" then ([| 0x48 |], "sll a")
        elif a = R16 && b = "x" then ([| 0x58 |], "sllw x")
        elif a = R16 && b = "y" then ([| 0x90; 0x58 |], "sllw y")
        elif a = DIRECT && p = REGRAM then ([| 0x38; lo v |],  "sll "+ (lo v |> string))
        elif a = DIRECT && p = RAM then ([| 0x72; 0x58; hi v; lo v |],  "sll "+ (v |> string))
        elif a = INDEXED && b = "x" && v = 0 then ([| 0x78 |], "sll (x)")
        elif a = INDEXED && b = "x" && v < 256 then ([| 0x68; lo v |], "sll ("+ (lo v |> string)+",x)")
        elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x48; hi v; lo v |], "sll ("+ (v |> string)+",x)")
        elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x78 |], "sll (y)")
        elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x68; lo v |], "sll ("+ (lo v |> string)+",y)")
        elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x48; hi v; lo v |], "sll ("+ (v |> string)+",y)")
        elif a = INDEXED && b = "sp" && v < 256 then ([| 0x08; lo v |], "sll ("+ (lo v |> string)+",sp)")
        elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x38; lo v |],"sll ["+ (lo v |> string)+"]")
        elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x38; hi v; lo v |],"sll ["+ (v |> string)+"]")
        elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x68; lo v |],"sll (["+ (lo v |> string)+"],x)")
        elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x68; hi v; lo v |],"sll (["+ (v |> string)+"],x)")
        elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x68; lo v |],"sll (["+ (lo v |> string)+"],y)")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_sllw( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   b = "x" then ([| 0x58 |], "sllw x,a")
        elif b = "y" then ([| 0x90; 0x58 |], "sllw y,a")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_sra( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   a = R8 && b = "a" then ([| 0x47 |], "sra a")
        elif a = R16 && b = "x" then ([| 0x57 |], "sraw x")
        elif a = R16 && b = "y" then ([| 0x90; 0x57 |], "sraw y")
        elif a = DIRECT && p = REGRAM then ([| 0x37; lo v |],  "sra "+ (lo v |> string))
        elif a = DIRECT && p = RAM then ([| 0x72; 0x57; hi v; lo v |],  "sra "+ (v |> string))
        elif a = INDEXED && b = "x" && v = 0 then ([| 0x77 |], "sra (x)")
        elif a = INDEXED && b = "x" && v < 256 then ([| 0x67; lo v |], "sra ("+ (lo v |> string)+",x)")
        elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x47; hi v; lo v |], "sra ("+ (v |> string)+",x)")
        elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x77 |], "sra (y)")
        elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x67; lo v |], "sra ("+ (lo v |> string)+",y)")
        elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x47; hi v; lo v |], "sra ("+ (v |> string)+",y)")
        elif a = INDEXED && b = "sp" && v < 256 then ([| 0x07; lo v |], "sra ("+ (lo v |> string)+",sp)")
        elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x37; lo v |],"sra ["+ (lo v |> string)+"]")
        elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x37; hi v; lo v |],"sra ["+ (v |> string)+"]")
        elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x67; lo v |],"sra (["+ (lo v |> string)+"],x)")
        elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x67; hi v; lo v |],"sra (["+ (v |> string)+"],x)")
        elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x67; lo v |],"sra (["+ (lo v |> string)+"],y)")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_sraw( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   b = "x" then ([| 0x57 |], "sraw x,a")
        elif b = "y" then ([| 0x90; 0x57 |], "sraw y,a")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_srl( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   a = R8 && b = "a" then ([| 0x44 |], "srl a")
        elif a = R16 && b = "x" then ([| 0x54 |], "srlw x")
        elif a = R16 && b = "y" then ([| 0x90; 0x54 |], "srlw y")
        elif a = DIRECT && p = REGRAM then ([| 0x34; lo v |],  "srl "+ (lo v |> string))
        elif a = DIRECT && p = RAM then ([| 0x72; 0x54; hi v; lo v |],  "srl "+ (v |> string))
        elif a = INDEXED && b = "x" && v = 0 then ([| 0x74 |], "srl (x)")
        elif a = INDEXED && b = "x" && v < 256 then ([| 0x64; lo v |], "srl ("+ (lo v |> string)+",x)")
        elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x44; hi v; lo v |], "srl ("+ (v |> string)+",x)")
        elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x74 |], "srl (y)")
        elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x64; lo v |], "srl ("+ (lo v |> string)+",y)")
        elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x44; hi v; lo v |], "srl ("+ (v |> string)+",y)")
        elif a = INDEXED && b = "sp" && v < 256 then ([| 0x04; lo v |], "srl ("+ (lo v |> string)+",sp)")
        elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x34; lo v |],"srl ["+ (lo v |> string)+"]")
        elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x34; hi v; lo v |],"srl ["+ (v |> string)+"]")
        elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x64; lo v |],"srl (["+ (lo v |> string)+"],x)")
        elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x64; hi v; lo v |],"srl (["+ (v |> string)+"],x)")
        elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x64; lo v |],"srl (["+ (lo v |> string)+"],y)")
        else failwith "Error: incorrect command-parameter-addressing combination"

    member this.op_srlw( a : Addressing, p : Place, b : string, v : int, m : string) =
        if   b = "x" then ([| 0x54 |], "srlw x,a")
        elif b = "y" then ([| 0x90; 0x54 |], "srlw y,a")
        else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_sub( f : string, a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   f = "a" && a = IMMEDIATE && v < 256 then ([| 0xa0; lo v |], "sub a,#"+ (lo v |> string))
    //    elif f = "a" && a = DIRECT && p = REGRAM then ([| 0xb0; lo v |],  "sub a,"+ (lo v |> string))
    //    elif f = "a" && a = DIRECT && p = RAM then ([| 0xc0; hi v; lo v |],  "sub a,"+ (v |> string))
    //    elif f = "a" && a = INDEXED && b = "x" && v = 0 then ([| 0xf0 |], "sub a,(x)")
    //    elif f = "a" && a = INDEXED && b = "x" && v < 256 then ([| 0xe0; lo v |], "sub a,("+ (lo v |> string)+",x)")
    //    elif f = "a" && a = INDEXED && b = "x" && v < 65536 then ([|0xd0; hi v; lo v |], "sub a,("+ (v |> string)+",x)")
    //    elif f = "a" && a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf0 |], "sub a,(y)")
    //    elif f = "a" && a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe0; lo v |], "sub a,("+ (lo v |> string)+",y)")
    //    elif f = "a" && a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd0; hi v; lo v |], "sub a,("+ (v |> string)+",y)")
    //    elif f = "a" && a = INDEXED && b = "sp" && v < 256 then ([| 0x10; lo v |], "sub a,("+ (lo v |> string)+",sp)")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc0; lo v |],"sub a,["+ (lo v |> string)+"]")
    //    elif f = "a" && a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc0; hi v; lo v |],"sub a,["+ (v |> string)+"]")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd0; lo v |],"sub a,(["+ (lo v |> string)+"],x)")
    //    elif f = "a" && a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd0; hi v; lo v |],"sub a,(["+ (v |> string)+"],x)")
    //    elif f = "a" && a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd0; lo v |],"sub a,(["+ (lo v |> string)+"],y)")
    //    elif f = "x" && a = IMMEDIATE && v < 65536  then ([| 0x1d; hi v; lo v |], "subw x,#"+ (v |> string))
    //    elif f = "x" && a = DIRECT then ([| 0x72; 0xb0; hi v; lo v |], "subw x,"+ (v |> string))
    //    elif f = "x" && a = INDEXED && b = "sp" && v < 256  then ([| 0x72; 0xf0; lo v |],"subw x,("+ (lo v |> string)+",sp)")
    //    elif f = "y" && a = IMMEDIATE && v < 65536 then ([| 0x72; 0xa2; hi v; lo v |], "subw y,#"+ (v |> string))
    //    elif f = "y" && a = DIRECT then ([| 0x72; 0xb2; hi v; lo v |], "subw y,"+ (v |> string))
    //    elif f = "y" && a = INDEXED && b = "sp" && v < 256 then ([| 0x72; 0xf2; lo v |],"subw y,("+ (lo v |> string)+",sp)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_swap( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x4e |], "swap a")
    //    elif a = R16 && b = "x" then ([| 0x5e |], "swapw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x5e |], "swapw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x3e; lo v |],  "swap "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x5e; hi v; lo v |],  "swap "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x7e |], "swap (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x6e; lo v |], "swap ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x4e; hi v; lo v |], "swap ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x7e |], "swap (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x6e; lo v |], "swap ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x4e; hi v; lo v |], "swap ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x0e; lo v |], "swap ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x3e; lo v |],"swap ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x3e; hi v; lo v |],"swap ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x6e; lo v |],"swap (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x6e; hi v; lo v |],"swap (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x6e; lo v |],"swap (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_tnz( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = R8 && b = "a" then ([| 0x4d |], "tnz a")
    //    elif a = R16 && b = "x" then ([| 0x5d |], "tnzw x")
    //    elif a = R16 && b = "y" then ([| 0x90; 0x5d |], "tnzw y")
    //    elif a = DIRECT && p = REGRAM then ([| 0x3d; lo v |],  "tnz "+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0x72; 0x5d; hi v; lo v |],  "tnz "+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0x7d |], "tnz (x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0x6d; lo v |], "tnz ("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([| 0x72; 0x4d; hi v; lo v |], "tnz ("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0x7d |], "tnz (y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0x6d; lo v |], "tnz ("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0x4d; hi v; lo v |], "tnz ("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x0d; lo v |], "tnz ("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0x3d; lo v |],"tnz ["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0x3d; hi v; lo v |],"tnz ["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0x6d; lo v |],"tnz (["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0x6d; hi v; lo v |],"tnz (["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0x6d; lo v |],"tnz (["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

    //member this.op_trap = ([| 0x83 |],"trap")

    //member this.op_wfe = ([| 0x72; 0x8f |],"wfe")

    //member this.op_wfi = ([| 0x8f |],"wfi")

    //member this.op_xor( a : Addressing, p : Place, b : string, v : int, m : string) =
    //    if   a = IMMEDIATE && v < 256 then ([| 0xa8; lo v |], "xor a,#"+ (lo v |> string))
    //    elif a = DIRECT && p = REGRAM then ([| 0xb8; lo v |],  "xor a,"+ (lo v |> string))
    //    elif a = DIRECT && p = RAM then ([| 0xc8; hi v; lo v |],  "xor a,"+ (v |> string))
    //    elif a = INDEXED && b = "x" && v = 0 then ([| 0xf8 |], "xor a,(x)")
    //    elif a = INDEXED && b = "x" && v < 256 then ([| 0xe8; lo v |], "xor a,("+ (lo v |> string)+",x)")
    //    elif a = INDEXED && b = "x" && v < 65536 then ([|0xd8; hi v; lo v |], "xor a,("+ (v |> string)+",x)")
    //    elif a = INDEXED && b = "y" && v = 0 then ([| 0x90; 0xf8 |], "xor a,(y)")
    //    elif a = INDEXED && b = "y" && v < 256 && v < 256 then ([| 0x90; 0xe8; lo v |], "xor a,("+ (lo v |> string)+",y)")
    //    elif a = INDEXED && b = "y" && v < 65536 then ([| 0x90; 0xd8; hi v; lo v |], "xor a,("+ (v |> string)+",y)")
    //    elif a = INDEXED && b = "sp" && v < 256 then ([| 0x18; lo v |], "xor a,("+ (lo v |> string)+",sp)")
    //    elif a = INDIRECT && p = REGRAM && b = "" && m = "w" then ([| 0x92; 0xc8; lo v |],"xor a,["+ (lo v |> string)+"]")
    //    elif a = INDIRECT && p = RAM && b = "" && m = "w"  then ([| 0x72; 0xc8; hi v; lo v |],"xor a,["+ (v |> string)+"]")
    //    elif a = INDIRECT && p = REGRAM && b = "x" && m = "w" then ([| 0x92; 0xd8; lo v |],"xor a,(["+ (lo v |> string)+"],x)")
    //    elif a = INDIRECT && p = RAM && b = "x" && m = "w" then ([| 0x72; 0xd8; hi v; lo v |],"xor a,(["+ (v |> string)+"],x)")
    //    elif a = INDIRECT && p = REGRAM && b = "y" && m = "w" then ([| 0x91; 0xd8; lo v |],"xor a,(["+ (lo v |> string)+"],y)")
    //    else failwith "Error: incorrect command-parameter-addressing combination"

let OP = new Op();


*)