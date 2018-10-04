module elem

type Type = XX | ERROR | LABEL | DATA | CODE | INLINE
type Subtype = XX | ERROR | VAR | CONST | ARRAY | CONSTARRAY | SIMPLE | WITHADDRESS | NOPARAM | ONEPARAM | TWOPARAM 
type Place = XX | ERROR | OTHER | FLASH | HIFLASH | EEPROM | REGRAM | RAM | UNIFY

type Op = XX |NOP | TRAP | RET | RETF | IRET | HALT | WAIT | WAITI | DIV16 | CALLN | CALL | CALLF | PUSH | SWAP | SHL | SHR | SHLA | 
            SHRA | SHL0 | SHR0 | SHRS | POP | SUBTEST | ANDTEST | TEST | GO | GON | GOF | IFC1 | IFC0 | IFZ1 | IFZ0 | IFH1 | IFH0 | IFV1 | 
            IFN1 | IFN0 | IFV0 | IFI1 | IFI0 | IFINT1 | IFINT0 | IFT | IFF | IFSGE | IFSG | IFSLE | IFSL | IFUGE | IFUG | IFULE | IFUL | IFBT | IFBF | 
            IFBTF | IFT0 | IFT1 | INC | DEC | NEG | NOT | ADD | ADC | SUB | SBC | MUL | DIV | AND | OR | XOR | CHANGE | TO | BREAK | SKIP | INLINE

type Addressing = XX | ERROR | R1 | R8 | R16 | INHERENT | IMMEDIATE | DIRECT | INDEXED | INDIRECT | RELATIVE | BITOPERATION |
                    PORT | PORTBIT | PREINC | POSTINC | PREDEC | POSTDEC | REGBIT

let stub _ = ""

type Elm = 
    val source : string*int*string
    val elem : string
    val mutable t : Type 
    val mutable n : Op
    val mutable s : Subtype
    val mutable p : Place
    val mutable sz : int
    val mutable m1 : Addressing
    val mutable b1 : string
    val mutable v1 : string
    val mutable ev1 : string
    val mutable l1 : int
    val mutable m2 : Addressing
    val mutable b2 : string
    val mutable v2 : string
    val mutable ev2 : string
    val mutable l2 : int
    val mutable template : string
    val mutable prn : (Elm) -> string
    val mutable res : string

    new (f,n,l,e) = { 
        source = (f,n,l); elem = e; t = Type.XX; n = Op.XX; s = Subtype.XX; p = Place.XX; sz = 0;
        m1 = XX; b1 = ""; v1 = ""; ev1 = ""; l1 = 0; 
        m2 = XX; b2 = ""; v2 = ""; ev2 = ""; l2 = 0; 
        template = ""; prn = stub ; res = ""
    }

    new (n0,m10,b10,v10,ev10,l10,m20,b20,v20,ev20,l20,tem,pr) = {
        source = ("-1",-1,"-1"); elem = "-1"; t = CODE; s = Subtype.XX; p = FLASH; sz = -1; n = n0; m1 = m10; b1 = b10; v1 = v10; ev1 = ev10; l1 = l10;
        m2 = m20; b2 = b20; v2 = v20; ev2 = ev20; l2 = l20; template = tem ; prn = pr; res = ""
    }

    new (src,e0,t0,s0,p0,sz0,n0,m10,b10,v10,ev10,l10, m20,b20,v20,ev20,l20,tem,pr,re) = {
        source = src; elem = e0; t = t0; s = s0; p = p0; sz = sz0; n = n0; m1 = m10; b1 = b10; v1 = v10; ev1 = ev10; l1 = l10;
        m2 = m20; b2 = b20; v2 = v20; ev2 = ev20; l2 = l20; template = tem ; prn = pr; res = re 
    }

let sizecvt sz =
    match sz with
    | "8" -> 1
    | "16" -> 2
    | "32" -> 4
    | _ -> failwith "Error: Bad variable size, must be only 8,16,32"

let placecvt p =
    match p with
    | "r" -> REGRAM
    | "m" -> RAM
    | "f" -> FLASH
    | "e" -> EEPROM
    | "x" -> HIFLASH
    | "u" -> UNIFY
    | _ -> failwith ("Error: bad data place modifyer: '"+p+"' use only r,m,f,e,u" ) 
