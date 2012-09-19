; komandu tipai

kNormal       equ 1  
kExtraOpk     equ 2  ; reikia ziuret i OPK kad nustatyt komanda
kUnknown      equ 3  
kUnsupported  equ 3  ; PVZ PUSHA, POPA. Nepalaikomos 8086 architekturos
kEmpty        equ 3  ; pvz 0x66. Assembleris mnemonikos neturi tam
kPrefix       equ 4
kPrefixOp     equ 5

; komandu argumentu tipai 
; komandu argumentu tipai 
argNone       equ 0
argAL         equ 1
argCL         equ 2
argDL         equ 3
argBL         equ 4
argAH         equ 5
argCH         equ 6
argDH         equ 7
argBH         equ 8
argAX         equ 9
argCX         equ 10
argDX         equ 11
argBX         equ 12
argSP         equ 13
argBP         equ 14
argSI         equ 15 
argDI         equ 16
  
argES         equ 17
argCS         equ 18
argSS         equ 19
argDS         equ 20
  
argConst1     equ 21 ; '1'
argConst3     equ 22 ; '3'
; argumentai nereikalaujantys addresavimo baito
argImm8       equ 23
argEImm8      equ 24 ; 8 bit praplestas iki 16 bitu
argShort      equ 25
argOffs8      equ 26
argImm16      equ 27
argOffs16     equ 28
argNear       equ 29 
argFar        equ 30
;argumentai reikalaujantys addresavimo baito
argRegMem8    equ 31
argRegMem16   equ 32
argReg8       equ 33
argReg16      equ 34 
argSegReg     equ 35
  
; adresavimas 

t_opUnknown     db    "Unknown command$"
t_opUnsupported db    "Command unsupported by 8086 architecture$"
t_opEmpty       db    "Command has no mnemonic in 8086 architecture$"
t_opAAA         db    "AAA$"
t_opAAD         db    "AAD$"
t_opAAM         db    "AAM$"
t_opAAS         db    "AAS$"
t_opADC         db    "ADC$"
t_opADD         db    "ADD$"
t_opAND         db    "AND$"
t_opCALL        db    "CALL$"
t_opCALLF       db    "CALLF$"
t_opCBW         db    "CBW$"
t_opCLC         db    "CLC$"
t_opCLD         db    "CLD$"
t_opCLI         db    "CLI$"
t_opCMC         db    "CMC$"
t_opCMP         db    "CMP$"
t_opCMPSB       db    "CMPSB$"
t_opCMPSW       db    "CMPSW$"
t_opCWD         db    "CWD$"
t_opDAA         db    "DAA$"
t_opDAS         db    "DAS$"
t_opDEC         db    "DEC$"
t_opDIV         db    "DIV$"
t_opHLT         db    "HLT$"
t_opIDIV        db    "IDIV$"
t_opIMUL        db    "IMUL$"
t_opIN          db    "IN$"
t_opINC         db    "INC$"
t_opINT         db    "INT$"
t_opINTO        db    "INTO$"
t_opIRET        db    "IRET$"
t_opJA          db    "JA$"
t_opJAE         db    "JAE$"
t_opJB          db    "JB$"
t_opJBE         db    "JBE$"
t_opJCXZ        db    "JCXZ$"
t_opJG          db    "JG$"
t_opJGE         db    "JGE$"
t_opJL          db    "JL$"
t_opJLE         db    "JLE$"
t_opJMP         db    "JMP$"
t_opJMPF        db    "JMPF$"
t_opJNO         db    "JNO$"
t_opJNS         db    "JNS$"
t_opJNZ         db    "JNZ$"
t_opJO          db    "JO$"
t_opJPE         db    "JPE$"
t_opJPO         db    "JPO$"
t_opJS          db    "JS$"
t_opJZ          db    "JZ$"
t_opLAHF        db    "LAHF$"
t_opLDS         db    "LDS$"
t_opLEA         db    "LEA$"
t_opLES         db    "LES$"
t_opLOCK        db    "LOCK$"
t_opLODSB       db    "LODSB$"
t_opLODSW       db    "LODSW$"
t_opLOOP        db    "LOOP$"
t_opLOOPNZ      db    "LOOPNZ$"
t_opLOOPZ       db    "LOOPZ$"
t_opMOV         db    "MOV$"
t_opMOVSB       db    "MOVSB$"
t_opMOVSW       db    "MOVSW$"
t_opMUL         db    "MUL$"
t_opNEG         db    "NEG$"
t_opNOP         db    "NOP$"
t_opNOT         db    "NOT$"
t_opOR          db    "OR$"
t_opOUT         db    "OUT$"
t_opPOP         db    "POP$"
t_opPOPF        db    "POPF$"
t_opPUSH        db    "PUSH$"
t_opPUSHF       db    "PUSHF$"
t_opRCL         db    "RCL$"
t_opRCR         db    "RCR$"
t_opREP         db    "REP$"
t_opREPNE       db    "REPNE$"
t_opRETF        db    "RETF$"
t_opRETN        db    "RETN$"
t_opREPZ        db    "REPZ$"
t_opROL         db    "ROL$"
t_opROR         db    "ROR$"
t_opSAHF        db    "SAHF$"
t_opSAL         db    "SAL$"
t_opSAR         db    "SAR$"
t_opSBB         db    "SBB$"
t_opSCASB       db    "SCASB$"
t_opSCASW       db    "SCASW$"
t_opSEGCS       db    "SEGCS$"
t_opSEGDS       db    "SEGDS$"
t_opSEGES       db    "SEGES$"
t_opSEGSS       db    "SEGSS$"
t_opSHL         db    "SHL$"
t_opSHR         db    "SHR$"
t_opSTC         db    "STC$"
t_opSTD         db    "STD$"
t_opSTI         db    "STI$"
t_opSTOS        db    "STOS$" 
t_opSTOSB       db    "STOSB$"
t_opSTOSW       db    "STOSW$"
t_opSUB         db    "SUB$"
t_opTEST        db    "TEST$"
t_opWAIT        db    "WAIT$"
t_opXCHG        db    "XCHG$"
t_opXLAT        db    "XLAT$"
t_opXOR         db    "XOR$"

; Visi operacijos kodai
; Operacijos uzrasas, tipas, argumentas 1, argumentas 2
; reference - ref.x86asm.net/coder32.html
;                                                      po (primary code)

label opCodes opStruct
    opStruct <t_opADD, kNormal, argRegMem8, argReg8>      ;00
    opStruct <t_opADD, kNormal, argRegMem16, argReg16> ;01
    opStruct <t_opADD, kNormal, argReg8, argRegMem8>      ;02
    opStruct <t_opADD, kNormal, argReg16, argRegMem16>    ;03
    opStruct <t_opADD, kNormal, argAL, argImm8>           ;04
    opStruct <t_opADD, kNormal, argAX, argImm16>          ;05
    opStruct <t_opPush, kNormal, argES, argNone>          ;06
    opStruct <t_opPop, kNormal, argES, argNone>  ;07
    opStruct <t_opOr, kNormal, argRegMem8, argReg8> ;08
    opStruct <t_opOr, kNormal, argRegMem16, argReg16> ;09
    opStruct <t_opOr, kNormal, argReg8, argRegMem8> ;0A
    opStruct <t_opOr, kNormal, argReg16, argRegMem16> ;0B
    opStruct <t_opOr, kNormal, argAL, argImm8> ;0C
    opStruct <t_opOr, kNormal, argAX, argImm16> ;0D
    opStruct <t_opPush, kNormal, argCS, argNone> ;0E
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;0F - prefixas 2 baitu komandom (ju nereikia)
    opStruct <t_opAdc, kNormal, argRegMem8, argReg8> ;10
    opStruct <t_opAdc, kNormal, argRegMem16, argReg16> ;11
    opStruct <t_opAdc, kNormal, argReg8, argRegMem8> ;12
    opStruct <t_opAdc, kNormal, argReg16, argRegMem16> ;13
    opStruct <t_opAdc, kNormal, argAL, argImm8> ;14
    opStruct <t_opAdc, kNormal, argAX, argImm16> ;15
    opStruct <t_opPush, kNormal, argSS, argNone> ;16
    opStruct <t_opPop, kNormal, argSS, argNone> ;17
    opStruct <t_opSbb, kNormal, argRegMem8, argReg8> ;18
    opStruct <t_opSbb, kNormal, argRegMem16, argReg16> ; 19
    opStruct <t_opSbb, kNormal, argReg8, argRegMem8> ;1A
    opStruct <t_opSbb, kNormal, argReg16, argRegMem16> ;1B
    opStruct <t_opSbb, kNormal, argAL, argImm8> ; 1C
    opStruct <t_opSbb, kNormal, argAX, argImm16> ;1D
    opStruct <t_opPush, kNormal, argSS, argNone> ; 1E
    opStruct <t_opPop, kNormal, argSS, argNone> ;1F
    opStruct <t_opAdd, kNormal, argRegMem8, argReg8> ;20
    opStruct <t_opAdd, kNormal, argRegMem16, argReg16> ;21
    opStruct <t_opAdd, kNormal, argAl, argImm8> ;22
    opStruct <t_opAdd, kNormal, argReg16, argRegMem16> ;23
    opStruct <t_opAdd, kNormal, argAL, argImm8> ;24
    opStruct <t_opAdd, kNormal, argAX, argImm16> ; 25
    opStruct <s_regES, kPrefix, argNone, argNone> ; 26
    opStruct <t_opDaa, kNormal, argAL, argNone> ; 27
    opStruct <t_opSub, kNormal, argRegMem8, argReg8> ; 28
    opStruct <t_opSub, kNormal, argRegMem16, argReg16> ; 29
    opStruct <t_opSub, kNormal, argReg8, argRegMem8> ; 2A
    opStruct <t_opSub, kNormal, argReg16, argRegMem16> ; 2B
    opStruct <t_opSub, kNormal, argAL, argImm8> ;2C
    opStruct <t_opSub, kNormal, argAX, argImm16> ;2D
    opStruct <s_regCS, kPrefix, argNone, argNone> ;2E
    opStruct <t_opDas, kNormal, argAL, argNone> ;2F
    opStruct <t_opXor, kNormal, argRegMem8, argReg8> ; 30
    opStruct <t_opXor, kNormal, argRegMem16, argReg16> ;31
    opStruct <t_opXor, kNormal, argReg8, argRegMem16> ;32
    opStruct <t_opXor, kNormal, argReg16, argRegMem16> ;33
    opStruct <t_opXor, kNormal, argAL, argImm8> ;34
    opStruct <t_opXor, kNormal, argAX, argImm16> ;35
    opStruct <s_regSS, kPrefix, argNone, argNone> ; 36
    opStruct <t_opAaa, kNormal, argAL, argAH> ;37
    opStruct <t_opCmp, kNormal, argRegMem8, argReg8> ;38
    opStruct <t_opCmp, kNormal, argRegMem16, argReg16> ;39
    opStruct <t_opCmp, kNormal, argReg8, argRegMem8> ;3A
    opStruct <t_opCmp, kNormal, argReg16, argRegMem16> ;3B
    opStruct <t_opCmp, kNormal, argAL, argImm8> ;3C
    opStruct <t_opCmp, kNormal, argAX, argImm16> ; 3D
    opStruct <s_regDS, kPrefix, argNone, argNone> ; 3E
    opStruct <t_opAas, kNormal, argAl, argAh> ;3F
    opStruct <t_opInc, kNormal, argAX, argNone> ;40
    opStruct <t_opInc, kNormal, argCX, argNone> ;41
    opStruct <t_opInc, kNormal, argDX, argNone> ;42 
    opStruct <t_opInc, kNormal, argBX, argNone> ;43
    opStruct <t_opInc, kNormal, argSP, argNone> ;44
    opStruct <t_opInc, kNormal, argBP, argNone> ;45
    opStruct <t_opInc, kNormal, argSI, argNone> ;46
    opStruct <t_opInc, kNormal, argDI, argNone> ;47
    opStruct <t_opDec, kNormal, argAX, argNone> ;48
    opStruct <t_opDec, kNormal, argCX, argNone> ;49
    opStruct <t_opDec, kNormal, argDX, argNone> ;4A
    opStruct <t_opDec, kNormal, argBX, argNone> ;4B
    opStruct <t_opDec, kNormal, argSP, argNone> ;4C
    opStruct <t_opDec, kNormal, argBP, argNone> ;4D
    opStruct <t_opDec, kNormal, argSI, argNone> ;4E
    opStruct <t_opDec, kNormal, argDI, argNone> ;4F
    opStruct <t_opPush, kNormal, argAX, argNone> ;50
    opStruct <t_opPush, kNormal, argCX, argNone> ;51
    opStruct <t_opPush, kNormal, argDX, argNone> ;52
    opStruct <t_opPush, kNormal, argBX, argNone> ;53
    opStruct <t_opPush, kNormal, argSP, argNone> ;54
    opStruct <t_opPush, kNormal, argBP, argNone> ;55
    opStruct <t_opPush, kNormal, argSI, argNone> ;56
    opStruct <t_opPush, kNormal, argDI, argNone> ;57
    opStruct <t_opPop, kNormal, argAX, argNone> ;58
    opStruct <t_opPop, kNormal, argCX, argNone> ;59
    opStruct <t_opPop, kNormal, argDX, argNone> ;5A
    opStruct <t_opPop, kNormal, argBX, argNone> ;5B
    opStruct <t_opPop, kNormal, argSP, argNone> ;5C
    opStruct <t_opPop, kNormal, argBP, argNone> ;5D
    opStruct <t_opPop, kNormal, argSI, argNone> ;5E
    opStruct <t_opPop, kNormal, argDI, argNone> ;5F
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;60
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;61
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;62
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;63
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;64
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;65
    opStruct <t_opEmpty, kEmpty, argNone, argNone> ;66 
    opStruct <t_opEmpty, kEmpty, argNone, argNone> ;67
    opStruct <t_opEmpty, kEmpty, argNone, argNone> ;68
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone>  ;69
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone>  ;6A
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;6B
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;6C
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;6D
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;6E
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;6F
    opStruct <t_opJo, kNormal, argShort, argNone> ;70
    opStruct <t_opJno, kNormal, argShort, argNone> ;71
    opStruct <t_opJb, kNormal, argShort, argNone> ;72
    opStruct <t_opJae, kNormal, argShort, argNone> ;73
    opStruct <t_opJz, kNormal, argShort, argNone> ;74
    opStruct <t_opJnz, kNormal, argShort, argNone> ;75
    opStruct <t_opJbe, kNormal, argShort, argNone> ;76
    opStruct <t_opJa, kNormal, argShort, argNone> ;77
    opStruct <t_opJs, kNormal, argShort, argNone> ;78
    opStruct <t_opJns, kNormal, argShort, argNone> ;79
    opStruct <t_opJns, kNormal, argShort, argNone> ;7A
    opStruct <t_opJpo, kNormal, argShort, argNone> ;7B
    opStruct <t_opJl, kNormal, argShort, argNone> ;7C
    opStruct <t_opJge, kNormal, argShort, argNone> ;7D
    opStruct <t_opJle, kNormal, argShort, argNone> ;7E
    opStruct <t_opJg, kNormal, argShort, argNone> ;7F
    opStruct <t_opEmpty, kExtraOpk, argRegMem8, argImm8> ;80
    opStruct <t_opEmpty, kExtraOpk, argRegMem16, argImm16> ;81
    opStruct <t_opEmpty, kExtraOpk, argRegMem8, argImm8> ;82
    opStruct <t_opEmpty, kExtraOpk, argRegMem16, argEImm8> ;83
    opStruct <t_opTest, kNormal, argRegMem8, argReg8> ;84
    opStruct <t_opTest, kNormal, argRegMem16, argReg16> ;85
    opStruct <t_opXchg, kNormal, argReg8, argRegMem8> ;86
    opStruct <t_opXchg, kNormal, argReg16, argRegMem16> ;87
    opStruct <t_opMov, kNormal, argRegMem8, argReg8> ;88
    opStruct <t_opMov, kNormal, argRegMem16, argReg16> ;89
    opStruct <t_opMov, kNormal, argReg8, argRegMem8> ;8A
    opStruct <t_opMov, kNormal, argReg16, argRegMem16> ;8B
    opStruct <t_opMov, kNormal, argReg16, argSegReg> ;8C
    opStruct <t_opLea, kNormal, argReg16, argRegMem16> ;8D
    opStruct <t_opMov, kNormal, argSegReg, argRegMem16> ;8E
    opStruct <t_opPop, kNormal, argRegMem16, argNone> ;8F
    opStruct <t_opXchg, kNormal, argAX, argAX> ;90
    opStruct <t_opXchg, kNormal, argCX, argAX> ;91
    opStruct <t_opXchg, kNormal, argDX, argAX> ;92
    opStruct <t_opXchg, kNormal, argBX, argAX> ;93
    opStruct <t_opXchg, kNormal, argSP, argAX> ;94
    opStruct <t_opXchg, kNormal, argBP, argAX> ;95
    opStruct <t_opXchg, kNormal, argSI, argAX> ;96
    opStruct <t_opXchg, kNormal, argDI, argAX> ;97
    opStruct <t_opCbw, kNormal, argNone, argNone> ;98
    opStruct <t_opCwd, kNormal, argNone, argNone> ;99
    opStruct <t_opCallF, kNormal, argFar, argNone> ;9A
    opStruct <t_opWait, kNormal, argNone, argNone> ;9B
    opStruct <t_opPushF, kNormal, argNone, argNone> ;9C
    opStruct <t_opPopF, kNormal, argNone, argNone> ;9D
    opStruct <t_opSahf, kNormal, argNone, argNone> ;9E
    opStruct <t_opLahf, kNormal, argNone, argNone> ;9F
    opStruct <t_opMov, kNormal, argAL, argOffs8> ;A0
    opStruct <t_opMov, kNormal, argAX, argOffs16> ;A1
    opStruct <t_opMov, kNormal, argOffs8, argAL> ;A2
    opStruct <t_opMov, kNormal, argOffs16, argAx> ;A3
    opStruct <t_opMovsb, kNormal, argNone, argNone> ;A4
    opStruct <t_opMovsw, kNormal, argNone, argNone> ;A5
    opStruct <t_opCmpsb, kNormal, argNone, argNone> ;A6
    opStruct <t_opCmpsw, kNormal, argNone, argNone> ;A7
    opStruct <t_opTest, kNormal, argAL, argImm8> ;A8
    opStruct <t_opTest, kNormal, argAX, argImm16> ;A9
    opStruct <t_opStos, kNormal, argNone, argNone> ;AA
    opStruct <t_opStos, kNormal, argNone, argNone> ;AB
    opStruct <t_opLodsb, kNormal, argNone, argNone> ;AC
    opStruct <t_opLodsw, kNormal, argNone, argNone> ;AD
    opStruct <t_opScasb, kNormal, argNone, argNone> ;AE
    opStruct <t_opScasw, kNormal, argNone, argNone> ;AF
    opStruct <t_opMov, kNormal, argAL, argImm8> ;B0
    opStruct <t_opMov, kNormal, argCL, argImm8> ;B1
    opStruct <t_opMov, kNormal, argDL, argImm8> ;B2
    opStruct <t_opMov, kNormal, argBL, argImm8> ;B3
    opStruct <t_opMov, kNormal, argAH, argImm8> ;B4
    opStruct <t_opMov, kNormal, argCH, argImm8> ;B5
    opStruct <t_opMov, kNormal, argDH, argImm8> ;B6
    opStruct <t_opMov, kNormal, argBH, argImm8> ;B7
    opStruct <t_opMov, kNormal, argAX, argImm16> ;B8
    opStruct <t_opMov, kNormal, argCX, argImm16> ;B9
    opStruct <t_opMov, kNormal, argDX, argImm16> ;BA
    opStruct <t_opMov, kNormal, argBX, argImm16> ;BB
    opStruct <t_opMov, kNormal, argSP, argImm16> ;BC
    opStruct <t_opMov, kNormal, argBP, argImm16> ;BD
    opStruct <t_opMov, kNormal, argSI, argImm16> ;BE
    opStruct <t_opMov, kNormal, argDI, argImm16> ;BF
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;C0
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;C1
    opStruct <t_opRetN, kNormal, argNone, argNone> ;C2
    opStruct <t_opRetN, kNormal, argNone, argNone> ;C3
    opStruct <t_opLes, kNormal, argES, argReg16> ;C4
    opStruct <t_opLDS, kNormal, argDS, argReg16> ;C5
    opStruct <t_opMov, kNormal, argRegMem8, argImm8> ;C6
    opStruct <t_opMov, kNormal, argRegMem16, argImm16> ;C7
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;C8
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;C9
    opStruct <t_opRetF, kNormal, argImm16, argNone> ;CA
    opStruct <t_opRetF, kNormal, argNone, argNone> ;CB
    opStruct <t_opRetF, kNormal, argConst3, argNone> ;CC
    opStruct <t_opInt, kNormal, argImm8, argNone> ;CD
    opStruct <t_opInto, kNormal, argNone, argNone> ;CE
    opStruct <t_opIret, kNormal, argNone, argNone> ;CF
    opStruct <t_opEmpty, kExtraOpk, argRegMem8, argConst1> ;D0
    opStruct <t_opEmpty, kExtraOpk, argRegMem16, argConst1> ;D1
    opStruct <t_opEmpty, kExtraOpk, argRegMem8, argCL> ;D2
    opStruct <t_opEmpty, kExtraOpk, argRegMem16, argCL> ;D3
    opStruct <t_opAAM, kNormal, argImm8, argNone> ;D4
    opStruct <t_opAAD, kNormal, argImm8, argNone> ;D5
    opStruct <t_opUnsupported, kUnsupported, argNone, argNone> ;D6
    opStruct <t_opXlat, kNormal, argNone, argNone> ;D7
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;D8
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;D9
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DA
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DB
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DC
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DD
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DE
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;DF
    opStruct <t_opLoopnz, kNormal, argShort, argNone> ;E0
    opStruct <t_opLoopz, kNormal, argShort, argNone> ;E1
    opStruct <t_opLoop, kNormal, argShort, argNone> ;E2
    opStruct <t_opJcxz, kNormal, argShort, argNone> ;E3
    opStruct <t_opIn, kNormal, argAL, argImm8> ;E4
    opStruct <t_opIn, kNormal, argAX, argImm8> ;E5
    opStruct <t_opOut, kNormal, argImm8, argAL> ;E6
    opStruct <t_opOut, kNormal, argImm8, argAX> ;E7
    opStruct <t_opCall, kNormal, argNear, argNone> ;E8
    opStruct <t_opJmp, kNormal, argNear, argNone> ;E9
    opStruct <t_opJmpF, kNormal, argFar, argNone> ;EA
    opStruct <t_opJmp, kNormal, argShort, argNone> ;EB
    opStruct <t_opIn, kNormal, argAl, argDX> ;EC
    opStruct <t_opIn, kNormal, argAx, argDX> ;ED
    opStruct <t_opOut, kNormal, argDX, argAl> ;EE
    opStruct <t_opOut, kNormal, argDx, argAX> ;EF
    opStruct <t_opLOCK, kPrefixOP, argNone, argNone> ;F0
    opStruct <t_opUnknown, kUnknown, argNone, argNone> ;F1
    opStruct <t_opRepZ, kPrefixOp, argNone, argNone> ;F2
    opStruct <t_opRep, kPrefixOp, argNone, argNone> ;F3
    opStruct <t_opHLT, kNormal, argNone, argNone> ;F4
    opStruct <t_opCmc, kNormal, argNone, argNone> ;F5
    opStruct <t_opEmpty , kExtraOPK, argRegMem8, argNone> ;F6
    opStruct <t_opEmpty , kExtraOPK, argRegMem16, argNone> ;F7
    opStruct <t_opClc, kNormal, argNone, argNone> ;F8
    opStruct <t_opStc, kNormal, argNone, argNone> ;F9
    opStruct <t_opCli, kNormal, argNone, argNone> ;FA
    opStruct <t_opSti, kNormal, argNone, argNone> ;FB
    opStruct <t_opCld, kNormal, argNone, argNone> ;FC
    opStruct <t_opStd, kNormal, argNone, argNone> ;FD
    opStruct <t_opEmpty, kExtraOpk, argRegMem8 argNone> ;FE
    opStruct <t_opEmpty, kExtraOpk, argRegMem16, argNone> ;FF
	
label grp1Opk word ;80-83
    dw t_opADD, t_opOR, t_opADC, t_opSBB
	dw t_opAND, t_opSUB, t_opXOR, t_opCMP

label grp2opk word ;D0-D3
    dw t_opROL, t_opROR, t_opRCL, t_opRCR
	dw t_opSHL, t_opSHR, t_opSAL, t_opSAR
	
label grp3opk word ; F6-F7
    dw t_opTEST, t_opTEST, t_opNOT, t_opNEG
	dw t_opMUL, t_opIMUL, t_opDIV, t_opIDIV
	
label grp4opk word ;FE
    dw t_opINC, t_opDEC, t_opUnknown, t_opUnknown
	dw t_opUnknown, t_opUnknown, t_opUnknown, t_opUnknown
	
label grp5opk word
    dw t_opINC, t_opDEC, t_opCALL, t_opCALLF ;FF
	dw t_opJMP, t_opJMP, t_opPUSH, t_opUnknown
    