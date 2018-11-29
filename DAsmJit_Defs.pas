unit DAsmJit_Defs;

{$I DAsmJit.inc}

interface

uses
  DAsmJit;

type
  TCustomLogger = class(TObject)
  protected
    FEnabled: Boolean;
  public
    procedure log(buf: string); virtual; abstract;
    procedure logAlign(m: SysInt); virtual; abstract;
    procedure logFormat(fmt: string; args: array of const); virtual; abstract;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

const
  //TERROR_CODE_E = (
    ERROR_NONE = 0;
    ERROR_NO_HEAP_MEMORY = 1;
    ERROR_NO_VIRTUAL_MEMORY = 2;
    ERROR_UNKNOWN_INSTRUCTION = 3;
    ERROR_ILLEGAL_INSTRUCTION = 4;
    ERROR_ILLEGAL_ADDRESING = 5;
    ERROR_ILLEGAL_SHORT_JUMP = 6;
    _ERROR_COUNT = 7;
  //);

  //TOP_E = (
    OP_NONE = 0;
    OP_REG = 1;
    OP_MEM = 2;
    OP_IMM = 3;
    OP_LABEL = 4;
    OP_VAR = 5;
  //);

  //TSIZE_E = (
    SIZE_BYTE   = 1;
    SIZE_WORD   = 2;
    SIZE_DWORD  = 4;
    SIZE_QWORD  = 8;
    SIZE_TWORD  = 10;
    SIZE_DQWORD = 16;
  //);

  //TRELOC_MODE_E = (
    RELOC_NONE = 0;
    RELOC_OVERWRITE = 1;
  //);

  //TLABEL_STATE_E = (
    LABEL_STATE_UNUSED = 0;
    LABEL_STATE_LINKED = 1;
    LABEL_STATE_BOUND = 2;
  //);

  //TPROPERTY_E = (
    PROPERTY_OPTIMIZE_ALIGN = 0;
    PROPERTY_X86_FORCE_REX = 1;
    PROPERTY_X86_JCC_HINTS = 2;
  //);

  //TRID_E = (
    RID_EAX = 0;
    RID_ECX = 1;
    RID_EDX = 2;
    RID_EBX = 3;
    RID_ESP = 4;
    RID_EBP = 5;
    RID_ESI = 6;
    RID_EDI = 7;
  //);

  //TSEGMENT_E = (
    SEGMENT_NONE = 0;
    SEGMENT_CS = 1;
    SEGMENT_SS = 2;
    SEGMENT_DS = 3;
    SEGMENT_ES = 4;
    SEGMENT_FS = 5;
    SEGMENT_GS = 6;
    _SEGMENT_END = 7;
  //);

  //TPREFETCH_HINT_E = (
    PREFETCH_T0  = 1;
    PREFETCH_T1  = 2;
    PREFETCH_T2  = 3;
    PREFETCH_NTA = 0;
  //);

  //TSCALE_E = (
    TIMES_1 = 0;
    TIMES_2 = 1;
    TIMES_4 = 2;
    TIMES_8 = 3;
  //);

  //THINT_E = (
    HINT_NONE = 0;
    HINT_TAKEN = 1;
    HINT_NOT_TAKEN = 2;
  //);

  //THINT_BYTE_VALUE_E = (
    HINT_BYTE_VALUE_TAKEN = $3E;
    HINT_BYTE_VALUE_NOT_TAKEN = $2E;
  //);

  //TFP_STATUS_E = (
    FP_C0 = $100;
    FP_C1 = $200;
    FP_C2 = $400;
    FP_C3 = $4000;
    FP_CC_MASK = $4500;
  //);

//const
  REGTYPE_MASK = $F0;
  REGCODE_MASK = $0F;
  REG_GPB = $00;
  REG_GPW = $10;
  REG_GPD = $20;
  REG_GPQ = $30;
{$IFDEF ASMJIT_X86}
  REG_GPN = REG_GPD;
{$ELSE}
  REG_GPN = REG_GPQ;
{$ENDIF}
  REG_X87 = $50;
  REG_MM = $60;
  REG_XMM = $70;
  REG_AL   = REG_GPB ; REG_CL   = 1;  REG_DL   = 2;  REG_BL   = 3;  REG_AH   = 4;  REG_CH   = 5;  REG_DH  = 6;    REG_BH   = 7;
{$IFDEF ASMJIT_X64}
  REG_R8B  = 8       ; REG_R9B  = 9;  REG_R10B = 10; REG_R11B = 11; REG_R12B = 12; REG_R13B = 13; REG_R14B = 14; REG_R15B = 15;
{$ENDIF}
  REG_AX   = REG_GPW ; REG_CX   = 17; REG_DX   = 18; REG_BX   = 19; REG_SP   = 20; REG_BP   = 21; REG_SI   = 22; REG_DI   = 23;
{$IFDEF ASMJIT_X64}
  REG_R8W  = 24      ; REG_R9W  = 25; REG_R10W = 26; REG_R11W = 27; REG_R12W = 28; REG_R13W = 29; REG_R14W = 30; REG_R15W = 31;
{$ENDIF}
  REG_EAX  = REG_GPD ; REG_ECX  = 33; REG_EDX  = 34; REG_EBX  = 35; REG_ESP  = 36; REG_EBP  = 37; REG_ESI  = 38; REG_EDI  = 39;
{$IFDEF ASMJIT_X64}
  REG_R8D  = 40      ; REG_R9D  = 41; REG_R10D = 42; REG_R11D = 43; REG_R12D = 44; REG_R13D = 45; REG_R14D = 46; REG_R15D = 47;
  REG_RAX  = REG_GPQ ; REG_RCX  = 49; REG_RDX  = 50; REG_RBX  = 51; REG_RSP  = 52; REG_RBP  = 53; REG_RSI  = 54; REG_RDI  = 55;
  REG_R8   = 56      ; REG_R9   = 57; REG_R10  = 58; REG_R11  = 59; REG_R12  = 60; REG_R13  = 61; REG_R14  = 62; REG_R15  = 63;
{$ENDIF}
  REG_MM0  = REG_MM  ; REG_MM1  = 97; REG_MM2  = 98; REG_MM3  = 99; REG_MM4  = 100; REG_MM5 = 101; REG_MM6 = 102; REG_MM7 = 103;
  REG_XMM0 = REG_XMM ; REG_XMM1 =113; REG_XMM2 =114; REG_XMM3 =115; REG_XMM4 = 116; REG_XMM5 =117; REG_XMM6 =118; REG_XMM7 =119;
{$IFDEF ASMJIT_X64}
  REG_XMM8 = 120     ; REG_XMM9 =121; REG_XMM10=122; REG_XMM11=123; REG_XMM12=124; REG_XMM13= 125; REG_XMM14=126; REG_XMM15=127;
{$ENDIF}
{$IFDEF ASMJIT_X86}
  REG_NAX  = REG_GPD ; REG_NCX  = 33; REG_NDX  = 34; REG_NBX  = 35; REG_NSP  = 36; REG_NBP  =  37; REG_NSI  = 38; REG_NDI  = 39;
{$ELSE}
  REG_NAX  = REG_GPQ ; REG_NCX  = 49; REG_NDX  = 50; REG_NBX  = 51; REG_NSP  = 52; REG_NBP  =  53; REG_NSI  = 54; REG_NDI  = 55;
{$ENDIF}
  NO_REG = $FF;

{$IFDEF ASMJIT_X86}
  NUM_REGS = 8;
{$ELSE}
  NUM_REGS = 16;
{$ENDIF}

  C_NO_CONDITION  = -1;

  C_A             = $7;
  C_AE            = $3;
  C_B             = $2;
  C_BE            = $6;
  C_C             = $2;
  C_E             = $4;
  C_G             = $F;
  C_GE            = $D;
  C_L             = $C;
  C_LE            = $E;
  C_NA            = $6;
  C_NAE           = $2;
  C_NB            = $3;
  C_NBE           = $7;
  C_NC            = $3;
  C_NE            = $5;
  C_NG            = $E;
  C_NGE           = $C;
  C_NL            = $D;
  C_NLE           = $F;
  C_NO            = $1;
  C_NP            = $B;
  C_NS            = $9;
  C_NZ            = $5;
  C_O             = $0;
  C_P             = $A;
  C_PE            = $A;
  C_PO            = $B;
  C_S             = $8;
  C_Z             = $4;

  C_OVERFLOW      = $0;
  C_NO_OVERFLOW   = $1;
  C_BELOW         = $2;
  C_ABOVE_EQUAL   = $3;
  C_EQUAL         = $4;
  C_NOT_EQUAL     = $5;
  C_BELOW_EQUAL   = $6;
  C_ABOVE         = $7;
  C_SIGN          = $8;
  C_NOT_SIGN      = $9;
  C_PARITY_EVEN   = $A;
  C_PARITY_ODD    = $B;
  C_LESS          = $C;
  C_GREATER_EQUAL = $D;
  C_LESS_EQUAL    = $E;
  C_GREATER       = $F;

  C_ZERO          = $4;
  C_NOT_ZERO      = $5;
  C_NEGATIVE      = $8;
  C_POSITIVE      = $9;

  C_FP_UNORDERED  = 16;
  C_FP_NOT_UNORDERED = 17;

  FP_CW_INVOPEX_MASK  = $001;
  FP_CW_DENOPEX_MASK  = $002;
  FP_CW_ZERODIV_MASK  = $004;
  FP_CW_OVFEX_MASK    = $008;
  FP_CW_UNDFEX_MASK   = $010;
  FP_CW_PRECEX_MASK   = $020;
  FP_CW_PRECC_MASK    = $300;
  FP_CW_ROUNDC_MASK   = $C00;

  FP_CW_PREC_SINGLE   = $000;
  FP_CW_PREC_DOUBLE   = $200;
  FP_CW_PREC_EXTENDED = $300;

  FP_CW_ROUND_NEAREST = $000;
  FP_CW_ROUND_DOWN    = $400;
  FP_CW_ROUND_UP      = $800;
  FP_CW_ROUND_TOZERO  = $C00;

  INST_ADC                     = 001;
  INST_ADD                     = 002;
  INST_ADDPD                   = 003;
  INST_ADDPS                   = 004;
  INST_ADDSD                   = 005;
  INST_ADDSS                   = 006;
  INST_ADDSUBPD                = 007;
  INST_ADDSUBPS                = 008;
  INST_AMD_PREFETCH            = 009;
  INST_AMD_PREFETCHW           = 010;
  INST_AND                     = 011;
  INST_ANDNPD                  = 012;
  INST_ANDNPS                  = 013;
  INST_ANDPD                   = 014;
  INST_ANDPS                   = 015;
  INST_BLENDPD                 = 016;
  INST_BLENDPS                 = 017;
  INST_BLENDVPD                = 018;
  INST_BLENDVPS                = 019;
  INST_BSF                     = 020;
  INST_BSR                     = 021;
  INST_BSWAP                   = 022;
  INST_BT                      = 023;
  INST_BTC                     = 024;
  INST_BTR                     = 025;
  INST_BTS                     = 026;
  INST_CALL                    = 027;
  INST_CBW                     = 028;
  INST_CDQE                    = 029;
  INST_CLC                     = 030;
  INST_CLD                     = 031;
  INST_CLFLUSH                 = 032;
  INST_CMC                     = 033;
  INST_CMOV                    = 034;
  INST_CMOVA = INST_CMOV;
  INST_CMOVAE                  = 035;
  INST_CMOVB                   = 036;
  INST_CMOVBE                  = 037;
  INST_CMOVC                   = 038;
  INST_CMOVE                   = 039;
  INST_CMOVG                   = 040;
  INST_CMOVGE                  = 041;
  INST_CMOVL                   = 042;
  INST_CMOVLE                  = 043;
  INST_CMOVNA                  = 044;
  INST_CMOVNAE                 = 045;
  INST_CMOVNB                  = 046;
  INST_CMOVNBE                 = 047;
  INST_CMOVNC                  = 048;
  INST_CMOVNE                  = 049;
  INST_CMOVNG                  = 050;
  INST_CMOVNGE                 = 051;
  INST_CMOVNL                  = 052;
  INST_CMOVNLE                 = 053;
  INST_CMOVNO                  = 054;
  INST_CMOVNP                  = 055;
  INST_CMOVNS                  = 056;
  INST_CMOVNZ                  = 057;
  INST_CMOVO                   = 058;
  INST_CMOVP                   = 059;
  INST_CMOVPE                  = 060;
  INST_CMOVPO                  = 061;
  INST_CMOVS                   = 062;
  INST_CMOVZ                   = 063;

  INST_CMP                     = 064;
  INST_CMPPD                   = 065;
  INST_CMPPS                   = 066;
  INST_CMPSD                   = 067;
  INST_CMPSS                   = 068;
  INST_CMPXCHG                 = 069;
  INST_CMPXCHG16B              = 070;
  INST_CMPXCHG8B               = 071;
  INST_COMISD                  = 072;
  INST_COMISS                  = 073;
  INST_CPUID                   = 074;
  INST_CRC32                   = 075;
  INST_CVTDQ2PD                = 076;
  INST_CVTDQ2PS                = 077;
  INST_CVTPD2DQ                = 078;
  INST_CVTPD2PI                = 079;
  INST_CVTPD2PS                = 080;
  INST_CVTPI2PD                = 081;
  INST_CVTPI2PS                = 082;
  INST_CVTPS2DQ                = 083;
  INST_CVTPS2PD                = 084;
  INST_CVTPS2PI                = 085;
  INST_CVTSD2SI                = 086;
  INST_CVTSD2SS                = 087;
  INST_CVTSI2SD                = 088;
  INST_CVTSI2SS                = 089;
  INST_CVTSS2SD                = 090;
  INST_CVTSS2SI                = 091;
  INST_CVTTPD2DQ               = 092;
  INST_CVTTPD2PI               = 093;
  INST_CVTTPS2DQ               = 094;
  INST_CVTTPS2PI               = 095;
  INST_CVTTSD2SI               = 096;
  INST_CVTTSS2SI               = 097;
  INST_CWDE                    = 098;
  INST_DAA                     = 099;
  INST_DAS                     = 100;
  INST_DEC                     = 101;
  INST_DIV                     = 102;
  INST_DIVPD                   = 103;
  INST_DIVPS                   = 104;
  INST_DIVSD                   = 105;
  INST_DIVSS                   = 106;
  INST_DPPD                    = 107;
  INST_DPPS                    = 108;
  INST_EMMS                    = 109;
  INST_ENTER                   = 110;
  INST_EXTRACTPS               = 111;
  INST_F2XM1                   = 112;
  INST_FABS                    = 113;
  INST_FADD                    = 114;
  INST_FADDP                   = 115;
  INST_FBLD                    = 116;
  INST_FBSTP                   = 117;
  INST_FCHS                    = 118;
  INST_FCLEX                   = 119;
  INST_FCMOVB                  = 120;
  INST_FCMOVBE                 = 121;
  INST_FCMOVE                  = 122;
  INST_FCMOVNB                 = 123;
  INST_FCMOVNBE                = 124;
  INST_FCMOVNE                 = 125;
  INST_FCMOVNU                 = 126;
  INST_FCMOVU                  = 127;
  INST_FCOM                    = 128;
  INST_FCOMI                   = 129;
  INST_FCOMIP                  = 130;
  INST_FCOMP                   = 131;
  INST_FCOMPP                  = 132;
  INST_FCOS                    = 133;
  INST_FDECSTP                 = 134;
  INST_FDIV                    = 135;
  INST_FDIVP                   = 136;
  INST_FDIVR                   = 137;
  INST_FDIVRP                  = 138;
  INST_FEMMS                   = 139;
  INST_FFREE                   = 140;
  INST_FIADD                   = 141;
  INST_FICOM                   = 142;
  INST_FICOMP                  = 143;
  INST_FIDIV                   = 144;
  INST_FIDIVR                  = 145;
  INST_FILD                    = 146;
  INST_FIMUL                   = 147;
  INST_FINCSTP                 = 148;
  INST_FINIT                   = 149;
  INST_FIST                    = 150;
  INST_FISTP                   = 151;
  INST_FISTTP                  = 152;
  INST_FISUB                   = 153;
  INST_FISUBR                  = 154;
  INST_FLD                     = 155;
  INST_FLD1                    = 156;
  INST_FLDCW                   = 157;
  INST_FLDENV                  = 158;
  INST_FLDL2E                  = 159;
  INST_FLDL2T                  = 160;
  INST_FLDLG2                  = 161;
  INST_FLDLN2                  = 162;
  INST_FLDPI                   = 163;
  INST_FLDZ                    = 164;
  INST_FMUL                    = 165;
  INST_FMULP                   = 166;
  INST_FNCLEX                  = 167;
  INST_FNINIT                  = 168;
  INST_FNOP                    = 169;
  INST_FNSAVE                  = 170;
  INST_FNSTCW                  = 171;
  INST_FNSTENV                 = 172;
  INST_FNSTSW                  = 173;
  INST_FPATAN                  = 174;
  INST_FPREM                   = 175;
  INST_FPREM1                  = 176;
  INST_FPTAN                   = 177;
  INST_FRNDINT                 = 178;
  INST_FRSTOR                  = 179;
  INST_FSAVE                   = 180;
  INST_FSCALE                  = 181;
  INST_FSIN                    = 182;
  INST_FSINCOS                 = 183;
  INST_FSQRT                   = 184;
  INST_FST                     = 185;
  INST_FSTCW                   = 186;
  INST_FSTENV                  = 187;
  INST_FSTP                    = 188;
  INST_FSTSW                   = 189;
  INST_FSUB                    = 190;
  INST_FSUBP                   = 191;
  INST_FSUBR                   = 192;
  INST_FSUBRP                  = 193;
  INST_FTST                    = 194;
  INST_FUCOM                   = 195;
  INST_FUCOMI                  = 196;
  INST_FUCOMIP                 = 197;
  INST_FUCOMP                  = 198;
  INST_FUCOMPP                 = 199;
  INST_FWAIT                   = 200;
  INST_FXAM                    = 201;
  INST_FXCH                    = 202;
  INST_FXRSTOR                 = 203;
  INST_FXSAVE                  = 204;
  INST_FXTRACT                 = 205;
  INST_FYL2X                   = 206;
  INST_FYL2XP1                 = 207;
  INST_HADDPD                  = 208;
  INST_HADDPS                  = 209;
  INST_HSUBPD                  = 210;
  INST_HSUBPS                  = 211;
  INST_IDIV                    = 212;
  INST_IMUL                    = 213;
  INST_INC                     = 214;
  INST_INT3                    = 215;

  INST_J                       = 216;
  INST_JA = INST_J;
  INST_JAE                     = 217;
  INST_JB                      = 218;
  INST_JBE                     = 219;
  INST_JC                      = 220;
  INST_JE                      = 221;
  INST_JG                      = 222;
  INST_JGE                     = 223;
  INST_JL                      = 224;
  INST_JLE                     = 225;
  INST_JNA                     = 226;
  INST_JNAE                    = 227;
  INST_JNB                     = 228;
  INST_JNBE                    = 229;
  INST_JNC                     = 230;
  INST_JNE                     = 231;
  INST_JNG                     = 232;
  INST_JNGE                    = 233;
  INST_JNL                     = 234;
  INST_JNLE                    = 235;
  INST_JNO                     = 236;
  INST_JNP                     = 237;
  INST_JNS                     = 238;
  INST_JNZ                     = 239;
  INST_JO                      = 240;
  INST_JP                      = 241;
  INST_JPE                     = 242;
  INST_JPO                     = 243;
  INST_JS                      = 244;
  INST_JZ                      = 245;
  INST_JMP                     = 246;

  INST_J_SHORT                 = 247;
  INST_JA_SHORT = INST_J_SHORT;
  INST_JAE_SHORT               = 248;
  INST_JB_SHORT                = 249;
  INST_JBE_SHORT               = 250;
  INST_JC_SHORT                = 251;
  INST_JE_SHORT                = 252;
  INST_JG_SHORT                = 253;
  INST_JGE_SHORT               = 254;
  INST_JL_SHORT                = 255;
  INST_JLE_SHORT               = 256;
  INST_JNA_SHORT               = 257;
  INST_JNAE_SHORT              = 258;
  INST_JNB_SHORT               = 259;
  INST_JNBE_SHORT              = 260;
  INST_JNC_SHORT               = 261;
  INST_JNE_SHORT               = 262;
  INST_JNG_SHORT               = 263;
  INST_JNGE_SHORT              = 264;
  INST_JNL_SHORT               = 265;
  INST_JNLE_SHORT              = 266;
  INST_JNO_SHORT               = 267;
  INST_JNP_SHORT               = 268;
  INST_JNS_SHORT               = 269;
  INST_JNZ_SHORT               = 270;
  INST_JO_SHORT                = 271;
  INST_JP_SHORT                = 272;
  INST_JPE_SHORT               = 273;
  INST_JPO_SHORT               = 274;
  INST_JS_SHORT                = 275;
  INST_JZ_SHORT                = 276;
  INST_JMP_SHORT               = 277;

  INST_LDDQU                   = 278;
  INST_LDMXCSR                 = 279;
  INST_LEA                     = 280;
  INST_LEAVE                   = 281;
  INST_LFENCE                  = 282;
  INST_LOCK                    = 283;
  INST_MASKMOVDQU              = 284;
  INST_MASKMOVQ                = 285;
  INST_MAXPD                   = 286;
  INST_MAXPS                   = 287;
  INST_MAXSD                   = 288;
  INST_MAXSS                   = 289;
  INST_MFENCE                  = 290;
  INST_MINPD                   = 291;
  INST_MINPS                   = 292;
  INST_MINSD                   = 293;
  INST_MINSS                   = 294;
  INST_MONITOR                 = 295;
  INST_MOV                     = 296;
  INST_MOVAPD                  = 297;
  INST_MOVAPS                  = 298;
  INST_MOVBE                   = 299;
  INST_MOVD                    = 300;
  INST_MOVDDUP                 = 301;
  INST_MOVDQ2Q                 = 302;
  INST_MOVDQA                  = 303;
  INST_MOVDQU                  = 304;
  INST_MOVHLPS                 = 305;
  INST_MOVHPD                  = 306;
  INST_MOVHPS                  = 307;
  INST_MOVLHPS                 = 308;
  INST_MOVLPD                  = 309;
  INST_MOVLPS                  = 310;
  INST_MOVMSKPD                = 311;
  INST_MOVMSKPS                = 312;
  INST_MOVNTDQ                 = 313;
  INST_MOVNTDQA                = 314;
  INST_MOVNTI                  = 315;
  INST_MOVNTPD                 = 316;
  INST_MOVNTPS                 = 317;
  INST_MOVNTQ                  = 318;
  INST_MOVQ                    = 319;
  INST_MOVQ2DQ                 = 320;
  INST_MOVSD                   = 321;
  INST_MOVSHDUP                = 322;
  INST_MOVSLDUP                = 323;
  INST_MOVSS                   = 324;
  INST_MOVSX                   = 325;
  INST_MOVSXD                  = 326;
  INST_MOVUPD                  = 327;
  INST_MOVUPS                  = 328;
  INST_MOVZX                   = 329;
  INST_MOV_PTR                 = 330;
  INST_MPSADBW                 = 331;
  INST_MUL                     = 332;
  INST_MULPD                   = 333;
  INST_MULPS                   = 334;
  INST_MULSD                   = 335;
  INST_MULSS                   = 336;
  INST_MWAIT                   = 337;
  INST_NEG                     = 338;
  INST_NOP                     = 339;
  INST_NOT                     = 340;
  INST_OR                      = 341;
  INST_ORPD                    = 342;
  INST_ORPS                    = 343;
  INST_PABSB                   = 344;
  INST_PABSD                   = 345;
  INST_PABSW                   = 346;
  INST_PACKSSDW                = 347;
  INST_PACKSSWB                = 348;
  INST_PACKUSDW                = 349;
  INST_PACKUSWB                = 350;
  INST_PADDB                   = 351;
  INST_PADDD                   = 352;
  INST_PADDQ                   = 353;
  INST_PADDSB                  = 354;
  INST_PADDSW                  = 355;
  INST_PADDUSB                 = 356;
  INST_PADDUSW                 = 357;
  INST_PADDW                   = 358;
  INST_PALIGNR                 = 359;
  INST_PAND                    = 360;
  INST_PANDN                   = 361;
  INST_PAUSE                   = 362;
  INST_PAVGB                   = 363;
  INST_PAVGW                   = 364;
  INST_PBLENDVB                = 365;
  INST_PBLENDW                 = 366;
  INST_PCMPEQB                 = 367;
  INST_PCMPEQD                 = 368;
  INST_PCMPEQQ                 = 369;
  INST_PCMPEQW                 = 370;
  INST_PCMPESTRI               = 371;
  INST_PCMPESTRM               = 372;
  INST_PCMPGTB                 = 373;
  INST_PCMPGTD                 = 374;
  INST_PCMPGTQ                 = 375;
  INST_PCMPGTW                 = 376;
  INST_PCMPISTRI               = 377;
  INST_PCMPISTRM               = 378;
  INST_PEXTRB                  = 379;
  INST_PEXTRD                  = 380;
  INST_PEXTRQ                  = 381;
  INST_PEXTRW                  = 382;
  INST_PF2ID                   = 383;
  INST_PF2IW                   = 384;
  INST_PFACC                   = 385;
  INST_PFADD                   = 386;
  INST_PFCMPEQ                 = 387;
  INST_PFCMPGE                 = 388;
  INST_PFCMPGT                 = 389;
  INST_PFMAX                   = 390;
  INST_PFMIN                   = 391;
  INST_PFMUL                   = 392;
  INST_PFNACC                  = 393;
  INST_PFPNACC                 = 394;
  INST_PFRCP                   = 395;
  INST_PFRCPIT1                = 396;
  INST_PFRCPIT2                = 397;
  INST_PFRSQIT1                = 398;
  INST_PFRSQRT                 = 399;
  INST_PFSUB                   = 400;
  INST_PFSUBR                  = 401;
  INST_PHADDD                  = 402;
  INST_PHADDSW                 = 403;
  INST_PHADDW                  = 404;
  INST_PHMINPOSUW              = 405;
  INST_PHSUBD                  = 406;
  INST_PHSUBSW                 = 407;
  INST_PHSUBW                  = 408;
  INST_PI2FD                   = 409;
  INST_PI2FW                   = 410;
  INST_PINSRB                  = 411;
  INST_PINSRD                  = 412;
  INST_PINSRQ                  = 413;
  INST_PINSRW                  = 414;
  INST_PMADDUBSW               = 415;
  INST_PMADDWD                 = 416;
  INST_PMAXSB                  = 417;
  INST_PMAXSD                  = 418;
  INST_PMAXSW                  = 419;
  INST_PMAXUB                  = 420;
  INST_PMAXUD                  = 421;
  INST_PMAXUW                  = 422;
  INST_PMINSB                  = 423;
  INST_PMINSD                  = 424;
  INST_PMINSW                  = 425;
  INST_PMINUB                  = 426;
  INST_PMINUD                  = 427;
  INST_PMINUW                  = 428;
  INST_PMOVMSKB                = 429;
  INST_PMOVSXBD                = 430;
  INST_PMOVSXBQ                = 431;
  INST_PMOVSXBW                = 432;
  INST_PMOVSXDQ                = 433;
  INST_PMOVSXWD                = 434;
  INST_PMOVSXWQ                = 435;
  INST_PMOVZXBD                = 436;
  INST_PMOVZXBQ                = 437;
  INST_PMOVZXBW                = 438;
  INST_PMOVZXDQ                = 439;
  INST_PMOVZXWD                = 440;
  INST_PMOVZXWQ                = 441;
  INST_PMULDQ                  = 442;
  INST_PMULHRSW                = 443;
  INST_PMULHUW                 = 444;
  INST_PMULHW                  = 445;
  INST_PMULLD                  = 446;
  INST_PMULLW                  = 447;
  INST_PMULUDQ                 = 448;
  INST_POP                     = 449;
  INST_POPAD                   = 450;
  INST_POPCNT                  = 451;
  INST_POPFD                   = 452;
  INST_POPFQ                   = 453;
  INST_POR                     = 454;
  INST_PREFETCH                = 455;
  INST_PSADBW                  = 456;
  INST_PSHUFB                  = 457;
  INST_PSHUFD                  = 458;
  INST_PSHUFW                  = 459;
  INST_PSHUFHW                 = 460;
  INST_PSHUFLW                 = 461;
  INST_PSIGNB                  = 462;
  INST_PSIGND                  = 463;
  INST_PSIGNW                  = 464;
  INST_PSLLD                   = 465;
  INST_PSLLDQ                  = 466;
  INST_PSLLQ                   = 467;
  INST_PSLLW                   = 468;
  INST_PSRAD                   = 469;
  INST_PSRAW                   = 470;
  INST_PSRLD                   = 471;
  INST_PSRLDQ                  = 472;
  INST_PSRLQ                   = 473;
  INST_PSRLW                   = 474;
  INST_PSUBB                   = 475;
  INST_PSUBD                   = 476;
  INST_PSUBQ                   = 477;
  INST_PSUBSB                  = 478;
  INST_PSUBSW                  = 479;
  INST_PSUBUSB                 = 480;
  INST_PSUBUSW                 = 481;
  INST_PSUBW                   = 482;
  INST_PSWAPD                  = 483;
  INST_PTEST                   = 484;
  INST_PUNPCKHBW               = 485;
  INST_PUNPCKHDQ               = 486;
  INST_PUNPCKHQDQ              = 487;
  INST_PUNPCKHWD               = 488;
  INST_PUNPCKLBW               = 489;
  INST_PUNPCKLDQ               = 490;
  INST_PUNPCKLQDQ              = 491;
  INST_PUNPCKLWD               = 492;
  INST_PUSH                    = 493;
  INST_PUSHAD                  = 494;
  INST_PUSHFD                  = 495;
  INST_PUSHFQ                  = 496;
  INST_PXOR                    = 497;
  INST_RCL                     = 498;
  INST_RCPPS                   = 499;
  INST_RCPSS                   = 500;
  INST_RCR                     = 501;
  INST_RDTSC                   = 502;
  INST_RDTSCP                  = 503;
  INST_RET                     = 504;
  INST_ROL                     = 505;
  INST_ROR                     = 506;
  INST_ROUNDPD                 = 507;
  INST_ROUNDPS                 = 508;
  INST_ROUNDSD                 = 509;
  INST_ROUNDSS                 = 510;
  INST_RSQRTPS                 = 511;
  INST_RSQRTSS                 = 512;
  INST_SAHF                    = 513;
  INST_SAL                     = 514;
  INST_SAR                     = 515;
  INST_SBB                     = 516;
  INST_SET                     = 517;
  INST_SETA=INST_SET;
  INST_SETAE                   = 518;
  INST_SETB                    = 519;
  INST_SETBE                   = 520;
  INST_SETC                    = 521;
  INST_SETE                    = 522;
  INST_SETG                    = 523;
  INST_SETGE                   = 524;
  INST_SETL                    = 525;
  INST_SETLE                   = 526;
  INST_SETNA                   = 527;
  INST_SETNAE                  = 528;
  INST_SETNB                   = 529;
  INST_SETNBE                  = 530;
  INST_SETNC                   = 531;
  INST_SETNE                   = 532;
  INST_SETNG                   = 533;
  INST_SETNGE                  = 534;
  INST_SETNL                   = 535;
  INST_SETNLE                  = 536;
  INST_SETNO                   = 537;
  INST_SETNP                   = 538;
  INST_SETNS                   = 539;
  INST_SETNZ                   = 540;
  INST_SETO                    = 541;
  INST_SETP                    = 542;
  INST_SETPE                   = 543;
  INST_SETPO                   = 544;
  INST_SETS                    = 545;
  INST_SETZ                    = 546;
  INST_SFENCE                  = 547;
  INST_SHL                     = 548;
  INST_SHLD                    = 549;
  INST_SHR                     = 550;
  INST_SHRD                    = 551;
  INST_SHUFPD                  = 552;
  INST_SHUFPS                  = 553;
  INST_SQRTPD                  = 554;
  INST_SQRTPS                  = 555;
  INST_SQRTSD                  = 556;
  INST_SQRTSS                  = 557;
  INST_STC                     = 558;
  INST_STD                     = 559;
  INST_STMXCSR                 = 560;
  INST_SUB                     = 561;
  INST_SUBPD                   = 562;
  INST_SUBPS                   = 563;
  INST_SUBSD                   = 564;
  INST_SUBSS                   = 565;
  INST_TEST                    = 566;
  INST_UCOMISD                 = 567;
  INST_UCOMISS                 = 568;
  INST_UD2                     = 569;
  INST_UNPCKHPD                = 570;
  INST_UNPCKHPS                = 571;
  INST_UNPCKLPD                = 572;
  INST_UNPCKLPS                = 573;
  INST_XADD                    = 574;
  INST_XCHG                    = 575;
  INST_XOR                     = 576;
  INST_XORPD                   = 577;
  INST_XORPS                   = 578;

  _INST_COUNT                  = 579;

function errorCodeToString(Error: UInt32): string;

implementation

function errorCodeToString(Error: UInt32): string;
const
  errorMessage: array[0..7] of string = (
    'No error',
    'No heap memory',
    'No virtual memory',
    'Unknown instruction',
    'Illegal instruction',
    'Illegal addressing',
    'Illegal short jump',
    'Unknown error'
  );
begin
  if (Error > _ERROR_COUNT) then
    Error := _ERROR_COUNT;

  Result := errorMessage[Error];
end;

end.
