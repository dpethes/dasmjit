unit DAsmJit_Serializer;

{$I DAsmJit.inc}

interface

uses
  DAsmJit, DAsmJit_Defs, DAsmJit_MemoryManager, DAsmJit_Util;

const
  MAX_INLINE_COMMENT_SIZE = 255;
  BufSize = 64 - (2 * SizeOf(Pointer));

type
  PLabel = ^TLabel;

  PBaseData = ^TBaseData;
  TBaseData = record
    op: UInt8;
    size: UInt8;
  end;

  PRegData = ^TRegData;
  TRegData = record
    op: UInt8;
    size: UInt8;
    code: UInt8;
    reserved: UInt8;
  end;

  PMemData = ^TMemData;
  TMemData = record
    op: UInt8;
    size: UInt8;
    base: UInt8;
    index: UInt8;
    shift: UInt8;
    segmentPrefix: UInt8;
    hasLabel: Boolean;
    reserved: UInt8;
    displacement: SysInt;

    case Integer of
      0: (target: Pointer);
      1: (lbl: PLabel);
  end;

  PImmData = ^TImmData;
  TImmData = record
    op: UInt8;
    size: UInt8;
    isUnsigned: Boolean;
    relocMode: UInt8;
    value: SysInt;
  end;

  PLblData = ^TLblData;
  TLblData = record
    op: UInt8;
    size: UInt8;
    state: UInt8;
    reserved: UInt8;
    id: UInt32;
    position: SysInt;
    link: Pointer;
  end;

  TOperandUnion = record
    case Integer of
      0: (_base: TBaseData);
      1: (_reg: TRegData);
      2: (_mem: TMemData);
      3: (_imm: TImmData);
      4: (_lbl: TLblData);
      5: (_buf: array[0..BufSize - 1] of UInt8);
  end;

  POperand = ^TOperand;
  TOperand = object
  protected
    FCompilerData: Pointer;
    FOperandID: UInt32;
    {$IFDEF ASMJIT_X64}
    Fx64Padding: UInt32;
    {$ENDIF}
  public
    u: TOperandUnion;

    constructor Create; overload;
    constructor Create(Other: TOperand); overload;
    constructor Create(x: _DontInitialize); overload;

    function op: UInt8;
    function isNone: Boolean;
    function isReg: Boolean;
    function isMem: Boolean;
    function isImm: Boolean;
    function isLabel: Boolean;
    function isRegType(regType: UInt8): Boolean;
    function isRegCode(regCode: UInt8): Boolean;
    function isRegIndex(regIndex: UInt8): Boolean;
    function isRegMem: Boolean; overload;
    function isRegMem(regType: UInt8): Boolean; overload;
    function Size: UInt8;
    procedure ClearId;

    procedure _init(other: TOperand);
    procedure _copy(other: Toperand);

    property CompilerData: Pointer read FCompilerData write FCompilerData;
    property OperandID: UInt32 read FOperandID write FOperandID;
  end;

  //PBaseReg = ^TBaseReg;
  TBaseReg = object(TOperand)
  public
    constructor Create(ACode, ASize: UInt8); overload;

    function Typ: UInt8;
    function Code: UInt8;
    function Index: UInt8;
    function isRegType(regType: UInt8): Boolean;
    function isRegCode(regCode: UInt8): Boolean;
    function isRegIndex(regIndex: UInt8): Boolean;
    procedure setCode(ACode: UInt8);
    procedure setSize(ASize: UInt8);
  end;

  //PRegister = ^TRegister;
  TRegister = object(TBaseReg)
  public
    constructor Create; overload;
    constructor Create(x: _Initialize; ACode: UInt8); overload;
  end;

  //PX87Register = ^TX87Register;
  TX87Register = object(TBaseReg)
  public
    constructor Create; overload;
    constructor Create(x: _Initialize; ACode: UInt8); overload;
  end;

  //PMMRegister = ^TMMRegister;
  TMMRegister = object(TBaseReg)
  public
    constructor Create; overload;
    constructor Create(x: _Initialize; ACode: UInt8); overload;
  end;

  //PXMMRegister = ^TXMMRegister;
  TXMMRegister = object(TBaseReg)
  public
    constructor Create; overload;
    constructor Create(x: _Initialize; ACode: UInt8); overload;
  end;

  TLabel = object(TOperand)
  public
    constructor Create(id: UInt32 = 0); overload;
    destructor Destroy;

    procedure unuse;
    function state: UInt8;
    function labelID: UInt32;
    function isUnused: Boolean;
    function isLinked: Boolean;
    function isBound: Boolean;
    function position: SysInt;
    procedure setId(id: UInt32);
    procedure setStatePos(AState: UInt8; APosition: SysInt);
  end;

  PMem = ^TMem;
  TMem = object(TOperand)
  public
    constructor Create; overload;
    constructor Create(ALbl: PLabel; ADisplacement: SysInt; ASize: UInt8 = 0); overload;
    constructor Create(ABase: TRegister; ADisplacement: SysInt; ASize: UInt8 = 0); overload;
    constructor Create(ABase, AIndex: TRegister; AShift: UInt32; ADisplacement: SysInt; ASize: UInt8 = 0); overload;

    function hasBase: Boolean;
    function hasIndex: Boolean;
    function Base: UInt8;
    function Index: UInt8;
    function Shift: UInt8;
    function segmentPrefix: UInt8;
    function hasLabel: Boolean;
    function hasTarget: Boolean;
    function Lbl: TLabel;
    function Target: Pointer;
    function Displacement: SysInt;
    procedure setDisplacement(ADisplacement: SysInt);
  end;

  PImmediate = ^TImmediate;
  TImmediate = object(TOperand)
  public
    constructor Create; overload;
    constructor Create(i: SysInt); overload;
    constructor Create(i: SysInt; AIsUnsigned: Boolean); overload;

    function isUnsigned: Boolean;
    function relocMode: UInt8;
    function value: SysInt;
    function uvalue: SysUInt;
    procedure setValue(val: SysInt; AIsUnsigned: Boolean = False);
    procedure setUValue(val: SysUInt);
  end;

type
  TSerializerCore = class
  protected
    FLogger: TCustomLogger;
    FError: UInt32;
    //FZone: TZone;
    FMemZone: TPodVector;
    FProperties: UInt32;

    procedure setError(Error: UInt32); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function getProperty(key: UInt32): UInt32;
    function setProperty(key, value: UInt32): UInt32;
    procedure _inlineComment(Text: string); virtual; abstract;
    procedure _emitX86(code: UInt32; o1, o2, o3: POperand); virtual; abstract;
    procedure emitX86(code: UInt32); overload;
    procedure emitX86(code: UInt32; o1: TOperand); overload;
    procedure emitX86(code: UInt32; o1, o2: TOperand); overload;
    procedure emitX86(code: UInt32; o1, o2, o3: TOperand); overload;
    procedure _emitJcc(code: UInt32; Lbl: TLabel; hint: UInt32);
    procedure _embed(dataPtr: Pointer; dataLen: SysUInt); virtual; abstract;
    procedure Align(m: SysInt); virtual; abstract;
    procedure Bind(Lbl: PLabel); virtual; abstract;
    //function _zoneAlloc(Size: SysUInt): Pointer;
    procedure clearError; virtual;
    function Make(DAsmJit_MemoryManager: TMemoryManager; allocType: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer; virtual; abstract;
    function conditionToJCC(cc: SysInt): UInt32;
    function conditionToCMovCC(cc: SysInt): UInt32;
    function conditionToSetCC(cc: SysInt): UInt32;

    property Logger: TCustomLogger read FLogger write FLogger;
    property Error: UInt32 read FError write setError;
    property Properties: UInt32 read FProperties write FProperties;
  end;

  TSerializerIntrinsics = class(TSerializerCore)
  public
    procedure db(x: UInt8);
    procedure dw(x: UInt16);
    procedure dd(x: UInt32);
    procedure dq(x: UInt64);
    procedure dint8(x: Int8);
    procedure duint8(x: UInt8);
    procedure dint16(x: Int16);
    procedure duint16(x: UInt16);
    procedure dint32(x: Int32);
    procedure duint32(x: UInt32);
    procedure dint64(x: Int64);
    procedure duint64(x: UInt64);
    procedure dsysint(x: SysInt);
    procedure dsysuint(x: SysUInt);
    procedure dfloat(x: Single);
    procedure ddouble(x: Double);
    procedure dptr(x: Pointer);
    procedure dmm(x: TMMData);
    procedure dxmm(x: TXMMData);
    procedure data(Data: Pointer; Size: SysUInt);
    //procedure dstruct(const T& x);
    procedure adc(dst: TRegister; src: TRegister); overload;
    procedure adc(dst: TRegister; src: TMem); overload;
    procedure adc(dst: TRegister; src: TImmediate); overload;
    procedure adc(dst: TMem; src: TRegister); overload;
    procedure adc(dst: TMem; src: TImmediate); overload;
    procedure add(dst: TRegister; src: TRegister); overload;
    procedure add(dst: TRegister; src: TMem); overload;
    procedure add(dst: TRegister; src: TImmediate);  overload;
    procedure add(dst: TMem; src: TRegister); overload;
    procedure add(dst: TMem; src: TImmediate); overload;
    procedure and_(dst: TRegister; src: TRegister); overload;
    procedure and_(dst: TRegister; src: TMem); overload;
    procedure and_(dst: TRegister; src: TImmediate); overload;
    procedure and_(dst: TMem; src: TRegister); overload;
    procedure and_(dst: TMem; src: TImmediate); overload;
    procedure bsf(dst: TRegister; src: TRegister); overload;
    procedure bsf(dst: TRegister; src: TMem); overload;
    procedure bsr(dst: TRegister; src: TRegister); overload;
    procedure bsr(dst: TRegister; src: TMem); overload;
    procedure bswap(dst: TRegister);
    procedure bt(dst: TRegister; src: TRegister); overload;
    procedure bt(dst: TRegister; src: TImmediate); overload;
    procedure bt(dst: TMem; src: TRegister); overload;
    procedure bt(dst: TMem; src: TImmediate); overload;
    procedure btc(dst: TRegister; src: TRegister); overload;
    procedure btc(dst: TRegister; src: TImmediate); overload;
    procedure btc(dst: TMem; src: TRegister); overload;
    procedure btc(dst: TMem; src: TImmediate); overload;
    procedure btr(dst: TRegister; src: TRegister); overload;
    procedure btr(dst: TRegister; src: TImmediate); overload;
    procedure btr(dst: TMem; src: TRegister); overload;
    procedure btr(dst: TMem; src: TImmediate); overload;
    procedure bts(dst: TRegister; src: TRegister); overload;
    procedure bts(dst: TRegister; src: TImmediate); overload;
    procedure bts(dst: TMem; src: TRegister); overload;
    procedure bts(dst: TMem; src: TImmediate); overload;
    procedure call(dst: TRegister); overload;
    procedure call(dst: TMem); overload;
    procedure call(dst: TImmediate); overload;
    procedure call(dst: Pointer); overload;
    procedure call(Lbl: TLabel); overload;
    procedure cbw;
    procedure cwde;
{$IFDEF ASMJIT_X64}
    procedure cdqe;
{$ENDIF}
    procedure clc;
    procedure cld;
    procedure cmc;
    procedure cmov(cc: SysInt; dst: TRegister; src: TRegister); overload;
    procedure cmov(cc: SysInt; dst: TRegister; src: TMem); overload;
    procedure cmova  (dst: TRegister; src: TRegister); overload;
    procedure cmova  (dst: TRegister; src: TMem); overload;
    procedure cmovae (dst: TRegister; src: TRegister); overload;
    procedure cmovae (dst: TRegister; src: TMem); overload;
    procedure cmovb  (dst: TRegister; src: TRegister); overload;
    procedure cmovb  (dst: TRegister; src: TMem); overload;
    procedure cmovbe (dst: TRegister; src: TRegister); overload;
    procedure cmovbe (dst: TRegister; src: TMem); overload;
    procedure cmovc  (dst: TRegister; src: TRegister); overload;
    procedure cmovc  (dst: TRegister; src: TMem); overload;
    procedure cmove  (dst: TRegister; src: TRegister); overload;
    procedure cmove  (dst: TRegister; src: TMem); overload;
    procedure cmovg  (dst: TRegister; src: TRegister); overload;
    procedure cmovg  (dst: TRegister; src: TMem); overload;
    procedure cmovge (dst: TRegister; src: TRegister); overload;
    procedure cmovge (dst: TRegister; src: TMem); overload;
    procedure cmovl  (dst: TRegister; src: TRegister); overload;
    procedure cmovl  (dst: TRegister; src: TMem); overload;
    procedure cmovle (dst: TRegister; src: TRegister); overload;
    procedure cmovle (dst: TRegister; src: TMem); overload;
    procedure cmovna (dst: TRegister; src: TRegister); overload;
    procedure cmovna (dst: TRegister; src: TMem); overload;
    procedure cmovnae(dst: TRegister; src: TRegister); overload;
    procedure cmovnae(dst: TRegister; src: TMem); overload;
    procedure cmovnb (dst: TRegister; src: TRegister); overload;
    procedure cmovnb (dst: TRegister; src: TMem); overload;
    procedure cmovnbe(dst: TRegister; src: TRegister); overload;
    procedure cmovnbe(dst: TRegister; src: TMem); overload;
    procedure cmovnc (dst: TRegister; src: TRegister); overload;
    procedure cmovnc (dst: TRegister; src: TMem); overload;
    procedure cmovne (dst: TRegister; src: TRegister); overload;
    procedure cmovne (dst: TRegister; src: TMem); overload;
    procedure cmovng (dst: TRegister; src: TRegister); overload;
    procedure cmovng (dst: TRegister; src: TMem); overload;
    procedure cmovnge(dst: TRegister; src: TRegister); overload;
    procedure cmovnge(dst: TRegister; src: TMem); overload;
    procedure cmovnl (dst: TRegister; src: TRegister); overload;
    procedure cmovnl (dst: TRegister; src: TMem); overload;
    procedure cmovnle(dst: TRegister; src: TRegister); overload;
    procedure cmovnle(dst: TRegister; src: TMem); overload;
    procedure cmovno (dst: TRegister; src: TRegister); overload;
    procedure cmovno (dst: TRegister; src: TMem); overload;
    procedure cmovnp (dst: TRegister; src: TRegister); overload;
    procedure cmovnp (dst: TRegister; src: TMem); overload;
    procedure cmovns (dst: TRegister; src: TRegister); overload;
    procedure cmovns (dst: TRegister; src: TMem); overload;
    procedure cmovnz (dst: TRegister; src: TRegister); overload;
    procedure cmovnz (dst: TRegister; src: TMem); overload;
    procedure cmovo  (dst: TRegister; src: TRegister); overload;
    procedure cmovo  (dst: TRegister; src: TMem); overload;
    procedure cmovp  (dst: TRegister; src: TRegister); overload;
    procedure cmovp  (dst: TRegister; src: TMem); overload;
    procedure cmovpe (dst: TRegister; src: TRegister); overload;
    procedure cmovpe (dst: TRegister; src: TMem); overload;
    procedure cmovpo (dst: TRegister; src: TRegister); overload;
    procedure cmovpo (dst: TRegister; src: TMem); overload;
    procedure cmovs  (dst: TRegister; src: TRegister); overload;
    procedure cmovs  (dst: TRegister; src: TMem); overload;
    procedure cmovz  (dst: TRegister; src: TRegister); overload;
    procedure cmovz  (dst: TRegister; src: TMem); overload;
    procedure cmp(dst: TRegister; src: TRegister); overload;
    procedure cmp(dst: TRegister; src: TMem); overload;
    procedure cmp(dst: TRegister; src: TImmediate); overload;
    procedure cmp(dst: TMem; src: TRegister); overload;
    procedure cmp(dst: TMem; src: TImmediate); overload;
    procedure cmpxchg(dst: TRegister; src: TRegister); overload;
    procedure cmpxchg(dst: TMem; src: TRegister); overload;
    procedure cmpxchg8b(dst: TMem);
{$IFDEF ASMJIT_X64}
    procedure cmpxchg16b(dst: TMem);
{$ENDIF}
    procedure cpuid;
{$IFDEF ASMJIT_X86}
    procedure daa;
{$ENDIF}
{$IFDEF ASMJIT_X86}
    procedure das;
{$ENDIF}
    procedure dec_(dst: TRegister); overload;
    procedure dec_(dst: TMem); overload;
    procedure div_(src: TRegister); overload;
    procedure div_(src: TMem); overload;
    procedure enter(imm16: TImmediate; imm8: TImmediate);
    procedure idiv(src: TRegister); overload;
    procedure idiv(src: TMem); overload;
    procedure imul(src: TRegister); overload;
    procedure imul(src: TMem); overload;
    procedure imul(dst: TRegister; src: TRegister); overload;
    procedure imul(dst: TRegister; src: TMem); overload;
    procedure imul(dst: TRegister; src: TImmediate); overload;
    procedure imul(dst: TRegister; src: TRegister; imm: TImmediate); overload;
    procedure imul(dst: TRegister; src: TMem; imm: TImmediate); overload;
    procedure inc_(dst: TRegister); overload;
    procedure inc_(dst: TMem); overload;
    procedure int3;
    procedure j(cc: SysInt; Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure ja  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jae (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jb  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jbe (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jc  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure je  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jg  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jge (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jl  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jle (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jna (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnae(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnb (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnbe(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnc (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jne (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jng (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnge(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnl (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnle(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jno (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnp (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jns (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnz (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jo  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jp  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jpe (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jpo (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure js  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jz  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure j_short(cc: SysInt; Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure ja_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jae_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jb_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jbe_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jc_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure je_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jg_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jge_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jl_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jle_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jna_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnae_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnb_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnbe_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnc_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jne_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jng_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnge_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnl_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnle_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jno_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnp_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jns_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jnz_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jo_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jp_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jpe_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jpo_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure js_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jz_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
    procedure jmp(dst: TRegister); overload;
    procedure jmp(dst: TMem); overload;
    procedure jmp(dst: TImmediate); overload;
    procedure jmp(dst: Pointer); overload;
    procedure jmp(Lbl: TLabel); overload;
    procedure jmp_short(Lbl: TLabel);
    procedure lea(dst: TRegister; src: TMem);
    procedure leave;
    procedure DAsmJit_Lock;
    procedure mov(dst: TRegister; src: TRegister); overload;
    procedure mov(dst: TRegister; src: TMem); overload;
    procedure mov(dst: TRegister; src: TImmediate); overload;
    procedure mov(dst: TMem; src: TRegister); overload;
    procedure mov(dst: TMem; src: TImmediate); overload;
    procedure mov_ptr(dst: TRegister; src: Pointer); overload;
    procedure mov_ptr(dst: Pointer; src: TRegister); overload;
    procedure movsx(dst: TRegister; src: TRegister); overload;
    procedure movsx(dst: TRegister; src: TMem); overload;
{$IFDEF ASMJIT_X64}
    procedure movsxd(dst: TRegister; src: TRegister); overload;
    procedure movsxd(dst: TRegister; src: TMem); overload;
{$ENDIF}
    procedure movzx(dst: TRegister; src: TRegister); overload;
    procedure movzx(dst: TRegister; src: TMem); overload;
    procedure mul(src: TRegister); overload;
    procedure mul(src: TMem); overload;
    procedure neg(dst: TRegister); overload;
    procedure neg(dst: TMem); overload;
    procedure nop;
    procedure not_(dst: TRegister); overload;
    procedure not_(dst: TMem); overload;
    procedure or_(dst: TRegister; src: TRegister); overload;
    procedure or_(dst: TRegister; src: TMem); overload;
    procedure or_(dst: TRegister; src: TImmediate); overload;
    procedure or_(dst: TMem; src: TRegister); overload;
    procedure or_(dst: TMem; src: TImmediate); overload;
    procedure pop(dst: TRegister); overload;
    procedure pop(dst: TMem); overload;
{$IFDEF ASMJIT_X86}
    procedure popad;
{$ENDIF}
    procedure popf;
{$IFDEF ASMJIT_X86}
    procedure popfd;
{$ELSE}
    procedure popfq;
{$ENDIF}
    procedure push(src: TRegister); overload;
    procedure push(src: TMem); overload;
    procedure push(src: TImmediate); overload;
{$IFDEF ASMJIT_X86}
    procedure pushad;
{$ENDIF}
    procedure pushf;
{$IFDEF ASMJIT_X86}
    procedure pushfd;
{$ELSE}
    procedure pushfq;
{$ENDIF}
    procedure rcl(dst: TRegister; src: TRegister); overload;
    procedure rcl(dst: TRegister; src: TImmediate); overload;
    procedure rcl(dst: TMem; src: TRegister); overload;
    procedure rcl(dst: TMem; src: TImmediate); overload;
    procedure rcr(dst: TRegister; src: TRegister); overload;
    procedure rcr(dst: TRegister; src: TImmediate); overload;
    procedure rcr(dst: TMem; src: TRegister); overload;
    procedure rcr(dst: TMem; src: TImmediate); overload;
    procedure rdtsc;
    procedure rdtscp;
    procedure ret; overload;
    procedure ret(imm16: TImmediate); overload;
    procedure rol(dst: TRegister; src: TRegister); overload;
    procedure rol(dst: TRegister; src: TImmediate); overload;
    procedure rol(dst: TMem; src: TRegister); overload;
    procedure rol(dst: TMem; src: TImmediate); overload;
    procedure ror(dst: TRegister; src: TRegister); overload;
    procedure ror(dst: TRegister; src: TImmediate); overload;
    procedure ror(dst: TMem; src: TRegister); overload;
    procedure ror(dst: TMem; src: TImmediate); overload;
{$IFDEF ASMJIT_X86}
    procedure sahf;
{$ENDIF}
    procedure sbb(dst: TRegister; src: TRegister); overload;
    procedure sbb(dst: TRegister; src: TMem); overload;
    procedure sbb(dst: TRegister; src: TImmediate); overload;
    procedure sbb(dst: TMem; src: TRegister); overload;
    procedure sbb(dst: TMem; src: TImmediate); overload;
    procedure sal(dst: TRegister; src: TRegister); overload;
    procedure sal(dst: TRegister; src: TImmediate); overload;
    procedure sal(dst: TMem; src: TRegister); overload;
    procedure sal(dst: TMem; src: TImmediate); overload;
    procedure sar(dst: TRegister; src: TRegister); overload;
    procedure sar(dst: TRegister; src: TImmediate); overload;
    procedure sar(dst: TMem; src: TRegister); overload;
    procedure sar(dst: TMem; src: TImmediate); overload;
    procedure set_(cc: SysInt; dst: TRegister); overload;
    procedure set_(cc: SysInt; dst: TMem); overload;
    procedure seta  (dst: TRegister); overload;
    procedure seta  (dst: TMem); overload;
    procedure setae (dst: TRegister); overload;
    procedure setae (dst: TMem); overload;
    procedure setb  (dst: TRegister); overload;
    procedure setb  (dst: TMem); overload;
    procedure setbe (dst: TRegister); overload;
    procedure setbe (dst: TMem); overload;
    procedure setc  (dst: TRegister); overload;
    procedure setc  (dst: TMem); overload;
    procedure sete  (dst: TRegister); overload;
    procedure sete  (dst: TMem); overload;
    procedure setg  (dst: TRegister); overload;
    procedure setg  (dst: TMem); overload;
    procedure setge (dst: TRegister); overload;
    procedure setge (dst: TMem); overload;
    procedure setl  (dst: TRegister); overload;
    procedure setl  (dst: TMem); overload;
    procedure setle (dst: TRegister); overload;
    procedure setle (dst: TMem); overload;
    procedure setna (dst: TRegister); overload;
    procedure setna (dst: TMem); overload;
    procedure setnae(dst: TRegister); overload;
    procedure setnae(dst: TMem); overload;
    procedure setnb (dst: TRegister); overload;
    procedure setnb (dst: TMem); overload;
    procedure setnbe(dst: TRegister); overload;
    procedure setnbe(dst: TMem); overload;
    procedure setnc (dst: TRegister); overload;
    procedure setnc (dst: TMem); overload;
    procedure setne (dst: TRegister); overload;
    procedure setne (dst: TMem); overload;
    procedure setng (dst: TRegister); overload;
    procedure setng (dst: TMem); overload;
    procedure setnge(dst: TRegister); overload;
    procedure setnge(dst: TMem); overload;
    procedure setnl (dst: TRegister); overload;
    procedure setnl (dst: TMem); overload;
    procedure setnle(dst: TRegister); overload;
    procedure setnle(dst: TMem); overload;
    procedure setno (dst: TRegister); overload;
    procedure setno (dst: TMem); overload;
    procedure setnp (dst: TRegister); overload;
    procedure setnp (dst: TMem); overload;
    procedure setns (dst: TRegister); overload;
    procedure setns (dst: TMem); overload;
    procedure setnz (dst: TRegister); overload;
    procedure setnz (dst: TMem); overload;
    procedure seto  (dst: TRegister); overload;
    procedure seto  (dst: TMem); overload;
    procedure setp  (dst: TRegister); overload;
    procedure setp  (dst: TMem); overload;
    procedure setpe (dst: TRegister); overload;
    procedure setpe (dst: TMem); overload;
    procedure setpo (dst: TRegister); overload;
    procedure setpo (dst: TMem); overload;
    procedure sets  (dst: TRegister); overload;
    procedure sets  (dst: TMem); overload;
    procedure setz  (dst: TRegister); overload;
    procedure setz  (dst: TMem); overload;
    procedure shl_(dst: TRegister; src: TRegister); overload;
    procedure shl_(dst: TRegister; src: TImmediate); overload;
    procedure shl_(dst: TMem; src: TRegister); overload;
    procedure shl_(dst: TMem; src: TImmediate); overload;
    procedure shr_(dst: TRegister; src: TRegister); overload;
    procedure shr_(dst: TRegister; src: TImmediate); overload;
    procedure shr_(dst: TMem; src: TRegister); overload;
    procedure shr_(dst: TMem; src: TImmediate); overload;
    procedure shld(dst: TRegister; src1, src2: TRegister); overload;
    procedure shld(dst: TRegister; src1: TRegister; src2: TImmediate); overload;
    procedure shld(dst: TMem; src1, src2: TRegister); overload;
    procedure shld(dst: TMem; src1: TRegister; src2: TImmediate); overload;
    procedure shrd(dst: TRegister; src1, src2: TRegister); overload;
    procedure shrd(dst: TRegister; src1: TRegister; src2: TImmediate); overload;
    procedure shrd(dst: TMem; src1, src2: TRegister); overload;
    procedure shrd(dst: TMem; src1: TRegister; src2: TImmediate); overload;
    procedure stc;
    procedure std;
    procedure sub(dst: TRegister; src: TRegister); overload;
    procedure sub(dst: TRegister; src: TMem); overload;
    procedure sub(dst: TRegister; src: TImmediate); overload;
    procedure sub(dst: TMem; src: TRegister); overload;
    procedure sub(dst: TMem; src: TImmediate); overload;
    procedure test(op1, op2: TRegister); overload;
    procedure test(op1: TRegister; op2: TImmediate); overload;
    procedure test(op1: TMem; op2: TRegister); overload;
    procedure test(op1: TMem; op2: TImmediate); overload;
    procedure ud2;
    procedure xadd(dst: TRegister; src: TRegister); overload;
    procedure xadd(dst: TMem; src: TRegister); overload;
    procedure xchg(dst: TRegister; src: TRegister); overload;
    procedure xchg(dst: TMem; src: TRegister); overload;
    procedure xchg(dst: TRegister; src: TMem); overload;
    procedure xor_(dst: TRegister; src: TRegister); overload;
    procedure xor_(dst: TRegister; src: TMem); overload;
    procedure xor_(dst: TRegister; src: TImmediate); overload;
    procedure xor_(dst: TMem; src: TRegister); overload;
    procedure xor_(dst: TMem; src: TImmediate); overload;
    procedure f2xm1;
    procedure fabs;
    procedure fadd(dst: TX87Register; src: TX87Register); overload;
    procedure fadd(src: TMem); overload;
    procedure faddp(dst: TX87Register);
    procedure fbld(src: TMem);
    procedure fbstp(dst: TMem);
    procedure fchs;
    procedure fclex;
    procedure fcmovb(src: TX87Register);
    procedure fcmovbe(src: TX87Register);
    procedure fcmove(src: TX87Register);
    procedure fcmovnb(src: TX87Register);
    procedure fcmovnbe(src: TX87Register);
    procedure fcmovne(src: TX87Register);
    procedure fcmovnu(src: TX87Register);
    procedure fcmovu(src: TX87Register);
    procedure fcom(reg: TX87Register); overload;
    procedure fcom(src: TMem); overload;
    procedure fcomp(reg: TX87Register); overload;
    procedure fcomp(mem: TMem); overload;
    procedure fcompp;
    procedure fcomi(reg: TX87Register);
    procedure fcomip(reg: TX87Register);
    procedure fcos;
    procedure fdecstp;
    procedure fdiv(dst: TX87Register; src: TX87Register); overload;
    procedure fdiv(src: TMem); overload;
    procedure fdivp(reg: TX87Register);
    procedure fdivr(dst: TX87Register; src: TX87Register); overload;
    procedure fdivr(src: TMem); overload;
    procedure fdivrp(reg: TX87Register);
    procedure ffree(reg: TX87Register);
    procedure fiadd(src: TMem);
    procedure ficom(src: TMem);
    procedure ficomp(src: TMem);
    procedure fidiv(src: TMem);
    procedure fidivr(src: TMem);
    procedure fild(src: TMem);
    procedure fimul(src: TMem);
    procedure fincstp;
    procedure finit;
    procedure fisub(src: TMem);
    procedure fisubr(src: TMem);
    procedure fninit;
    procedure fist(dst: TMem);
    procedure fistp(dst: TMem);
    procedure fld(src: TMem); overload;
    procedure fld(reg: TX87Register); overload;
    procedure fld1;
    procedure fldl2t;
    procedure fldl2e;
    procedure fldpi;
    procedure fldlg2;
    procedure fldln2;
    procedure fldz;
    procedure fldcw(src: TMem);
    procedure fldenv(src: TMem);
    procedure fmul(dst: TX87Register; src: TX87Register); overload;
    procedure fmul(src: TMem); overload;
    procedure fmulp(dst: TX87Register);
    procedure fnclex;
    procedure fnop;
    procedure fnsave(dst: TMem);
    procedure fnstenv(dst: TMem);
    procedure fnstcw(dst: TMem);
    procedure fnstsw(dst: TRegister); overload;
    procedure fnstsw(dst: TMem); overload;
    procedure fpatan;
    procedure fprem;
    procedure fprem1;
    procedure fptan;
    procedure frndint;
    procedure frstor(src: TMem);
    procedure fsave(dst: TMem);
    procedure fscale;
    procedure fsin;
    procedure fsincos;
    procedure fsqrt;
    procedure fst(dst: TMem); overload;
    procedure fst(reg: TX87Register); overload;
    procedure fstp(dst: TMem); overload;
    procedure fstp(reg: TX87Register); overload;
    procedure fstcw(dst: TMem);
    procedure fstenv(dst: TMem);
    procedure fstsw(dst: TRegister); overload;
    procedure fstsw(dst: TMem); overload;
    procedure fsub(dst: TX87Register; src: TX87Register); overload;
    procedure fsub(src: TMem); overload;
    procedure fsubp(dst: TX87Register);
    procedure fsubr(dst: TX87Register; src: TX87Register); overload;
    procedure fsubr(src: TMem); overload;
    procedure fsubrp(dst: TX87Register);
    procedure ftst;
    procedure fucom(reg: TX87Register);
    procedure fucomi(reg: TX87Register);
    procedure fucomip(reg: TX87Register);
    procedure fucomp(reg: TX87Register);
    procedure fucompp;
    procedure fwait;
    procedure fxam;
    procedure fxch(reg: TX87Register);
    procedure fxrstor(src: TMem);
    procedure fxsave(dst: TMem);
    procedure fxtract;
    procedure fyl2x;
    procedure fyl2xp1;
    procedure emms;
    procedure movd(dst: TMem; src: TMMRegister); overload;
    procedure movd(dst: TRegister; src: TMMRegister); overload;
    procedure movd(dst: TMMRegister; src: TMem); overload;
    procedure movd(dst: TMMRegister; src: TRegister); overload;
    procedure movq(dst: TMMRegister; src: TMMRegister); overload;
    procedure movq(dst: TMem; src: TMMRegister); overload;
{$IFDEF ASMJIT_X64}
    procedure movq(dst: TRegister; src: TMMRegister); overload;
{$ENDIF}
    procedure movq(dst: TMMRegister; src: TMem); overload;
{$IFDEF ASMJIT_X64}
    procedure movq(dst: TMMRegister; src: TRegister); overload;
{$ENDIF}
    procedure packuswb(dst: TMMRegister; src: TMMRegister); overload;
    procedure packuswb(dst: TMMRegister; src: TMem); overload;
    procedure paddb(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddb(dst: TMMRegister; src: TMem); overload;
    procedure paddw(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddw(dst: TMMRegister; src: TMem); overload;
    procedure paddd(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddd(dst: TMMRegister; src: TMem); overload;
    procedure paddsb(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddsb(dst: TMMRegister; src: TMem); overload;
    procedure paddsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddsw(dst: TMMRegister; src: TMem); overload;
    procedure paddusb(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddusb(dst: TMMRegister; src: TMem); overload;
    procedure paddusw(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddusw(dst: TMMRegister; src: TMem); overload;
    procedure pand(dst: TMMRegister; src: TMMRegister); overload;
    procedure pand(dst: TMMRegister; src: TMem); overload;
    procedure pandn(dst: TMMRegister; src: TMMRegister); overload;
    procedure pandn(dst: TMMRegister; src: TMem); overload;
    procedure pcmpeqb(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpeqb(dst: TMMRegister; src: TMem); overload;
    procedure pcmpeqw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpeqw(dst: TMMRegister; src: TMem); overload;
    procedure pcmpeqd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpeqd(dst: TMMRegister; src: TMem); overload;
    procedure pcmpgtb(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpgtb(dst: TMMRegister; src: TMem); overload;
    procedure pcmpgtw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpgtw(dst: TMMRegister; src: TMem); overload;
    procedure pcmpgtd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pcmpgtd(dst: TMMRegister; src: TMem); overload;
    procedure pmulhw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmulhw(dst: TMMRegister; src: TMem); overload;
    procedure pmullw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmullw(dst: TMMRegister; src: TMem); overload;
    procedure por(dst: TMMRegister; src: TMMRegister); overload;
    procedure por(dst: TMMRegister; src: TMem); overload;
    procedure pmaddwd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmaddwd(dst: TMMRegister; src: TMem); overload;
    procedure pslld(dst: TMMRegister; src: TMMRegister); overload;
    procedure pslld(dst: TMMRegister; src: TMem); overload;
    procedure pslld(dst: TMMRegister; src: TImmediate); overload;
    procedure psllq(dst: TMMRegister; src: TMMRegister); overload;
    procedure psllq(dst: TMMRegister; src: TMem); overload;
    procedure psllq(dst: TMMRegister; src: TImmediate); overload;
    procedure psllw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psllw(dst: TMMRegister; src: TMem); overload;
    procedure psllw(dst: TMMRegister; src: TImmediate); overload;
    procedure psrad(dst: TMMRegister; src: TMMRegister); overload;
    procedure psrad(dst: TMMRegister; src: TMem); overload;
    procedure psrad(dst: TMMRegister; src: TImmediate); overload;
    procedure psraw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psraw(dst: TMMRegister; src: TMem); overload;
    procedure psraw(dst: TMMRegister; src: TImmediate); overload;
    procedure psrld(dst: TMMRegister; src: TMMRegister); overload;
    procedure psrld(dst: TMMRegister; src: TMem); overload;
    procedure psrld(dst: TMMRegister; src: TImmediate); overload;
    procedure psrlq(dst: TMMRegister; src: TMMRegister); overload;
    procedure psrlq(dst: TMMRegister; src: TMem); overload;
    procedure psrlq(dst: TMMRegister; src: TImmediate); overload;
    procedure psrlw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psrlw(dst: TMMRegister; src: TMem); overload;
    procedure psrlw(dst: TMMRegister; src: TImmediate); overload;
    procedure psubb(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubb(dst: TMMRegister; src: TMem); overload;
    procedure psubw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubw(dst: TMMRegister; src: TMem); overload;
    procedure psubd(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubd(dst: TMMRegister; src: TMem); overload;
    procedure psubsb(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubsb(dst: TMMRegister; src: TMem); overload;
    procedure psubsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubsw(dst: TMMRegister; src: TMem); overload;
    procedure psubusb(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubusb(dst: TMMRegister; src: TMem); overload;
    procedure psubusw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubusw(dst: TMMRegister; src: TMem); overload;
    procedure punpckhbw(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpckhbw(dst: TMMRegister; src: TMem); overload;
    procedure punpckhwd(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpckhwd(dst: TMMRegister; src: TMem); overload;
    procedure punpckhdq(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpckhdq(dst: TMMRegister; src: TMem); overload;
    procedure punpcklbw(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpcklbw(dst: TMMRegister; src: TMem); overload;
    procedure punpcklwd(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpcklwd(dst: TMMRegister; src: TMem); overload;
    procedure punpckldq(dst: TMMRegister; src: TMMRegister); overload;
    procedure punpckldq(dst: TMMRegister; src: TMem); overload;
    procedure pxor(dst: TMMRegister; src: TMMRegister); overload;
    procedure pxor(dst: TMMRegister; src: TMem); overload;
    procedure femms;
    procedure pf2id(dst: TMMRegister; src: TMMRegister); overload;
    procedure pf2id(dst: TMMRegister; src: TMem); overload;
    procedure pf2iw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pf2iw(dst: TMMRegister; src: TMem); overload;
    procedure pfacc(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfacc(dst: TMMRegister; src: TMem); overload;
    procedure pfadd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfadd(dst: TMMRegister; src: TMem); overload;
    procedure pfcmpeq(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfcmpeq(dst: TMMRegister; src: TMem); overload;
    procedure pfcmpge(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfcmpge(dst: TMMRegister; src: TMem); overload;
    procedure pfcmpgt(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfcmpgt(dst: TMMRegister; src: TMem); overload;
    procedure pfmax(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfmax(dst: TMMRegister; src: TMem); overload;
    procedure pfmin(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfmin(dst: TMMRegister; src: TMem); overload;
    procedure pfmul(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfmul(dst: TMMRegister; src: TMem); overload;
    procedure pfnacc(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfnacc(dst: TMMRegister; src: TMem); overload;
    procedure pfpnaxx(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfpnacc(dst: TMMRegister; src: TMem); overload;
    procedure pfrcp(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfrcp(dst: TMMRegister; src: TMem); overload;
    procedure pfrcpit1(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfrcpit1(dst: TMMRegister; src: TMem); overload;
    procedure pfrcpit2(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfrcpit2(dst: TMMRegister; src: TMem); overload;
    procedure pfrsqit1(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfrsqit1(dst: TMMRegister; src: TMem); overload;
    procedure pfrsqrt(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfrsqrt(dst: TMMRegister; src: TMem); overload;
    procedure pfsub(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfsub(dst: TMMRegister; src: TMem); overload;
    procedure pfsubr(dst: TMMRegister; src: TMMRegister); overload;
    procedure pfsubr(dst: TMMRegister; src: TMem); overload;
    procedure pi2fd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pi2fd(dst: TMMRegister; src: TMem); overload;
    procedure pi2fw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pi2fw(dst: TMMRegister; src: TMem); overload;
    procedure pswapd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pswapd(dst: TMMRegister; src: TMem); overload;
    procedure addps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addps(dst: TXMMRegister; src: TMem); overload;
    procedure addss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addss(dst: TXMMRegister; src: TMem); overload;
    procedure andnps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure andnps(dst: TXMMRegister; src: TMem); overload;
    procedure andps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure andps(dst: TXMMRegister; src: TMem); overload;
    procedure cmpps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure cmpps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure cmpss(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure cmpss(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure comiss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure comiss(dst: TXMMRegister; src: TMem); overload;
    procedure cvtpi2ps(dst: TXMMRegister; src: TMMRegister); overload;
    procedure cvtpi2ps(dst: TXMMRegister; src: TMem); overload;
    procedure cvtps2pi(dst: TMMRegister; src: TXMMRegister); overload;
    procedure cvtps2pi(dst: TMMRegister; src: TMem); overload;
    procedure cvtsi2ss(dst: TXMMRegister; src: TRegister); overload;
    procedure cvtsi2ss(dst: TXMMRegister; src: TMem); overload;
    procedure cvtss2si(dst: TRegister; src: TXMMRegister); overload;
    procedure cvtss2si(dst: TRegister; src: TMem); overload;
    procedure cvttps2pi(dst: TMMRegister; src: TXMMRegister); overload;
    procedure cvttps2pi(dst: TMMRegister; src: TMem); overload;
    procedure cvttss2si(dst: TRegister; src: TXMMRegister); overload;
    procedure cvttss2si(dst: TRegister; src: TMem); overload;
    procedure divps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure divps(dst: TXMMRegister; src: TMem); overload;
    procedure divss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure divss(dst: TXMMRegister; src: TMem); overload;
    procedure ldmxcsr(src: TMem);
    procedure maskmovq(data: TMMRegister; mask: TMMRegister);
    procedure maxps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure maxps(dst: TXMMRegister; src: TMem); overload;
    procedure maxss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure maxss(dst: TXMMRegister; src: TMem); overload;
    procedure minps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure minps(dst: TXMMRegister; src: TMem); overload;
    procedure minss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure minss(dst: TXMMRegister; src: TMem); overload;
    procedure movaps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movaps(dst: TXMMRegister; src: TMem); overload;
    procedure movaps(dst: TMem; src: TXMMRegister); overload;
    procedure movd(dst: TMem; src: TXMMRegister); overload;
    procedure movd(dst: TRegister; src: TXMMRegister); overload;
    procedure movd(dst: TXMMRegister; src: TMem); overload;
    procedure movd(dst: TXMMRegister; src: TRegister); overload;
    procedure movq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movq(dst: TMem; src: TXMMRegister); overload;
{$IFDEF ASMJIT_X64}
    procedure movq(dst: TRegister; src: TXMMRegister); overload;
{$ENDIF}
    procedure movq(dst: TXMMRegister; src: TMem); overload;
{$IFDEF ASMJIT_X64}
    procedure movq(dst: TXMMRegister; src: TRegister); overload;
{$ENDIF}
    procedure movntq(dst: TMem; src: TMMRegister); overload;
    procedure movhlps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movhps(dst: TXMMRegister; src: TMem); overload;
    procedure movhps(dst: TMem; src: TXMMRegister); overload;
    procedure movlhps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movlps(dst: TXMMRegister; src: TMem); overload;
    procedure movlps(dst: TMem; src: TXMMRegister); overload;
    procedure movntps(dst: TMem; src: TXMMRegister); overload;
    procedure movss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movss(dst: TXMMRegister; src: TMem); overload;
    procedure movss(dst: TMem; src: TXMMRegister); overload;
    procedure movups(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movups(dst: TXMMRegister; src: TMem); overload;
    procedure movups(dst: TMem; src: TXMMRegister); overload;
    procedure mulps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure mulps(dst: TXMMRegister; src: TMem); overload;
    procedure mulss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure mulss(dst: TXMMRegister; src: TMem); overload;
    procedure orps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure orps(dst: TXMMRegister; src: TMem); overload;
    procedure pavgb(dst: TMMRegister; src: TMMRegister); overload;
    procedure pavgb(dst: TMMRegister; src: TMem); overload;
    procedure pavgw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pavgw(dst: TMMRegister; src: TMem); overload;
    procedure pextrw(dst: TRegister; src: TMMRegister; imm8: TImmediate); overload;
    procedure pinsrw(dst: TMMRegister; src: TRegister; imm8: TImmediate); overload;
    procedure pinsrw(dst: TMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pmaxsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmaxsw(dst: TMMRegister; src: TMem); overload;
    procedure pmaxub(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmaxub(dst: TMMRegister; src: TMem); overload;
    procedure pminsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pminsw(dst: TMMRegister; src: TMem); overload;
    procedure pminub(dst: TMMRegister; src: TMMRegister); overload;
    procedure pminub(dst: TMMRegister; src: TMem); overload;
    procedure pmovmskb(dst: TRegister; src: TMMRegister); overload;
    procedure pmulhuw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmulhuw(dst: TMMRegister; src: TMem); overload;
    procedure psadbw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psadbw(dst: TMMRegister; src: TMem); overload;
    procedure pshufw(dst: TMMRegister; src: TMMRegister; imm8: TImmediate); overload;
    procedure pshufw(dst: TMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure rcpps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure rcpps(dst: TXMMRegister; src: TMem); overload;
    procedure rcpss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure rcpss(dst: TXMMRegister; src: TMem); overload;
    procedure prefetch(mem: TMem; hint: TImmediate);
    procedure psadbw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psadbw(dst: TXMMRegister; src: TMem); overload;
    procedure rsqrtps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure rsqrtps(dst: TXMMRegister; src: TMem); overload;
    procedure rsqrtss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure rsqrtss(dst: TXMMRegister; src: TMem); overload;
    procedure sfence;
    procedure shufps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure shufps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure sqrtps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure sqrtps(dst: TXMMRegister; src: TMem); overload;
    procedure sqrtss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure sqrtss(dst: TXMMRegister; src: TMem); overload;
    procedure stmxcsr(dst: TMem);
    procedure subps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure subps(dst: TXMMRegister; src: TMem); overload;
    procedure subss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure subss(dst: TXMMRegister; src: TMem); overload;
    procedure ucomiss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure ucomiss(dst: TXMMRegister; src: TMem); overload;
    procedure unpckhps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure unpckhps(dst: TXMMRegister; src: TMem); overload;
    procedure unpcklps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure unpcklps(dst: TXMMRegister; src: TMem); overload;
    procedure xorps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure xorps(dst: TXMMRegister; src: TMem); overload;
    procedure addpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addpd(dst: TXMMRegister; src: TMem); overload;
    procedure addsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addsd(dst: TXMMRegister; src: TMem); overload;
    procedure andnpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure andnpd(dst: TXMMRegister; src: TMem); overload;
    procedure andpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure andpd(dst: TXMMRegister; src: TMem); overload;
    procedure clflush(mem: TMem);
    procedure cmppd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure cmppd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure cmpsd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure cmpsd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure comisd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure comisd(dst: TXMMRegister; src: TMem); overload;
    procedure cvtdq2pd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtdq2pd(dst: TXMMRegister; src: TMem); overload;
    procedure cvtdq2ps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtdq2ps(dst: TXMMRegister; src: TMem); overload;
    procedure cvtpd2dq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtpd2dq(dst: TXMMRegister; src: TMem); overload;
    procedure cvtpd2pi(dst: TMMRegister; src: TXMMRegister); overload;
    procedure cvtpd2pi(dst: TMMRegister; src: TMem); overload;
    procedure cvtpd2ps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtpd2ps(dst: TXMMRegister; src: TMem); overload;
    procedure cvtpi2pd(dst: TXMMRegister; src: TMMRegister); overload;
    procedure cvtpi2pd(dst: TXMMRegister; src: TMem); overload;
    procedure cvtps2dq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtps2dq(dst: TXMMRegister; src: TMem); overload;
    procedure cvtps2pd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtps2pd(dst: TXMMRegister; src: TMem); overload;
    procedure cvtsd2si(dst: TRegister; src: TXMMRegister); overload;
    procedure cvtsd2si(dst: TRegister; src: TMem); overload;
    procedure cvtsd2ss(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtsd2ss(dst: TXMMRegister; src: TMem); overload;
    procedure cvtsi2sd(dst: TXMMRegister; src: TRegister); overload;
    procedure cvtsi2sd(dst: TXMMRegister; src: TMem); overload;
    procedure cvtss2sd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvtss2sd(dst: TXMMRegister; src: TMem); overload;
    procedure cvttpd2pi(dst: TMMRegister; src: TXMMRegister); overload;
    procedure cvttpd2pi(dst: TMMRegister; src: TMem); overload;
    procedure cvttpd2dq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvttpd2dq(dst: TXMMRegister; src: TMem); overload;
    procedure cvttps2dq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure cvttps2dq(dst: TXMMRegister; src: TMem); overload;
    procedure cvttsd2si(dst: TRegister; src: TXMMRegister); overload;
    procedure cvttsd2si(dst: TRegister; src: TMem); overload;
    procedure divpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure divpd(dst: TXMMRegister; src: TMem); overload;
    procedure divsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure divsd(dst: TXMMRegister; src: TMem); overload;
    procedure lfence;
    procedure maskmovdqu(src: TXMMRegister; mask: TXMMRegister);
    procedure maxpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure maxpd(dst: TXMMRegister; src: TMem); overload;
    procedure maxsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure maxsd(dst: TXMMRegister; src: TMem); overload;
    procedure mfence;
    procedure minpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure minpd(dst: TXMMRegister; src: TMem); overload;
    procedure minsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure minsd(dst: TXMMRegister; src: TMem); overload;
    procedure movdqa(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movdqa(dst: TXMMRegister; src: TMem); overload;
    procedure movdqa(dst: TMem; src: TXMMRegister); overload;
    procedure movdqu(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movdqu(dst: TXMMRegister; src: TMem); overload;
    procedure movdqu(dst: TMem; src: TXMMRegister); overload;
    procedure movmskps(dst: TRegister; src: TXMMRegister);
    procedure movmskpd(dst: TRegister; src: TXMMRegister);
    procedure movsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movsd(dst: TXMMRegister; src: TMem); overload;
    procedure movsd(dst: TMem; src: TXMMRegister); overload;
    procedure movapd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movapd(dst: TXMMRegister; src: TMem); overload;
    procedure movapd(dst: TMem; src: TXMMRegister); overload;
    procedure movdq2q(dst: TMMRegister; src: TXMMRegister);
    procedure movq2dq(dst: TXMMRegister; src: TMMRegister);
    procedure movhpd(dst: TXMMRegister; src: TMem); overload;
    procedure movhpd(dst: TMem; src: TXMMRegister); overload;
    procedure movlpd(dst: TXMMRegister; src: TMem); overload;
    procedure movlpd(dst: TMem; src: TXMMRegister); overload;
    procedure movntdq(dst: TMem; src: TXMMRegister);
    procedure movnti(dst: TMem; src: TRegister);
    procedure movntpd(dst: TMem; src: TXMMRegister);
    procedure movupd(dst: TXMMRegister; src: TMem); overload;
    procedure movupd(dst: TMem; src: TXMMRegister); overload;
    procedure mulpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure mulpd(dst: TXMMRegister; src: TMem); overload;
    procedure mulsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure mulsd(dst: TXMMRegister; src: TMem); overload;
    procedure orpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure orpd(dst: TXMMRegister; src: TMem); overload;
    procedure packsswb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure packsswb(dst: TXMMRegister; src: TMem); overload;
    procedure packssdw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure packssdw(dst: TXMMRegister; src: TMem); overload;
    procedure packuswb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure packuswb(dst: TXMMRegister; src: TMem); overload;
    procedure paddb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddb(dst: TXMMRegister; src: TMem); overload;
    procedure paddw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddw(dst: TXMMRegister; src: TMem); overload;
    procedure paddd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddd(dst: TXMMRegister; src: TMem); overload;
    procedure paddq(dst: TMMRegister; src: TMMRegister); overload;
    procedure paddq(dst: TMMRegister; src: TMem); overload;
    procedure paddq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddq(dst: TXMMRegister; src: TMem); overload;
    procedure paddsb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddsb(dst: TXMMRegister; src: TMem); overload;
    procedure paddsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddsw(dst: TXMMRegister; src: TMem); overload;
    procedure paddusb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddusb(dst: TXMMRegister; src: TMem); overload;
    procedure paddusw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure paddusw(dst: TXMMRegister; src: TMem); overload;
    procedure pand(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pand(dst: TXMMRegister; src: TMem); overload;
    procedure pandn(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pandn(dst: TXMMRegister; src: TMem); overload;
    procedure pause;
    procedure pavgb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pavgb(dst: TXMMRegister; src: TMem); overload;
    procedure pavgw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pavgw(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpeqb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpeqb(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpeqw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpeqw(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpeqd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpeqd(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpgtb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpgtb(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpgtw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpgtw(dst: TXMMRegister; src: TMem); overload;
    procedure pcmpgtd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpgtd(dst: TXMMRegister; src: TMem); overload;
    procedure pmaxsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxsw(dst: TXMMRegister; src: TMem); overload;
    procedure pmaxub(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxub(dst: TXMMRegister; src: TMem); overload;
    procedure pminsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminsw(dst: TXMMRegister; src: TMem); overload;
    procedure pminub(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminub(dst: TXMMRegister; src: TMem); overload;
    procedure pmovmskb(dst: TRegister; src: TXMMRegister); overload;
    procedure pmulhw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmulhw(dst: TXMMRegister; src: TMem); overload;
    procedure pmulhuw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmulhuw(dst: TXMMRegister; src: TMem); overload;
    procedure pmullw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmullw(dst: TXMMRegister; src: TMem); overload;
    procedure pmuludq(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmuludq(dst: TMMRegister; src: TMem); overload;
    procedure pmuludq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmuludq(dst: TXMMRegister; src: TMem); overload;
    procedure por(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure por(dst: TXMMRegister; src: TMem); overload;
    procedure pslld(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pslld(dst: TXMMRegister; src: TMem); overload;
    procedure pslld(dst: TXMMRegister; src: TImmediate); overload;
    procedure psllq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psllq(dst: TXMMRegister; src: TMem); overload;
    procedure psllq(dst: TXMMRegister; src: TImmediate); overload;
    procedure psllw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psllw(dst: TXMMRegister; src: TMem); overload;
    procedure psllw(dst: TXMMRegister; src: TImmediate); overload;
    procedure pslldq(dst: TXMMRegister; src: TImmediate); overload;
    procedure psrad(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psrad(dst: TXMMRegister; src: TMem); overload;
    procedure psrad(dst: TXMMRegister; src: TImmediate); overload;
    procedure psraw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psraw(dst: TXMMRegister; src: TMem); overload;
    procedure psraw(dst: TXMMRegister; src: TImmediate); overload;
    procedure psubb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubb(dst: TXMMRegister; src: TMem); overload;
    procedure psubw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubw(dst: TXMMRegister; src: TMem); overload;
    procedure psubd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubd(dst: TXMMRegister; src: TMem); overload;
    procedure psubq(dst: TMMRegister; src: TMMRegister); overload;
    procedure psubq(dst: TMMRegister; src: TMem); overload;
    procedure psubq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubq(dst: TXMMRegister; src: TMem); overload;
    procedure pmaddwd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaddwd(dst: TXMMRegister; src: TMem); overload;
    procedure pshufd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pshufd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pshufhw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pshufhw(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pshuflw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pshuflw(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure psrld(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psrld(dst: TXMMRegister; src: TMem); overload;
    procedure psrld(dst: TXMMRegister; src: TImmediate); overload;
    procedure psrlq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psrlq(dst: TXMMRegister; src: TMem); overload;
    procedure psrlq(dst: TXMMRegister; src: TImmediate); overload;
    procedure psrldq(dst: TXMMRegister; src: TImmediate); overload;
    procedure psrlw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psrlw(dst: TXMMRegister; src: TMem); overload;
    procedure psrlw(dst: TXMMRegister; src: TImmediate); overload;
    procedure psubsb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubsb(dst: TXMMRegister; src: TMem); overload;
    procedure psubsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubsw(dst: TXMMRegister; src: TMem); overload;
    procedure psubusb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubusb(dst: TXMMRegister; src: TMem); overload;
    procedure psubusw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psubusw(dst: TXMMRegister; src: TMem); overload;
    procedure punpckhbw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpckhbw(dst: TXMMRegister; src: TMem); overload;
    procedure punpckhwd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpckhwd(dst: TXMMRegister; src: TMem); overload;
    procedure punpckhdq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpckhdq(dst: TXMMRegister; src: TMem); overload;
    procedure punpckhqdq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpckhqdq(dst: TXMMRegister; src: TMem); overload;
    procedure punpcklbw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpcklbw(dst: TXMMRegister; src: TMem); overload;
    procedure punpcklwd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpcklwd(dst: TXMMRegister; src: TMem); overload;
    procedure punpckldq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpckldq(dst: TXMMRegister; src: TMem); overload;
    procedure punpcklqdq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure punpcklqdq(dst: TXMMRegister; src: TMem); overload;
    procedure pxor(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pxor(dst: TXMMRegister; src: TMem); overload;
    procedure shufpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure shufpd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure sqrtpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure sqrtpd(dst: TXMMRegister; src: TMem); overload;
    procedure sqrtsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure sqrtsd(dst: TXMMRegister; src: TMem); overload;
    procedure subpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure subpd(dst: TXMMRegister; src: TMem); overload;
    procedure subsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure subsd(dst: TXMMRegister; src: TMem); overload;
    procedure ucomisd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure ucomisd(dst: TXMMRegister; src: TMem); overload;
    procedure unpckhpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure unpckhpd(dst: TXMMRegister; src: TMem); overload;
    procedure unpcklpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure unpcklpd(dst: TXMMRegister; src: TMem); overload;
    procedure xorpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure xorpd(dst: TXMMRegister; src: TMem); overload;
    procedure addsubpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addsubpd(dst: TXMMRegister; src: TMem); overload;
    procedure addsubps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure addsubps(dst: TXMMRegister; src: TMem); overload;
    procedure fisttp(dst: TMem);
    procedure haddpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure haddpd(dst: TXMMRegister; src: TMem); overload;
    procedure haddps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure haddps(dst: TXMMRegister; src: TMem); overload;
    procedure hsubpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure hsubpd(dst: TXMMRegister; src: TMem); overload;
    procedure hsubps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure hsubps(dst: TXMMRegister; src: TMem); overload;
    procedure lddqu(dst: TXMMRegister; src: TMem);
    procedure monitor;
    procedure movddup(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movddup(dst: TXMMRegister; src: TMem); overload;
    procedure movshdup(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movshdup(dst: TXMMRegister; src: TMem); overload;
    procedure movsldup(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure movsldup(dst: TXMMRegister; src: TMem); overload;
    procedure mwait;
    procedure psignb(dst: TMMRegister; src: TMMRegister); overload;
    procedure psignb(dst: TMMRegister; src: TMem); overload;
    procedure psignb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psignb(dst: TXMMRegister; src: TMem); overload;
    procedure psignw(dst: TMMRegister; src: TMMRegister); overload;
    procedure psignw(dst: TMMRegister; src: TMem); overload;
    procedure psignw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psignw(dst: TXMMRegister; src: TMem); overload;
    procedure psignd(dst: TMMRegister; src: TMMRegister); overload;
    procedure psignd(dst: TMMRegister; src: TMem); overload;
    procedure psignd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure psignd(dst: TXMMRegister; src: TMem); overload;
    procedure phaddw(dst: TMMRegister; src: TMMRegister); overload;
    procedure phaddw(dst: TMMRegister; src: TMem); overload;
    procedure phaddw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phaddw(dst: TXMMRegister; src: TMem); overload;
    procedure phaddd(dst: TMMRegister; src: TMMRegister); overload;
    procedure phaddd(dst: TMMRegister; src: TMem); overload;
    procedure phaddd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phaddd(dst: TXMMRegister; src: TMem); overload;
    procedure phaddsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure phaddsw(dst: TMMRegister; src: TMem); overload;
    procedure phaddsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phaddsw(dst: TXMMRegister; src: TMem); overload;
    procedure phsubw(dst: TMMRegister; src: TMMRegister); overload;
    procedure phsubw(dst: TMMRegister; src: TMem); overload;
    procedure phsubw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phsubw(dst: TXMMRegister; src: TMem); overload;
    procedure phsubd(dst: TMMRegister; src: TMMRegister); overload;
    procedure phsubd(dst: TMMRegister; src: TMem); overload;
    procedure phsubd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phsubd(dst: TXMMRegister; src: TMem); overload;
    procedure phsubsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure phsubsw(dst: TMMRegister; src: TMem); overload;
    procedure phsubsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phsubsw(dst: TXMMRegister; src: TMem); overload;
    procedure pmaddubsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmaddubsw(dst: TMMRegister; src: TMem); overload;
    procedure pmaddubsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaddubsw(dst: TXMMRegister; src: TMem); overload;
    procedure pabsb(dst: TMMRegister; src: TMMRegister); overload;
    procedure pabsb(dst: TMMRegister; src: TMem); overload;
    procedure pabsb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pabsb(dst: TXMMRegister; src: TMem); overload;
    procedure pabsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pabsw(dst: TMMRegister; src: TMem); overload;
    procedure pabsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pabsw(dst: TXMMRegister; src: TMem); overload;
    procedure pabsd(dst: TMMRegister; src: TMMRegister); overload;
    procedure pabsd(dst: TMMRegister; src: TMem); overload;
    procedure pabsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pabsd(dst: TXMMRegister; src: TMem); overload;
    procedure pmulhrsw(dst: TMMRegister; src: TMMRegister); overload;
    procedure pmulhrsw(dst: TMMRegister; src: TMem); overload;
    procedure pmulhrsw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmulhrsw(dst: TXMMRegister; src: TMem); overload;
    procedure pshufb(dst: TMMRegister; src: TMMRegister); overload;
    procedure pshufb(dst: TMMRegister; src: TMem); overload;
    procedure pshufb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pshufb(dst: TXMMRegister; src: TMem); overload;
    procedure palignr(dst: TMMRegister; src: TMMRegister; imm8: TImmediate); overload;
    procedure palignr(dst: TMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure palignr(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure palignr(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure blendpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure blendpd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure blendps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure blendps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure blendvpd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure blendvpd(dst: TXMMRegister; src: TMem); overload;
    procedure blendvps(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure blendvps(dst: TXMMRegister; src: TMem); overload;
    procedure dppd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure dppd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure dpps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure dpps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure extractps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure extractps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure movntdqa(dst: TXMMRegister; src: TMem);
    procedure mpsadbw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure mpsadbw(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure packusdw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure packusdw(dst: TXMMRegister; src: TMem); overload;
    procedure pblendvb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pblendvb(dst: TXMMRegister; src: TMem); overload;
    procedure pblendw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pblendw(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pcmpeqq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpeqq(dst: TXMMRegister; src: TMem); overload;
    procedure pextrb(dst: TRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrb(dst: TMem; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrd(dst: TRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrd(dst: TMem; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrq(dst: TRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrq(dst: TMem; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrw(dst: TRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pextrw(dst: TMem; src: TXMMRegister; imm8: TImmediate); overload;
    procedure phminposuw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure phminposuw(dst: TXMMRegister; src: TMem); overload;
    procedure pinsrb(dst: TXMMRegister; src: TRegister; imm8: TImmediate); overload;
    procedure pinsrb(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pinsrd(dst: TXMMRegister; src: TRegister; imm8: TImmediate); overload;
    procedure pinsrd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pinsrq(dst: TXMMRegister; src: TRegister; imm8: TImmediate); overload;
    procedure pinsrq(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pinsrw(dst: TXMMRegister; src: TRegister; imm8: TImmediate); overload;
    procedure pinsrw(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pmaxuw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxuw(dst: TXMMRegister; src: TMem); overload;
    procedure pmaxsb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxsb(dst: TXMMRegister; src: TMem); overload;
    procedure pmaxsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxsd(dst: TXMMRegister; src: TMem); overload;
    procedure pmaxud(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmaxud(dst: TXMMRegister; src: TMem); overload;
    procedure pminsb(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminsb(dst: TXMMRegister; src: TMem); overload;
    procedure pminuw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminuw(dst: TXMMRegister; src: TMem); overload;
    procedure pminud(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminud(dst: TXMMRegister; src: TMem); overload;
    procedure pminsd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pminsd(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxbw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxbw(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxbd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxbd(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxbq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxbq(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxwd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxwd(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxwq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxwq(dst: TXMMRegister; src: TMem); overload;
    procedure pmovsxdq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovsxdq(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxbw(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxbw(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxbd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxbd(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxbq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxbq(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxwd(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxwd(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxwq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxwq(dst: TXMMRegister; src: TMem); overload;
    procedure pmovzxdq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmovzxdq(dst: TXMMRegister; src: TMem); overload;
    procedure pmuldq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmuldq(dst: TXMMRegister; src: TMem); overload;
    procedure pmulld(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pmulld(dst: TXMMRegister; src: TMem); overload;
    procedure ptest(op1, op2: TXMMRegister); overload;
    procedure ptest(op1: TXMMRegister; op2: TMem); overload;
    procedure roundps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure roundps(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure roundss(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure roundss(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure roundpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure roundpd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure roundsd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure roundsd(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure crc32(dst: TRegister; src: TRegister); overload;
    procedure crc32(dst: TRegister; src: TMem); overload;
    procedure pcmpestri(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pcmpestri(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pcmpestrm(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pcmpestrm(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pcmpistri(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pcmpistri(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pcmpistrm(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate); overload;
    procedure pcmpistrm(dst: TXMMRegister; src: TMem; imm8: TImmediate); overload;
    procedure pcmpgtq(dst: TXMMRegister; src: TXMMRegister); overload;
    procedure pcmpgtq(dst: TXMMRegister; src: TMem); overload;
    procedure popcnt(dst: TRegister; src: TRegister); overload;
    procedure popcnt(dst: TRegister; src: TMem); overload;
    procedure amd_prefetch(mem: TMem);
    procedure amd_prefetchw(mem: TMem);
    procedure movbe(dst: TRegister; src: TMem); overload;
    procedure movbe(dst: TMem; src: TRegister); overload;
  end;

  TSerializer = class(TSerializerIntrinsics);


function mk_gpb(index: UInt8): TRegister;
function mk_gpw(index: UInt8): TRegister;
function mk_gpd(index: UInt8): TRegister;
{$IFDEF ASMJIT_X64}
function mk_gpq(index: UInt8): TRegister;
{$ENDIF}
function mk_gpn(index: UInt8): TRegister;
function mk_mm(index: UInt8): TMMRegister;
function mk_xmm(index: UInt8): TXMMRegister;
function st(i: Integer): TX87Register;

function _ptr_build(Lbl: PLabel; disp: SysInt; ptrSize: UInt8): TMem; overload;
function ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function byte_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function word_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function dword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function qword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function tword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function dqword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function mmword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function xmmword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;
function sysint_ptr(Lbl: PLabel; disp: SysInt = 0): TMem; overload;

function _ptr_build(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt; ptrSize: UInt8): TMem; overload;
function ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function byte_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function word_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function dword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function qword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function tword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function dqword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function mmword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function xmmword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;
function sysint_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem; overload;

function _ptr_build_abs(target: Pointer; disp: SysInt; segmentPrefix: UInt32; ptrSize: UInt8): TMem; overload;
function ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function byte_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function word_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function dword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function qword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function tword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function dqword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function mmword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function xmmword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function sysint_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;

function _ptr_build_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt; segmentPrefix: UInt32; ptrSize: UInt8): TMem; overload;
function ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function byte_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function word_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function dword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function qword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function tword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function dqword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function mmword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function xmmword_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;
function sysint_ptr_abs(Target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem; overload;

function _ptr_build(base: TRegister; disp: SysInt; ptrSize: UInt8): TMem; overload;
function ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function byte_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function word_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function dword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function qword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function tword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function dqword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function mmword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function xmmword_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;
function sysint_ptr(base: TRegister; disp: SysInt = 0): TMem; overload;

function _ptr_build(base, index: TRegister; shift: UInt32; disp: SysInt; ptrSize: UInt8): TMem; overload;
function ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function byte_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function word_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function dword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function qword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function tword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function dqword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function mmword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function xmmword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;
function sysint_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem; overload;

function imm(i: SysInt): TImmediate;
function uimm(i: SysUInt): TImmediate;

function mm_shuffle(z, y, x, w: UInt8): UInt8;

var
  reg_none,
  al,
  cl,
  dl,
  bl,
  ah,
  ch,
  dh,
  bh,
{$IFDEF ASMJIT_X64}
  r8b,
  r9b,
  r10b,
  r11b,
  r12b,
  r13b,
  r14b,
  r15b,
{$ENDIF}
  ax,
  cx,
  dx,
  bx,
  sp,
  bp,
  si,
  di,
{$IFDEF ASMJIT_X64}
  r8w,
  r9w,
  r10w,
  r11w,
  r12w,
  r13w,
  r14w,
  r15w,
{$ENDIF}
  eax,
  ecx,
  edx,
  ebx,
  esp,
  ebp,
  esi,
  edi,
{$IFDEF ASMJIT_X64}
  rax,
  rcx,
  rdx,
  rbx,
  rsp,
  rbp,
  rsi,
  rdi,
  r8,
  r9,
  r10,
  r11,
  r12,
  r13,
  r14,
  r15,
{$ENDIF}
  nax,
  ncx,
  ndx,
  nbx,
  nsp,
  nbp,
  nsi,
  ndi: TRegister;
  mm0,
  mm1,
  mm2,
  mm3,
  mm4,
  mm5,
  mm6,
  mm7: TMMRegister;
  xmm0,
  xmm1,
  xmm2,
  xmm3,
  xmm4,
  xmm5,
  xmm6,
  xmm7
{$IFDEF ASMJIT_X64}
  ,xmm8,
  xmm9,
  xmm10,
  xmm11,
  xmm12,
  xmm13,
  xmm14,
  xmm15
{$ENDIF}: TXMMRegister;

const
_jcctable: array[0..15] of UInt32 = (
  INST_JO,
  INST_JNO,
  INST_JB,
  INST_JAE,
  INST_JE,
  INST_JNE,
  INST_JBE,
  INST_JA,
  INST_JS,
  INST_JNS,
  INST_JPE,
  INST_JPO,
  INST_JL,
  INST_JGE,
  INST_JLE,
  INST_JG
);

_cmovcctable: array[0..15] of UInt32 = (
  INST_CMOVO,
  INST_CMOVNO,
  INST_CMOVB,
  INST_CMOVAE,
  INST_CMOVE,
  INST_CMOVNE,
  INST_CMOVBE,
  INST_CMOVA,
  INST_CMOVS,
  INST_CMOVNS,
  INST_CMOVPE,
  INST_CMOVPO,
  INST_CMOVL,
  INST_CMOVGE,
  INST_CMOVLE,
  INST_CMOVG
);

_setcctable: array[0..15] of UInt32 = (
  INST_SETO,
  INST_SETNO,
  INST_SETB,
  INST_SETAE,
  INST_SETE,
  INST_SETNE,
  INST_SETBE,
  INST_SETA,
  INST_SETS,
  INST_SETNS,
  INST_SETPE,
  INST_SETPO,
  INST_SETL,
  INST_SETGE,
  INST_SETLE,
  INST_SETG
);

implementation

uses
  DAsmJit_Logger;

function mk_gpb(index: UInt8): TRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_GPB));
end;

function mk_gpw(index: UInt8): TRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_GPW));
end;

function mk_gpd(index: UInt8): TRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_GPD));
end;

{$IFDEF ASMJIT_X64}
function mk_gpq(index: UInt8): TRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_GPQ));
end;
{$ENDIF}

function mk_gpn(index: UInt8): TRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_GPN));
end;

function mk_mm(index: UInt8): TMMRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_MM));
end;

function  mk_xmm(index: UInt8): TXMMRegister;
begin
  Result.Create(_Init, UInt8(Index or REG_XMM));
end;

function st(i: Integer): TX87Register;
begin
  Assert((i >= 0) and (i < 8));
  Result.Create(_Init, UInt8(i));
end;

function ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, 0);
end;

function byte_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_BYTE);
end;

function word_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_WORD);
end;

function dword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_DWORD);
end;

function qword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_QWORD);
end;

function tword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_TWORD);
end;

function dqword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_DQWORD);
end;

function mmword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_QWORD);
end;

function xmmword_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SIZE_DQWORD);
end;

function sysint_ptr(Lbl: PLabel; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, disp, SizeOf(SysInt));
end;

function ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, 0);
end;

function byte_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_BYTE);
end;

function word_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_WORD);
end;

function dword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_DWORD);
end;

function qword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_QWORD);
end;

function tword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_TWORD);
end;

function dqword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_DQWORD);
end;

function mmword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_QWORD);
end;

function xmmword_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SIZE_DQWORD);
end;

function sysint_ptr(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(Lbl, index, shift, disp, SizeOf(SysInt));
end;

function ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, 0);
end;

function byte_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_BYTE);
end;

function word_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_WORD);
end;

function dword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_DWORD);
end;

function qword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_QWORD);
end;

function tword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_TWORD);
end;

function dqword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_DQWORD);
end;

function mmword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_QWORD);
end;

function xmmword_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SIZE_DQWORD);
end;

function sysint_ptr_abs(target: Pointer; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, disp, segmentPrefix, SizeOf(SysInt));
end;

function ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, 0);
end;

function byte_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_BYTE);
end;

function word_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_WORD);
end;

function dword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_DWORD);
end;

function qword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_QWORD);
end;

function tword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_TWORD);
end;

function dqword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_DQWORD);
end;

function mmword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_QWORD);
end;

function xmmword_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SIZE_DQWORD);
end;

function sysint_ptr_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt = 0; segmentPrefix: UInt32 = SEGMENT_NONE): TMem;
begin
  Result := _ptr_build_abs(target, index, shift, disp, segmentPrefix, SizeOf(SysInt));
end;

function ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, 0);
end;

function byte_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_BYTE);
end;

function word_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_WORD);
end;

function dword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_DWORD);
end;

function qword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_QWORD);
end;

function tword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_TWORD);
end;

function dqword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_DQWORD);
end;

function mmword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_QWORD);
end;

function xmmword_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SIZE_DQWORD);
end;

function sysint_ptr(base: TRegister; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, disp, SizeOf(SysInt));
end;

function ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, 0);
end;

function byte_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_BYTE);
end;

function word_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_WORD);
end;

function dword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_DWORD);
end;

function qword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_QWORD);
end;

function tword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_TWORD);
end;

function dqword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_DQWORD);
end;

function mmword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_QWORD);
end;

function xmmword_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SIZE_DQWORD);
end;

function sysint_ptr(base, index: TRegister; shift: UInt32 = 0; disp: SysInt = 0): TMem;
begin
  Result := _ptr_build(base, index, shift, disp, SizeOf(SysInt));
end;

function _ptr_build(Lbl: PLabel; disp: SysInt; ptrSize: UInt8): TMem;
begin
  Result.Create(Lbl, disp, ptrSize);
end;

function _ptr_build(Lbl: PLabel; index: TRegister; shift: UInt32; disp: SysInt; ptrSize: UInt8): TMem;
begin
  Result.Create(Lbl, disp, ptrSize);
  Result.u._mem.index := index.Code and REGCODE_MASK;
  Result.u._mem.shift := shift;
end;

function _ptr_build_abs(target: Pointer; disp: SysInt; segmentPrefix: UInt32; ptrSize: UInt8): TMem;
begin
  Result.Create;
  Result.u._mem.size := ptrSize;
  Result.u._mem.base := NO_REG;
  Result.u._mem.index := NO_REG;
  Result.u._mem.segmentPrefix := segmentPrefix;
  Result.u._mem.target := target;
  Result.u._mem.displacement := disp;
end;

function _ptr_build_abs(target: Pointer; index: TRegister; shift: UInt32; disp: SysInt; segmentPrefix: UInt32; ptrSize: UInt8): TMem;
begin
  Result.Create;
  Result.u._mem.size := ptrSize;
  Result.u._mem.base := NO_REG;
  Result.u._mem.index := index.Index;
  Result.u._mem.segmentPrefix := SysInt(segmentPrefix);
  Result.u._mem.target := target;
  Result.u._mem.displacement := disp;
end;

function _ptr_build(base: TRegister; disp: SysInt; ptrSize: UInt8): TMem;
begin
  Result.Create(base, disp, ptrSize);
end;

function _ptr_build(base, index: TRegister; shift: UInt32; disp: SysInt; ptrSize: UInt8): TMem;
begin
  Result.Create(base, index, shift, disp, ptrSize);
end;

function imm(i: SysInt): TImmediate;
begin
  Result.Create(i, False);
end;

function uimm(i: SysUInt): TImmediate;
begin
  Result.Create(SysInt(i), True);
end;

function mm_shuffle(z, y, x, w: UInt8): UInt8;
begin
  Result := (z shl 6) or (y shl 4) or (x shl 2) or w;
end;

constructor TOperand.Create;
begin
  inherited;

  FCompilerData := nil;
  FOperandID := 0;
  {$IFDEF ASMJIT_X64}
  Fx64Padding := 0;
  {$ENDIF}
  FillChar(u, SizeOf(TOperandUnion), 0);
end;

constructor TOperand.Create(Other: TOperand);
begin
  //inherited Create;

  FCompilerData := nil;
  FOperandId := 0;
  _init(other);
end;

constructor TOperand.Create(x: _DontInitialize);
begin
  //inherited Create;

  FCompilerData := nil;
  FOperandId := 0;
end;

function TOperand.op: UInt8;
begin
  Result := u._base.op;
end;

function TOperand.isNone: Boolean;
begin
  Result := u._base.op = OP_NONE;
end;

function TOperand.isReg: Boolean;
begin
  Result := u._base.op = OP_REG;
end;

function TOperand.isMem: Boolean;
begin
  Result := u._base.op = OP_MEM;
end;

function TOperand.isImm: Boolean;
begin
  Result := u._base.op = OP_IMM;
end;

function TOperand.isLabel: Boolean;
begin
  Result := u._base.op = OP_LABEL;
end;

function TOperand.isRegType(regType: UInt8): Boolean;
begin
  Result := (isReg) and ((u._reg.code and REGTYPE_MASK) = regType);
end;

function TOperand.isRegCode(regCode: UInt8): Boolean;
begin
  Result := (isReg) and (u._reg.code = regCode);
end;

function TOperand.isRegIndex(regIndex: UInt8): Boolean;
begin
  Result := (isReg) and ((u._reg.code and REGCODE_MASK) = (regIndex and REGCODE_MASK));
end;

function TOperand.isRegMem: Boolean;
begin
  Result := isMem or isReg;
end;

function TOperand.isRegMem(regType: UInt8): Boolean;
begin
  Result := isMem or isRegType(regType);
end;

function TOperand.size: UInt8;
begin
  Result := u._base.size;
end;

procedure TOperand.clearId;
begin
  FOperandId := 0;
end;

procedure TOperand._init(other: TOperand);
begin
  _copy(other);
end;

procedure TOperand._copy(other: TOperand);
begin
  FCompilerData := other.FCompilerData;
  FOperandID := other.FOperandID;
  {$IFDEF ASMJIT_X64}
  Fx64Padding := other.Fx64Padding;
  {$ENDIF}
  Move(other.u, u, SizeOf(TOperandUnion));
end;

constructor TBaseReg.Create(ACode: Byte; ASize: Byte);
begin
  inherited Create(_DontInit);

  u._reg.op := OP_REG;
  u._reg.size := ASize;
  u._reg.code := ACode;
end;

function TBaseReg.typ: UInt8;
begin
  Result := UInt8(u._reg.code and REGTYPE_MASK);
end;

function TBaseReg.code: UInt8;
begin
  Result := UInt8(u._reg.code);
end;

function TBaseReg.index: UInt8;
begin
  Result := UInt8(u._reg.code and REGCODE_MASK);
end;

function TBaseReg.isRegCode(regCode: UInt8): Boolean;
begin
  Result := u._reg.code = regCode;
end;

function TBaseReg.isRegType(regType: UInt8): Boolean;
begin
  Result := (u._reg.code and REGTYPE_MASK) = regType;
end;

function TBaseReg.isRegIndex(regIndex: UInt8): Boolean;
begin
  Result := (u._reg.code and REGCODE_MASK) = (regIndex and REGCODE_MASK);
end;

procedure TBaseReg.setCode(ACode: UInt8);
begin
  u._reg.code := ACode;
end;

procedure TBaseReg.setSize(ASize: UInt8);
begin
  u._reg.size := ASize;
end;

constructor TRegister.Create;
begin
  inherited Create(NO_REG, 0);
end;

constructor TRegister.Create(x: _Initialize; ACode: UInt8);
begin
  inherited Create(ACode, UInt8(SysUInt(1) shl ((ACode and REGTYPE_MASK) shr 4)));
end;

constructor TX87Register.Create;
begin
  inherited Create(NO_REG, 0);
end;

constructor TX87Register.Create(x: _Initialize; ACode: UInt8);
begin
  inherited Create(ACode or REG_X87, 10);
end;

constructor TMMRegister.Create;
begin
  inherited Create(NO_REG, 0);
end;

constructor TMMRegister.Create(x: _Initialize; ACode: UInt8);
begin
  inherited Create(ACode, 8);
end;

constructor TXMMRegister.Create;
begin
  inherited Create(NO_REG, 0);
end;

constructor TXMMRegister.Create(x: _Initialize; ACode: UInt8);
begin
  inherited Create(ACode, 16);
end;

constructor TMem.Create;
begin
  inherited Create(_DontInit);

  FillChar(u._mem, SizeOf(TMemData), 0);
  u._mem.op := OP_MEM;
end;

constructor TMem.Create(ALbl: PLabel; ADisplacement: SysInt; ASize: UInt8 = 0);
begin
  inherited Create(_DontInit);

  u._mem.op := OP_MEM;
  u._mem.size := ASize;
  u._mem.base := NO_REG;
  u._mem.index := NO_REG;
  u._mem.shift := 0;
  u._mem.segmentPrefix := SEGMENT_NONE;
  u._mem.hasLabel := True;
  u._mem.Lbl := ALbl;
  u._mem.displacement := ADisplacement;
end;

constructor TMem.Create(ABase: TRegister; ADisplacement: SysInt; ASize: UInt8 = 0);
begin
  inherited Create(_DontInit);

  u._mem.op := OP_MEM;
  u._mem.size := ASize;
  u._mem.base := ABase.Code and REGCODE_MASK;
  u._mem.index := NO_REG;
  u._mem.shift := 0;
  u._mem.segmentPrefix := SEGMENT_NONE;
  u._mem.hasLabel := False;
  u._mem.target := nil;
  u._mem.displacement := ADisplacement;
end;

constructor TMem.Create(ABase, AIndex: TRegister; AShift: UInt32; ADisplacement: SysInt; ASize: UInt8 = 0);
begin
  inherited Create(_DontInit);
  Assert(shift <= 3);

  u._mem.op := OP_MEM;
  u._mem.size := ASize;
  u._mem.base := ABase.Code and REGCODE_MASK;
  u._mem.index := AIndex.Code and REGCODE_MASK;
  u._mem.shift := UInt8(AShift);
  u._mem.segmentPrefix := SEGMENT_NONE;
  u._mem.hasLabel := False;
  u._mem.target := nil;
  u._mem.displacement := ADisplacement;
end;

function TMem.hasBase: Boolean;
begin
  Result := u._mem.base <> NO_REG;
end;

function TMem.hasIndex: Boolean;
begin
  Result := u._mem.index <> NO_REG;
end;

function TMem.base: UInt8;
begin
  Result := u._mem.base;
end;

function TMem.index: UInt8;
begin
  Result := u._mem.index;
end;

function TMem.shift: UInt8;
begin
  Result := u._mem.shift;
end;

function TMem.segmentPrefix: UInt8;
begin
  Result := u._mem.segmentPrefix;
end;

function TMem.hasLabel: Boolean;
begin
  Result := u._mem.hasLabel;
end;

function TMem.hasTarget: Boolean;
begin
  Result := not u._mem.hasLabel;
end;

function TMem.Lbl: TLabel;
begin
  Result := u._mem.Lbl^;
end;

function TMem.target: Pointer;
begin
  Result := u._mem.target;
end;

function TMem.displacement: SysInt;
begin
  Result := u._mem.displacement;
end;

procedure TMem.setDisplacement(ADisplacement: SysInt);
begin
  u._mem.displacement := ADisplacement;
end;

constructor TImmediate.Create;
begin
  inherited Create(_DontInit);

  FillChar(u._imm, SizeOf(TImmData), 0);
  u._imm.op := OP_IMM;
end;

constructor TImmediate.Create(i: SysInt);
begin
  inherited Create(_DontInit);

  FillChar(u._imm, SizeOf(TImmData), 0);
  u._imm.op := OP_IMM;
  u._imm.value := i;
end;

constructor TImmediate.Create(i: SysInt; AIsUnsigned: Boolean);
begin
  inherited Create(_DontInit);

  FillChar(u._imm, SizeOf(TImmData), 0);
  u._imm.op := OP_IMM;
  u._imm.isUnsigned := AIsUnsigned;
  u._imm.value := i;
end;

function TImmediate.isUnsigned: Boolean;
begin
  Result := u._imm.isUnsigned;
end;

function TImmediate.relocMode: UInt8;
begin
  Result := u._imm.relocMode;
end;

function TImmediate.value: SysInt;
begin
  Result := u._imm.value;
end;

function TImmediate.uvalue: SysUInt;
begin
  Result := SysUInt(u._imm.value);
end;

procedure TImmediate.setValue(val: SysInt; AIsUnsigned: Boolean = False);
begin
  u._imm.value := val;
  u._imm.isUnsigned := AIsUnsigned;
end;

procedure TImmediate.setUValue(val: SysUInt);
begin
  u._imm.value := SysInt(val);
  u._imm.isUnsigned := True;
end;

constructor TLabel.Create(id: UInt32 = 0);
begin
  inherited Create;

  u._lbl.op := OP_LABEL;
  u._lbl.size := 4;
  u._lbl.state := LABEL_STATE_UNUSED;
  u._lbl.id := id;
  u._lbl.position := -1;
  u._lbl.link := nil;
end;

destructor TLabel.Destroy;
begin
  Assert(not isLinked);

  inherited;
end;

procedure TLabel.unuse;
begin
  FillChar(u._lbl, SizeOf(TLblData), 0);
  u._lbl.op := OP_LABEL;
  u._lbl.position := -1;
end;

function TLabel.state: UInt8;
begin
  Result := u._lbl.state;
end;

function TLabel.labelId: UInt32;
begin
  Result := u._lbl.id;
end;

function TLabel.isUnused: Boolean;
begin
  Result := u._lbl.state = LABEL_STATE_UNUSED;
end;

function TLabel.isLinked: Boolean;
begin
  Result := u._lbl.state = LABEL_STATE_LINKED;
end;

function TLabel.isBound: Boolean;
begin
  Result := u._lbl.state = LABEL_STATE_BOUND;
end;

function TLabel.position: SysInt;
begin
  Result := u._lbl.position;
end;

procedure TLabel.setId(id: UInt32);
begin
  u._lbl.id := id;
end;

procedure TLabel.setStatePos(AState: UInt8; APosition: SysInt);
begin
  u._lbl.state := AState;
  u._lbl.position := APosition;
end;

constructor TSerializerCore.Create;
begin
  inherited Create;

  FLogger := nil;
  //FZone := TZone.Create(65536 - SizeOf(TChunk) - 32);
  FMemZone := TPodVector.Create;
  FProperties := (1 shl PROPERTY_OPTIMIZE_ALIGN) or (1 shl PROPERTY_X86_JCC_HINTS);
  FError := 0;
end;

destructor TSerializerCore.Destroy;
var
  i: SysUInt;
begin
  if (FMemZone.Length > 0) then
    for i := 0 to FMemZone.Length - 1 do
      Dispose(FMemZone[i]);
  FMemZone.Free;
  //FZone.Free;

  inherited Destroy;
end;

function TSerializerCore.getProperty(key: UInt32): UInt32;
begin
  if (key < 32) then
    Result := (FProperties shr key) and 1
  else
    Result := $FFFFFFFF;
end;

function TSerializerCore.setProperty(key, value: UInt32): UInt32;
var
  mask: UInt32;
begin
  if (key < 32) then
  begin
    mask := (1 shl key);
    Result := (FProperties and mask) shr key;

    if (value > 0) then
      FProperties := FProperties or mask
    else
      FProperties := FProperties and (not mask);
  end
  else
    Result := $FFFFFFFF;
end;

procedure TSerializerCore.emitX86(code: UInt32);
begin
  _emitX86(code, nil, nil, nil);
end;

procedure TSerializerCore.emitX86(code: UInt32; o1: TOperand);
begin
  _emitX86(code, @o1, nil, nil);
end;

procedure TSerializerCore.emitX86(code: UInt32; o1, o2: TOperand);
begin
  _emitX86(code, @o1, @o2, nil);
end;

procedure TSerializerCore.emitX86(code: UInt32; o1, o2, o3: TOperand);
begin
  _emitX86(code, @o1, @o2, @o3);
end;

procedure TSerializerCore._emitJcc(code: UInt32; Lbl: TLabel; hint: UInt32);
var
  imm: TImmediate;
begin
  if (hint = 0) then
    emitX86(code, Lbl)
  else
  begin
    imm.Create(hint);
    emitX86(code, Lbl, imm);
  end;
end;

(*function TSerializerCore._zoneAlloc(Size: SysUInt): Pointer;
begin
  Result := FZone.alloc(size);
end; *)

procedure TSerializerCore.setError(Error: UInt32);
begin
  FError := Error;

  if ((FLogger <> nil) and FLogger.enabled) then
    FLogger.logFormat('; FATAL ERROR: %s (%u).' + LineEnding, [errorCodeToString(error), SysUInt(error)]);
end;

procedure TSerializerCore.clearError;
begin
  FError := ERROR_NONE;
end;

function TSerializerCore.conditionToJCC(cc: SysInt): UInt32;
begin
  Assert(cc <= $F);
  Result := _jcctable[cc];
end;

function TSerializerCore.conditionToCMovCC(cc: SysInt): UInt32;
begin
  Assert(cc <= $F);
  Result := _cmovcctable[cc];
end;

function TSerializerCore.conditionToSetCC(cc: SysInt): UInt32;
begin
  Assert(cc <= $F);
  Result := _setcctable[cc];
end;

procedure TSerializerIntrinsics.db(x: UInt8);
begin
  _embed(@x, 1);
end;

procedure TSerializerIntrinsics.dw(x: UInt16);
begin
  _embed(@x, 2);
end;

procedure TSerializerIntrinsics.dd(x: UInt32);
begin
  _embed(@x, 4);
end;

procedure TSerializerIntrinsics.dq(x: UInt64);
begin
  _embed(@x, 8);
end;

procedure TSerializerIntrinsics.dint8(x: Int8);
begin
  _embed(@x, SizeOf(Int8));
end;

procedure TSerializerIntrinsics.duint8(x: UInt8);
begin
  _embed(@x, SizeOf(UInt8));
end;

procedure TSerializerIntrinsics.dint16(x: Int16);
begin
  _embed(@x, SizeOf(Int16));
end;

procedure TSerializerIntrinsics.duint16(x: UInt16);
begin
  _embed(@x, SizeOf(UInt16));
end;

procedure TSerializerIntrinsics.dint32(x: Int32);
begin
  _embed(@x, SizeOf(Int32));
end;
procedure TSerializerIntrinsics.duint32(x: UInt32);
begin
  _embed(@x, SizeOf(UInt32));
end;

procedure TSerializerIntrinsics.dint64(x: Int64);
begin
  _embed(@x, SizeOf(Int64));
end;

procedure TSerializerIntrinsics.duint64(x: UInt64);
begin
  _embed(@x, SizeOf(UInt64));
end;

procedure TSerializerIntrinsics.dsysint(x: SysInt);
begin
  _embed(@x, SizeOf(SysInt));
end;

procedure TSerializerIntrinsics.dsysuint(x: SysUInt);
begin
  _embed(@x, SizeOf(SysUInt));
end;

procedure TSerializerIntrinsics.dfloat(x: Single);
begin
  _embed(@x, SizeOf(Single));
end;

procedure TSerializerIntrinsics.ddouble(x: Double);
begin
  _embed(@x, SizeOf(Double));
end;

procedure TSerializerIntrinsics.dptr(x: Pointer);
begin
  _embed(@x, SizeOf(Pointer));
end;

procedure TSerializerIntrinsics.dmm(x: TMMData);
begin
  _embed(@x, SizeOf(TMMData));
end;

procedure TSerializerIntrinsics.dxmm(x: TXMMData);
begin
  _embed(@x, SizeOf(TXMMData));
end;

procedure TSerializerIntrinsics.data(data: Pointer; size: SysUInt);
begin
  _embed(data, size);
end;

//template<typename T>
//procedure TSerializerIntrinsics.dstruct(const T& x);
//begin
//  _embed(x, SizeOf(T));
//end;

procedure TSerializerIntrinsics.adc(dst: TRegister; src: TRegister);
begin
  emitX86(INST_ADC, dst, src);
end;

procedure TSerializerIntrinsics.adc(dst: TRegister; src: TMem);
begin
  emitX86(INST_ADC, dst, src);
end;

procedure TSerializerIntrinsics.adc(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_ADC, dst, src);
end;

procedure TSerializerIntrinsics.adc(dst: TMem; src: TRegister);
begin
  emitX86(INST_ADC, dst, src);
end;

procedure TSerializerIntrinsics.adc(dst: TMem; src: TImmediate);
begin
  emitX86(INST_ADC, dst, src);
end;

procedure TSerializerIntrinsics.add(dst: TRegister; src: TRegister);
begin
  emitX86(INST_ADD, dst, src);
end;

procedure TSerializerIntrinsics.add(dst: TRegister; src: TMem);
begin
  emitX86(INST_ADD, dst, src);
end;

procedure TSerializerIntrinsics.add(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_ADD, dst, src);
end;

procedure TSerializerIntrinsics.add(dst: TMem; src: TRegister);
begin
  emitX86(INST_ADD, dst, src);
end;

procedure TSerializerIntrinsics.add(dst: TMem; src: TImmediate);
begin
  emitX86(INST_ADD, dst, src);
end;

procedure TSerializerIntrinsics.and_(dst: TRegister; src: TRegister);
begin
  emitX86(INST_AND, dst, src);
end;

procedure TSerializerIntrinsics.and_(dst: TRegister; src: TMem);
begin
  emitX86(INST_AND, dst, src);
end;

procedure TSerializerIntrinsics.and_(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_AND, dst, src);
end;

procedure TSerializerIntrinsics.and_(dst: TMem; src: TRegister);
begin
  emitX86(INST_AND, dst, src);
end;

procedure TSerializerIntrinsics.and_(dst: TMem; src: TImmediate);
begin
  emitX86(INST_AND, dst, src);
end;

procedure TSerializerIntrinsics.bsf(dst: TRegister; src: TRegister);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_BSF, dst, src);
end;

procedure TSerializerIntrinsics.bsf(dst: TRegister; src: TMem);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_BSF, dst, src);
end;

procedure TSerializerIntrinsics.bsr(dst: TRegister; src: TRegister);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_BSR, dst, src);
end;

procedure TSerializerIntrinsics.bsr(dst: TRegister; src: TMem);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_BSR, dst, src);
end;

procedure TSerializerIntrinsics.bswap(dst: TRegister);
begin
  Assert((dst.typ = REG_GPD) or (dst.typ = REG_GPQ));
  emitX86(INST_BSWAP, dst);
end;

procedure TSerializerIntrinsics.bt(dst: TRegister; src: TRegister);
begin
  emitX86(INST_BT, dst, src);
end;

procedure TSerializerIntrinsics.bt(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_BT, dst, src);
end;

procedure TSerializerIntrinsics.bt(dst: TMem; src: TRegister);
begin
  emitX86(INST_BT, dst, src);
end;

procedure TSerializerIntrinsics.bt(dst: TMem; src: TImmediate);
begin
  emitX86(INST_BT, dst, src);
end;

procedure TSerializerIntrinsics.btc(dst: TRegister; src: TRegister);
begin
  emitX86(INST_BTC, dst, src);
end;

procedure TSerializerIntrinsics.btc(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_BTC, dst, src);
end;

procedure TSerializerIntrinsics.btc(dst: TMem; src: TRegister);
begin
  emitX86(INST_BTC, dst, src);
end;

procedure TSerializerIntrinsics.btc(dst: TMem; src: TImmediate);
begin
  emitX86(INST_BTC, dst, src);
end;

procedure TSerializerIntrinsics.btr(dst: TRegister; src: TRegister);
begin
  emitX86(INST_BTR, dst, src);
end;

procedure TSerializerIntrinsics.btr(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_BTR, dst, src);
end;

procedure TSerializerIntrinsics.btr(dst: TMem; src: TRegister);
begin
  emitX86(INST_BTR, dst, src);
end;

procedure TSerializerIntrinsics.btr(dst: TMem; src: TImmediate);
begin
  emitX86(INST_BTR, dst, src);
end;

procedure TSerializerIntrinsics.bts(dst: TRegister; src: TRegister);
begin
  emitX86(INST_BTS, dst, src);
end;

procedure TSerializerIntrinsics.bts(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_BTS, dst, src);
end;

procedure TSerializerIntrinsics.bts(dst: TMem; src: TRegister);
begin
  emitX86(INST_BTS, dst, src);
end;

procedure TSerializerIntrinsics.bts(dst: TMem; src: TImmediate);
begin
  emitX86(INST_BTS, dst, src);
end;

procedure TSerializerIntrinsics.call(dst: TRegister);
begin
  Assert(not dst.isRegType(REG_GPN));
  emitX86(INST_CALL, dst);
end;

procedure TSerializerIntrinsics.call(dst: TMem);
begin
  emitX86(INST_CALL, dst);
end;

procedure TSerializerIntrinsics.call(dst: TImmediate);
begin
  emitX86(INST_CALL, dst);
end;

procedure TSerializerIntrinsics.call(dst: Pointer);
var
  imm: TImmediate;
begin
  imm.Create(SysInt(dst));
  emitX86(INST_CALL, imm);
end;

procedure TSerializerIntrinsics.call(Lbl: TLabel);
begin
  emitX86(INST_CALL, Lbl);
end;

procedure TSerializerIntrinsics.cbw;
begin
  emitX86(INST_CBW);
end;

procedure TSerializerIntrinsics.cwde;
begin
  emitX86(INST_CWDE);
end;

{$IFDEF ASMJIT_X64}
procedure TSerializerIntrinsics.cdqe;
begin
  emitX86(INST_CDQE);
end;
{$ENDIF}

procedure TSerializerIntrinsics.clc;
begin
  emitX86(INST_CLC);
end;

procedure TSerializerIntrinsics.cld;
begin
  emitX86(INST_CLD);
end;

procedure TSerializerIntrinsics.cmc;
begin
  emitX86(INST_CMC);
end;

procedure TSerializerIntrinsics.cmov(cc: SysInt; dst: TRegister; src: TRegister);
begin
  emitX86(conditionToCMovCC(cc), dst, src);
end;

procedure TSerializerIntrinsics.cmov(cc: SysInt; dst: TRegister; src: TMem);
begin
  emitX86(conditionToCMovCC(cc), dst, src);
end;

procedure TSerializerIntrinsics.cmova  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVA  , dst, src);
end;

procedure TSerializerIntrinsics.cmova  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVA  , dst, src);
end;

procedure TSerializerIntrinsics.cmovae (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVAE , dst, src);
end;

procedure TSerializerIntrinsics.cmovae (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVAE , dst, src);
end;

procedure TSerializerIntrinsics.cmovb  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVB  , dst, src);
end;

procedure TSerializerIntrinsics.cmovb  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVB  , dst, src);
end;

procedure TSerializerIntrinsics.cmovbe (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVBE , dst, src);
end;

procedure TSerializerIntrinsics.cmovbe (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVBE , dst, src);
end;

procedure TSerializerIntrinsics.cmovc  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVC  , dst, src);
end;

procedure TSerializerIntrinsics.cmovc  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVC  , dst, src);
end;

procedure TSerializerIntrinsics.cmove  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVE  , dst, src);
end;

procedure TSerializerIntrinsics.cmove  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVE  , dst, src);
end;

procedure TSerializerIntrinsics.cmovg  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVG  , dst, src);
end;

procedure TSerializerIntrinsics.cmovg  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVG  , dst, src);
end;

procedure TSerializerIntrinsics.cmovge (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVGE , dst, src);
end;

procedure TSerializerIntrinsics.cmovge (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVGE , dst, src);
end;

procedure TSerializerIntrinsics.cmovl  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVL  , dst, src);
end;

procedure TSerializerIntrinsics.cmovl  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVL  , dst, src);
end;

procedure TSerializerIntrinsics.cmovle (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVLE , dst, src);
end;

procedure TSerializerIntrinsics.cmovle (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVLE , dst, src);
end;

procedure TSerializerIntrinsics.cmovna (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNA , dst, src);
end;

procedure TSerializerIntrinsics.cmovna (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNA , dst, src);
end;

procedure TSerializerIntrinsics.cmovnae(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNAE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnae(dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNAE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnb (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNB , dst, src);
end;

procedure TSerializerIntrinsics.cmovnb (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNB , dst, src);
end;

procedure TSerializerIntrinsics.cmovnbe(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNBE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnbe(dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNBE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnc (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNC , dst, src);
end;

procedure TSerializerIntrinsics.cmovnc (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNC , dst, src);
end;

procedure TSerializerIntrinsics.cmovne (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNE , dst, src);
end;

procedure TSerializerIntrinsics.cmovne (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNE , dst, src);
end;

procedure TSerializerIntrinsics.cmovng (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNG , dst, src);
end;

procedure TSerializerIntrinsics.cmovng (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNG , dst, src);
end;

procedure TSerializerIntrinsics.cmovnge(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNGE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnge(dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNGE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnl (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNL , dst, src);
end;

procedure TSerializerIntrinsics.cmovnl (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNL , dst, src);
end;

procedure TSerializerIntrinsics.cmovnle(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNLE, dst, src);
end;

procedure TSerializerIntrinsics.cmovnle(dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNLE, dst, src);
end;

procedure TSerializerIntrinsics.cmovno (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNO , dst, src);
end;

procedure TSerializerIntrinsics.cmovno (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNO , dst, src);
end;

procedure TSerializerIntrinsics.cmovnp (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNP , dst, src);
end;

procedure TSerializerIntrinsics.cmovnp (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNP , dst, src);
end;

procedure TSerializerIntrinsics.cmovns (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNS , dst, src);
end;

procedure TSerializerIntrinsics.cmovns (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNS , dst, src);
end;

procedure TSerializerIntrinsics.cmovnz (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVNZ , dst, src);
end;

procedure TSerializerIntrinsics.cmovnz (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVNZ , dst, src);
end;

procedure TSerializerIntrinsics.cmovo  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVO  , dst, src);
end;

procedure TSerializerIntrinsics.cmovo  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVO  , dst, src);
end;

procedure TSerializerIntrinsics.cmovp  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVP  , dst, src);
end;

procedure TSerializerIntrinsics.cmovp  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVP  , dst, src);
end;

procedure TSerializerIntrinsics.cmovpe (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVPE , dst, src);
end;

procedure TSerializerIntrinsics.cmovpe (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVPE , dst, src);
end;

procedure TSerializerIntrinsics.cmovpo (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVPO , dst, src);
end;

procedure TSerializerIntrinsics.cmovpo (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVPO , dst, src);
end;

procedure TSerializerIntrinsics.cmovs  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVS  , dst, src);
end;

procedure TSerializerIntrinsics.cmovs  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVS  , dst, src);
end;

procedure TSerializerIntrinsics.cmovz  (dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMOVZ  , dst, src);
end;

procedure TSerializerIntrinsics.cmovz  (dst: TRegister; src: TMem);
begin
  emitX86(INST_CMOVZ  , dst, src);
end;

procedure TSerializerIntrinsics.cmp(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMP, dst, src);
end;

procedure TSerializerIntrinsics.cmp(dst: TRegister; src: TMem);
begin
  emitX86(INST_CMP, dst, src);
end;

procedure TSerializerIntrinsics.cmp(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_CMP, dst, src);
end;

procedure TSerializerIntrinsics.cmp(dst: TMem; src: TRegister);
begin
  emitX86(INST_CMP, dst, src);
end;

procedure TSerializerIntrinsics.cmp(dst: TMem; src: TImmediate);
begin
  emitX86(INST_CMP, dst, src);
end;

procedure TSerializerIntrinsics.cmpxchg(dst: TRegister; src: TRegister);
begin
  emitX86(INST_CMPXCHG, dst, src);
end;

procedure TSerializerIntrinsics.cmpxchg(dst: TMem; src: TRegister);
begin
  emitX86(INST_CMPXCHG, dst, src);
end;

procedure TSerializerIntrinsics.cmpxchg8b(dst: TMem);
begin
  emitX86(INST_CMPXCHG8B, dst);
end;

{$IFDEF ASMJIT_X64}

procedure TSerializerIntrinsics.cmpxchg16b(dst: TMem);
begin
  emitX86(INST_CMPXCHG16B, dst);
end;
{$ENDIF}

procedure TSerializerIntrinsics.cpuid;
begin
  emitX86(INST_CPUID);
end;

{$IFDEF ASMJIT_X86}

procedure TSerializerIntrinsics.daa;
begin
  emitX86(INST_DAA);
end;
{$ENDIF}

{$IFDEF ASMJIT_X86}

procedure TSerializerIntrinsics.das;
begin
  emitX86(INST_DAS);
end;
{$ENDIF}

procedure TSerializerIntrinsics.dec_(dst: TRegister);
begin
  emitX86(INST_DEC, dst);
end;

procedure TSerializerIntrinsics.dec_(dst: TMem);
begin
  emitX86(INST_DEC, dst);
end;

procedure TSerializerIntrinsics.div_(src: TRegister);
begin
  emitX86(INST_DIV, src);
end;

procedure TSerializerIntrinsics.div_(src: TMem);
begin
  emitX86(INST_DIV, src);
end;

procedure TSerializerIntrinsics.enter(imm16: TImmediate; imm8: TImmediate);
begin
  emitX86(INST_ENTER, imm16, imm8);
end;

procedure TSerializerIntrinsics.idiv(src: TRegister);
begin
  emitX86(INST_IDIV, src);
end;

procedure TSerializerIntrinsics.idiv(src: TMem);
begin
  emitX86(INST_IDIV, src);
end;

procedure TSerializerIntrinsics.imul(src: TRegister);
begin
  emitX86(INST_IMUL, src);
end;

procedure TSerializerIntrinsics.imul(src: TMem);
begin
  emitX86(INST_IMUL, src);
end;

procedure TSerializerIntrinsics.imul(dst: TRegister; src: TRegister);
begin
  emitX86(INST_IMUL, dst, src);
end;

procedure TSerializerIntrinsics.imul(dst: TRegister; src: TMem);
begin
  emitX86(INST_IMUL, dst, src);
end;

procedure TSerializerIntrinsics.imul(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_IMUL, dst, src);
end;

procedure TSerializerIntrinsics.imul(dst: TRegister; src: TRegister; imm: TImmediate);
begin
  emitX86(INST_IMUL, dst, src, imm);
end;

procedure TSerializerIntrinsics.imul(dst: TRegister; src: TMem; imm: TImmediate);
begin
  emitX86(INST_IMUL, dst, src, imm);
end;

procedure TSerializerIntrinsics.inc_(dst: TRegister);
begin
  emitX86(INST_INC, dst);
end;

procedure TSerializerIntrinsics.inc_(dst: TMem);
begin
  emitX86(INST_INC, dst);
end;

procedure TSerializerIntrinsics.int3;
begin
  emitX86(INST_INT3);
end;

procedure TSerializerIntrinsics.j(cc: SysInt; Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(conditionToJCC(cc), Lbl, hint);
end;

procedure TSerializerIntrinsics.ja  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JA  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jae (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JAE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jb  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JB  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jbe (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JBE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jc  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JC  , Lbl, hint);
end;

procedure TSerializerIntrinsics.je  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JE  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jg  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JG  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jge (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JGE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jl  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JL  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jle (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JLE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jna (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNA , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnae(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNAE, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnb (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNB , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnbe(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNBE, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnc (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNC , Lbl, hint);
end;

procedure TSerializerIntrinsics.jne (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jng (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNG , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnge(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNGE, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnl (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNL , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnle(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNLE, Lbl, hint);
end;

procedure TSerializerIntrinsics.jno (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNO , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnp (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNP , Lbl, hint);
end;

procedure TSerializerIntrinsics.jns (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNS , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnz (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNZ , Lbl, hint);
end;

procedure TSerializerIntrinsics.jo  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JO  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jp  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JP  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jpe (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JPE , Lbl, hint);
end;

procedure TSerializerIntrinsics.jpo (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JPO , Lbl, hint);
end;

procedure TSerializerIntrinsics.js  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JS  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jz  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JZ  , Lbl, hint);
end;

procedure TSerializerIntrinsics.j_short(cc: SysInt; Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin

_emitJcc(conditionToJCC(cc) + (INST_J_SHORT - INST_J), Lbl, hint);
end;

procedure TSerializerIntrinsics.ja_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JA_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jae_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JAE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jb_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JB_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jbe_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JBE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jc_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JC_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.je_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JE_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jg_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JG_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jge_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JGE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jl_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JL_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jle_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JLE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jna_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNA_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnae_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNAE_SHORT, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnb_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNB_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnbe_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNBE_SHORT, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnc_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNC_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jne_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jng_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNG_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnge_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNGE_SHORT, Lbl, hint);
end;

procedure TSerializerIntrinsics.jnl_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNL_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnle_short(Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNLE_SHORT, Lbl, hint);
end;

procedure TSerializerIntrinsics.jno_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNO_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnp_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNP_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jns_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNS_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jnz_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JNZ_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jo_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JO_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jp_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JP_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jpe_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JPE_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.jpo_short (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JPO_SHORT , Lbl, hint);
end;

procedure TSerializerIntrinsics.js_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JS_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jz_short  (Lbl: TLabel; hint: UInt32 = HINT_NONE);
begin
  _emitJcc(INST_JZ_SHORT  , Lbl, hint);
end;

procedure TSerializerIntrinsics.jmp(dst: TRegister);
begin
  emitX86(INST_JMP, dst);
end;

procedure TSerializerIntrinsics.jmp(dst: TMem);
begin
  emitX86(INST_JMP, dst);
end;

procedure TSerializerIntrinsics.jmp(dst: TImmediate);
begin
  emitX86(INST_JMP, dst);
end;

procedure TSerializerIntrinsics.jmp(dst: Pointer);
var
  imm: TImmediate;
begin
  imm.Create(SysInt(dst));
  emitX86(INST_JMP, imm);
end;

procedure TSerializerIntrinsics.jmp(Lbl: TLabel);
begin
  emitX86(INST_JMP, Lbl);
end;

procedure TSerializerIntrinsics.jmp_short(Lbl: TLabel);
begin
  emitX86(INST_JMP_SHORT, Lbl);
end;

procedure TSerializerIntrinsics.lea(dst: TRegister; src: TMem);
begin
  emitX86(INST_LEA, dst, src);
end;

procedure TSerializerIntrinsics.leave;
begin
  emitX86(INST_LEAVE);
end;

procedure TSerializerIntrinsics.DAsmJit_Lock;
begin
  emitX86(INST_LOCK);
end;

procedure TSerializerIntrinsics.mov(dst: TRegister; src: TRegister);
begin
  emitX86(INST_MOV, dst, src);
end;

procedure TSerializerIntrinsics.mov(dst: TRegister; src: TMem);
begin
  emitX86(INST_MOV, dst, src);
end;

procedure TSerializerIntrinsics.mov(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_MOV, dst, src);
end;

procedure TSerializerIntrinsics.mov(dst: TMem; src: TRegister);
begin
  emitX86(INST_MOV, dst, src);
end;

procedure TSerializerIntrinsics.mov(dst: TMem; src: TImmediate);
begin
  emitX86(INST_MOV, dst, src);
end;

procedure TSerializerIntrinsics.mov_ptr(dst: TRegister; src: Pointer);
var
  imm: TImmediate;
begin
  Assert(dst.index = 0);
  imm.Create(SysInt(src));
  emitX86(INST_MOV_PTR, dst, imm);
end;

procedure TSerializerIntrinsics.mov_ptr(dst: Pointer; src: TRegister);
var
  imm: TImmediate;
begin
  Assert(src.index = 0);
  imm.Create(SysInt(dst));
  emitX86(INST_MOV_PTR, imm, src);
end;

procedure TSerializerIntrinsics.movsx(dst: TRegister; src: TRegister);
begin
  emitX86(INST_MOVSX, dst, src);
end;

procedure TSerializerIntrinsics.movsx(dst: TRegister; src: TMem);
begin
  emitX86(INST_MOVSX, dst, src);
end;

{$IFDEF ASMJIT_X64}
procedure TSerializerIntrinsics.movsxd(dst: TRegister; src: TRegister);
begin
  emitX86(INST_MOVSXD, dst, src);
end;

procedure TSerializerIntrinsics.movsxd(dst: TRegister; src: TMem);
begin
  emitX86(INST_MOVSXD, dst, src);
end;
{$ENDIF}

procedure TSerializerIntrinsics.movzx(dst: TRegister; src: TRegister);
begin
  emitX86(INST_MOVZX, dst, src);
end;

procedure TSerializerIntrinsics.movzx(dst: TRegister; src: TMem);
begin
  emitX86(INST_MOVZX, dst, src);
end;

procedure TSerializerIntrinsics.mul(src: TRegister);
begin
  emitX86(INST_MUL, src);
end;

procedure TSerializerIntrinsics.mul(src: TMem);
begin
  emitX86(INST_MUL, src);
end;

procedure TSerializerIntrinsics.neg(dst: TRegister);
begin
  emitX86(INST_NEG, dst);
end;

procedure TSerializerIntrinsics.neg(dst: TMem);
begin
  emitX86(INST_NEG, dst);
end;

procedure TSerializerIntrinsics.nop;
begin
  emitX86(INST_NOP);
end;

procedure TSerializerIntrinsics.not_(dst: TRegister);
begin
  emitX86(INST_NOT, dst);
end;

procedure TSerializerIntrinsics.not_(dst: TMem);
begin
  emitX86(INST_NOT, dst);
end;

procedure TSerializerIntrinsics.or_(dst: TRegister; src: TRegister);
begin
  emitX86(INST_OR, dst, src);
end;

procedure TSerializerIntrinsics.or_(dst: TRegister; src: TMem);
begin
  emitX86(INST_OR, dst, src);
end;

procedure TSerializerIntrinsics.or_(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_OR, dst, src);
end;

procedure TSerializerIntrinsics.or_(dst: TMem; src: TRegister);
begin
  emitX86(INST_OR, dst, src);
end;

procedure TSerializerIntrinsics.or_(dst: TMem; src: TImmediate);
begin
  emitX86(INST_OR, dst, src);
end;

procedure TSerializerIntrinsics.pop(dst: TRegister);
begin
  Assert(dst.isRegType(REG_GPW) or dst.isRegType(REG_GPN));
  emitX86(INST_POP, dst);
end;

procedure TSerializerIntrinsics.pop(dst: TMem);
begin
  Assert((dst.size = 2) or (dst.size = SizeOf(SysInt)));
  emitX86(INST_POP, dst);
end;

{$IFDEF ASMJIT_X86}

procedure TSerializerIntrinsics.popad;
begin
  emitX86(INST_POPAD);
end;
{$ENDIF}

procedure TSerializerIntrinsics.popf;
begin
{$IFDEF ASMJIT_X86}
popfd;
{$ELSE}
popfq;
{$ENDIF}
end;

{$IFDEF ASMJIT_X86}
procedure TSerializerIntrinsics.popfd;
begin
  emitX86(INST_POPFD);
end;
{$ELSE}

procedure TSerializerIntrinsics.popfq;
begin
  emitX86(INST_POPFQ);
end;
{$ENDIF}

procedure TSerializerIntrinsics.push(src: TRegister);
begin
  Assert(src.isRegType(REG_GPW) or src.isRegType(REG_GPN));
  emitX86(INST_PUSH, src);
end;

procedure TSerializerIntrinsics.push(src: TMem);
begin
  Assert((src.size = 2) or (src.size = SizeOf(SysInt)));
  emitX86(INST_PUSH, src);
end;

procedure TSerializerIntrinsics.push(src: TImmediate);
begin
  emitX86(INST_PUSH, src);
end;

{$IFDEF ASMJIT_X86}

procedure TSerializerIntrinsics.pushad;
begin
  emitX86(INST_PUSHAD);
end;
{$ENDIF}

procedure TSerializerIntrinsics.pushf;
begin
{$IFDEF ASMJIT_X86}
pushfd;
{$ELSE}
pushfq;
{$ENDIF}
end;

{$IFDEF ASMJIT_X86}
procedure TSerializerIntrinsics.pushfd;
begin
  emitX86(INST_PUSHFD);
end;
{$ELSE}

procedure TSerializerIntrinsics.pushfq;
begin
  emitX86(INST_PUSHFQ);
end;
{$ENDIF}

procedure TSerializerIntrinsics.rcl(dst: TRegister; src: TRegister);
begin
  emitX86(INST_RCL, dst, src);
end;

procedure TSerializerIntrinsics.rcl(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_RCL, dst, src);
end;

procedure TSerializerIntrinsics.rcl(dst: TMem; src: TRegister);
begin
  emitX86(INST_RCL, dst, src);
end;

procedure TSerializerIntrinsics.rcl(dst: TMem; src: TImmediate);
begin
  emitX86(INST_RCL, dst, src);
end;

procedure TSerializerIntrinsics.rcr(dst: TRegister; src: TRegister);
begin
  emitX86(INST_RCR, dst, src);
end;

procedure TSerializerIntrinsics.rcr(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_RCR, dst, src);
end;

procedure TSerializerIntrinsics.rcr(dst: TMem; src: TRegister);
begin
  emitX86(INST_RCR, dst, src);
end;

procedure TSerializerIntrinsics.rcr(dst: TMem; src: TImmediate);
begin
  emitX86(INST_RCR, dst, src);
end;

procedure TSerializerIntrinsics.rdtsc;
begin
  emitX86(INST_RDTSC);
end;

procedure TSerializerIntrinsics.rdtscp;
begin
  emitX86(INST_RDTSCP);
end;

procedure TSerializerIntrinsics.ret;
begin
  emitX86(INST_RET);
end;

procedure TSerializerIntrinsics.ret(imm16: TImmediate);
begin
  emitX86(INST_RET, imm16);
end;

procedure TSerializerIntrinsics.rol(dst: TRegister; src: TRegister);
begin
  emitX86(INST_ROL, dst, src);
end;

procedure TSerializerIntrinsics.rol(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_ROL, dst, src);
end;

procedure TSerializerIntrinsics.rol(dst: TMem; src: TRegister);
begin
  emitX86(INST_ROL, dst, src);
end;

procedure TSerializerIntrinsics.rol(dst: TMem; src: TImmediate);
begin
  emitX86(INST_ROL, dst, src);
end;

procedure TSerializerIntrinsics.ror(dst: TRegister; src: TRegister);
begin
  emitX86(INST_ROR, dst, src);
end;

procedure TSerializerIntrinsics.ror(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_ROR, dst, src);
end;

procedure TSerializerIntrinsics.ror(dst: TMem; src: TRegister);
begin
  emitX86(INST_ROR, dst, src);
end;

procedure TSerializerIntrinsics.ror(dst: TMem; src: TImmediate);
begin
  emitX86(INST_ROR, dst, src);
end;

{$IFDEF ASMJIT_X86}

procedure TSerializerIntrinsics.sahf;
begin
  emitX86(INST_SAHF);
end;
{$ENDIF}

procedure TSerializerIntrinsics.sbb(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SBB, dst, src);
end;

procedure TSerializerIntrinsics.sbb(dst: TRegister; src: TMem);
begin
  emitX86(INST_SBB, dst, src);
end;

procedure TSerializerIntrinsics.sbb(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SBB, dst, src);
end;

procedure TSerializerIntrinsics.sbb(dst: TMem; src: TRegister);
begin
  emitX86(INST_SBB, dst, src);
end;

procedure TSerializerIntrinsics.sbb(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SBB, dst, src);
end;

procedure TSerializerIntrinsics.sal(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SAL, dst, src);
end;

procedure TSerializerIntrinsics.sal(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SAL, dst, src);
end;

procedure TSerializerIntrinsics.sal(dst: TMem; src: TRegister);
begin
  emitX86(INST_SAL, dst, src);
end;

procedure TSerializerIntrinsics.sal(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SAL, dst, src);
end;

procedure TSerializerIntrinsics.sar(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SAR, dst, src);
end;

procedure TSerializerIntrinsics.sar(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SAR, dst, src);
end;

procedure TSerializerIntrinsics.sar(dst: TMem; src: TRegister);
begin
  emitX86(INST_SAR, dst, src);
end;

procedure TSerializerIntrinsics.sar(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SAR, dst, src);
end;

procedure TSerializerIntrinsics.set_(cc: SysInt; dst: TRegister);
begin
  emitX86(conditionToSetCC(cc), dst);
end;

procedure TSerializerIntrinsics.set_(cc: SysInt; dst: TMem);
begin
  emitX86(conditionToSetCC(cc), dst);
end;

procedure TSerializerIntrinsics.seta  (dst: TRegister);
begin
  emitX86(INST_SETA  , dst);
end;

procedure TSerializerIntrinsics.seta  (dst: TMem);
begin
  emitX86(INST_SETA  , dst);
end;

procedure TSerializerIntrinsics.setae (dst: TRegister);
begin
  emitX86(INST_SETAE , dst);
end;

procedure TSerializerIntrinsics.setae (dst: TMem);
begin
  emitX86(INST_SETAE , dst);
end;

procedure TSerializerIntrinsics.setb  (dst: TRegister);
begin
  emitX86(INST_SETB  , dst);
end;

procedure TSerializerIntrinsics.setb  (dst: TMem);
begin
  emitX86(INST_SETB  , dst);
end;

procedure TSerializerIntrinsics.setbe (dst: TRegister);
begin
  emitX86(INST_SETBE , dst);
end;

procedure TSerializerIntrinsics.setbe (dst: TMem);
begin
  emitX86(INST_SETBE , dst);
end;

procedure TSerializerIntrinsics.setc  (dst: TRegister);
begin
  emitX86(INST_SETC  , dst);
end;

procedure TSerializerIntrinsics.setc  (dst: TMem);
begin
  emitX86(INST_SETC  , dst);
end;

procedure TSerializerIntrinsics.sete  (dst: TRegister);
begin
  emitX86(INST_SETE  , dst);
end;

procedure TSerializerIntrinsics.sete  (dst: TMem);
begin
  emitX86(INST_SETE  , dst);
end;

procedure TSerializerIntrinsics.setg  (dst: TRegister);
begin
  emitX86(INST_SETG  , dst);
end;

procedure TSerializerIntrinsics.setg  (dst: TMem);
begin
  emitX86(INST_SETG  , dst);
end;

procedure TSerializerIntrinsics.setge (dst: TRegister);
begin
  emitX86(INST_SETGE , dst);
end;

procedure TSerializerIntrinsics.setge (dst: TMem);
begin
  emitX86(INST_SETGE , dst);
end;

procedure TSerializerIntrinsics.setl  (dst: TRegister);
begin
  emitX86(INST_SETL  , dst);
end;

procedure TSerializerIntrinsics.setl  (dst: TMem);
begin
  emitX86(INST_SETL  , dst);
end;

procedure TSerializerIntrinsics.setle (dst: TRegister);
begin
  emitX86(INST_SETLE , dst);
end;

procedure TSerializerIntrinsics.setle (dst: TMem);
begin
  emitX86(INST_SETLE , dst);
end;

procedure TSerializerIntrinsics.setna (dst: TRegister);
begin
  emitX86(INST_SETNA , dst);
end;

procedure TSerializerIntrinsics.setna (dst: TMem);
begin
  emitX86(INST_SETNA , dst);
end;

procedure TSerializerIntrinsics.setnae(dst: TRegister);
begin
  emitX86(INST_SETNAE, dst);
end;

procedure TSerializerIntrinsics.setnae(dst: TMem);
begin
  emitX86(INST_SETNAE, dst);
end;

procedure TSerializerIntrinsics.setnb (dst: TRegister);
begin
  emitX86(INST_SETNB , dst);
end;

procedure TSerializerIntrinsics.setnb (dst: TMem);
begin
  emitX86(INST_SETNB , dst);
end;

procedure TSerializerIntrinsics.setnbe(dst: TRegister);
begin
  emitX86(INST_SETNBE, dst);
end;

procedure TSerializerIntrinsics.setnbe(dst: TMem);
begin
  emitX86(INST_SETNBE, dst);
end;

procedure TSerializerIntrinsics.setnc (dst: TRegister);
begin
  emitX86(INST_SETNC , dst);
end;

procedure TSerializerIntrinsics.setnc (dst: TMem);
begin
  emitX86(INST_SETNC , dst);
end;

procedure TSerializerIntrinsics.setne (dst: TRegister);
begin
  emitX86(INST_SETNE , dst);
end;

procedure TSerializerIntrinsics.setne (dst: TMem);
begin
  emitX86(INST_SETNE , dst);
end;

procedure TSerializerIntrinsics.setng (dst: TRegister);
begin
  emitX86(INST_SETNG , dst);
end;

procedure TSerializerIntrinsics.setng (dst: TMem);
begin
  emitX86(INST_SETNG , dst);
end;

procedure TSerializerIntrinsics.setnge(dst: TRegister);
begin
  emitX86(INST_SETNGE, dst);
end;

procedure TSerializerIntrinsics.setnge(dst: TMem);
begin
  emitX86(INST_SETNGE, dst);
end;

procedure TSerializerIntrinsics.setnl (dst: TRegister);
begin
  emitX86(INST_SETNL , dst);
end;

procedure TSerializerIntrinsics.setnl (dst: TMem);
begin
  emitX86(INST_SETNL , dst);
end;

procedure TSerializerIntrinsics.setnle(dst: TRegister);
begin
  emitX86(INST_SETNLE, dst);
end;

procedure TSerializerIntrinsics.setnle(dst: TMem);
begin
  emitX86(INST_SETNLE, dst);
end;

procedure TSerializerIntrinsics.setno (dst: TRegister);
begin
  emitX86(INST_SETNO , dst);
end;

procedure TSerializerIntrinsics.setno (dst: TMem);
begin
  emitX86(INST_SETNO , dst);
end;

procedure TSerializerIntrinsics.setnp (dst: TRegister);
begin
  emitX86(INST_SETNP , dst);
end;

procedure TSerializerIntrinsics.setnp (dst: TMem);
begin
  emitX86(INST_SETNP , dst);
end;

procedure TSerializerIntrinsics.setns (dst: TRegister);
begin
  emitX86(INST_SETNS , dst);
end;

procedure TSerializerIntrinsics.setns (dst: TMem);
begin
  emitX86(INST_SETNS , dst);
end;

procedure TSerializerIntrinsics.setnz (dst: TRegister);
begin
  emitX86(INST_SETNZ , dst);
end;

procedure TSerializerIntrinsics.setnz (dst: TMem);
begin
  emitX86(INST_SETNZ , dst);
end;

procedure TSerializerIntrinsics.seto  (dst: TRegister);
begin
  emitX86(INST_SETO  , dst);
end;

procedure TSerializerIntrinsics.seto  (dst: TMem);
begin
  emitX86(INST_SETO  , dst);
end;

procedure TSerializerIntrinsics.setp  (dst: TRegister);
begin
  emitX86(INST_SETP  , dst);
end;

procedure TSerializerIntrinsics.setp  (dst: TMem);
begin
  emitX86(INST_SETP  , dst);
end;

procedure TSerializerIntrinsics.setpe (dst: TRegister);
begin
  emitX86(INST_SETPE , dst);
end;

procedure TSerializerIntrinsics.setpe (dst: TMem);
begin
  emitX86(INST_SETPE , dst);
end;

procedure TSerializerIntrinsics.setpo (dst: TRegister);
begin
  emitX86(INST_SETPO , dst);
end;

procedure TSerializerIntrinsics.setpo (dst: TMem);
begin
  emitX86(INST_SETPO , dst);
end;

procedure TSerializerIntrinsics.sets  (dst: TRegister);
begin
  emitX86(INST_SETS  , dst);
end;

procedure TSerializerIntrinsics.sets  (dst: TMem);
begin
  emitX86(INST_SETS  , dst);
end;

procedure TSerializerIntrinsics.setz  (dst: TRegister);
begin
  emitX86(INST_SETZ  , dst);
end;

procedure TSerializerIntrinsics.setz  (dst: TMem);
begin
  emitX86(INST_SETZ  , dst);
end;

procedure TSerializerIntrinsics.shl_(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SHL, dst, src);
end;

procedure TSerializerIntrinsics.shl_(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SHL, dst, src);
end;

procedure TSerializerIntrinsics.shl_(dst: TMem; src: TRegister);
begin
  emitX86(INST_SHL, dst, src);
end;

procedure TSerializerIntrinsics.shl_(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SHL, dst, src);
end;

procedure TSerializerIntrinsics.shr_(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SHR, dst, src);
end;

procedure TSerializerIntrinsics.shr_(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SHR, dst, src);
end;

procedure TSerializerIntrinsics.shr_(dst: TMem; src: TRegister);
begin
  emitX86(INST_SHR, dst, src);
end;

procedure TSerializerIntrinsics.shr_(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SHR, dst, src);
end;

procedure TSerializerIntrinsics.shld(dst: TRegister; src1: TRegister; src2: TRegister);
begin
  emitX86(INST_SHLD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shld(dst: TRegister; src1: TRegister; src2: TImmediate);
begin
  emitX86(INST_SHLD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shld(dst: TMem; src1: TRegister; src2: TRegister);
begin
  emitX86(INST_SHLD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shld(dst: TMem; src1: TRegister; src2: TImmediate);
begin
  emitX86(INST_SHLD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shrd(dst: TRegister; src1: TRegister; src2: TRegister);
begin
  emitX86(INST_SHRD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shrd(dst: TRegister; src1: TRegister; src2: TImmediate);
begin
  emitX86(INST_SHRD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shrd(dst: TMem; src1: TRegister; src2: TRegister);
begin
  emitX86(INST_SHRD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.shrd(dst: TMem; src1: TRegister; src2: TImmediate);
begin
  emitX86(INST_SHRD, dst, src1, src2);
end;

procedure TSerializerIntrinsics.stc;
begin
  emitX86(INST_STC);
end;

procedure TSerializerIntrinsics.std;
begin
  emitX86(INST_STD);
end;

procedure TSerializerIntrinsics.sub(dst: TRegister; src: TRegister);
begin
  emitX86(INST_SUB, dst, src);
end;

procedure TSerializerIntrinsics.sub(dst: TRegister; src: TMem);
begin
  emitX86(INST_SUB, dst, src);
end;

procedure TSerializerIntrinsics.sub(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_SUB, dst, src);
end;

procedure TSerializerIntrinsics.sub(dst: TMem; src: TRegister);
begin
  emitX86(INST_SUB, dst, src);
end;

procedure TSerializerIntrinsics.sub(dst: TMem; src: TImmediate);
begin
  emitX86(INST_SUB, dst, src);
end;

procedure TSerializerIntrinsics.test(op1: TRegister; op2: TRegister);
begin
  emitX86(INST_TEST, op1, op2);
end;

procedure TSerializerIntrinsics.test(op1: TRegister; op2: TImmediate);
begin
  emitX86(INST_TEST, op1, op2);
end;

procedure TSerializerIntrinsics.test(op1: TMem; op2: TRegister);
begin
  emitX86(INST_TEST, op1, op2);
end;

procedure TSerializerIntrinsics.test(op1: TMem; op2: TImmediate);
begin
  emitX86(INST_TEST, op1, op2);
end;

procedure TSerializerIntrinsics.ud2;
begin
  emitX86(INST_UD2);
end;

procedure TSerializerIntrinsics.xadd(dst: TRegister; src: TRegister);
begin
  emitX86(INST_XADD, dst, src);
end;

procedure TSerializerIntrinsics.xadd(dst: TMem; src: TRegister);
begin
  emitX86(INST_XADD, dst, src);
end;

procedure TSerializerIntrinsics.xchg(dst: TRegister; src: TRegister);
begin
  emitX86(INST_XCHG, dst, src);
end;

procedure TSerializerIntrinsics.xchg(dst: TMem; src: TRegister);
begin
  emitX86(INST_XCHG, dst, src);
end;

procedure TSerializerIntrinsics.xchg(dst: TRegister; src: TMem);
begin
  emitX86(INST_XCHG, src, dst);
end;

procedure TSerializerIntrinsics.xor_(dst: TRegister; src: TRegister);
begin
  emitX86(INST_XOR, dst, src);
end;

procedure TSerializerIntrinsics.xor_(dst: TRegister; src: TMem);
begin
  emitX86(INST_XOR, dst, src);
end;

procedure TSerializerIntrinsics.xor_(dst: TRegister; src: TImmediate);
begin
  emitX86(INST_XOR, dst, src);
end;

procedure TSerializerIntrinsics.xor_(dst: TMem; src: TRegister);
begin
  emitX86(INST_XOR, dst, src);
end;

procedure TSerializerIntrinsics.xor_(dst: TMem; src: TImmediate);
begin
  emitX86(INST_XOR, dst, src);
end;

procedure TSerializerIntrinsics.f2xm1;
begin
  emitX86(INST_F2XM1);
end;

procedure TSerializerIntrinsics.fabs;
begin
  emitX86(INST_FABS);
end;

procedure TSerializerIntrinsics.fadd(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FADD, dst, src);
end;

procedure TSerializerIntrinsics.fadd(src: TMem);
begin
  emitX86(INST_FADD, src);
end;

procedure TSerializerIntrinsics.faddp(dst: TX87Register);
begin
  emitX86(INST_FADDP, dst);
end;

procedure TSerializerIntrinsics.fbld(src: TMem);
begin
  emitX86(INST_FBLD, src);
end;

procedure TSerializerIntrinsics.fbstp(dst: TMem);
begin
  emitX86(INST_FBSTP, dst);
end;

procedure TSerializerIntrinsics.fchs;
begin
  emitX86(INST_FCHS);
end;

procedure TSerializerIntrinsics.fclex;
begin
  emitX86(INST_FCLEX);
end;

procedure TSerializerIntrinsics.fcmovb(src: TX87Register);
begin
  emitX86(INST_FCMOVB, src);
end;

procedure TSerializerIntrinsics.fcmovbe(src: TX87Register);
begin
  emitX86(INST_FCMOVBE, src);
end;

procedure TSerializerIntrinsics.fcmove(src: TX87Register);
begin
  emitX86(INST_FCMOVE, src);
end;

procedure TSerializerIntrinsics.fcmovnb(src: TX87Register);
begin
  emitX86(INST_FCMOVNB, src);
end;

procedure TSerializerIntrinsics.fcmovnbe(src: TX87Register);
begin
  emitX86(INST_FCMOVNBE, src);
end;

procedure TSerializerIntrinsics.fcmovne(src: TX87Register);
begin
  emitX86(INST_FCMOVNE, src);
end;

procedure TSerializerIntrinsics.fcmovnu(src: TX87Register);
begin
  emitX86(INST_FCMOVNU, src);
end;

procedure TSerializerIntrinsics.fcmovu(src: TX87Register);
begin
  emitX86(INST_FCMOVU, src);
end;

procedure TSerializerIntrinsics.fcom(reg: TX87Register);
begin
  emitX86(INST_FCOM, reg);
end;

procedure TSerializerIntrinsics.fcom(src: TMem);
begin
  emitX86(INST_FCOM, src);
end;

procedure TSerializerIntrinsics.fcomp(reg: TX87Register);
begin
  emitX86(INST_FCOMP, reg);
end;

procedure TSerializerIntrinsics.fcomp(mem: TMem);
begin
  emitX86(INST_FCOMP, mem);
end;

procedure TSerializerIntrinsics.fcompp;
begin
  emitX86(INST_FCOMPP);
end;

procedure TSerializerIntrinsics.fcomi(reg: TX87Register);
begin
  emitX86(INST_FCOMI, reg);
end;

procedure TSerializerIntrinsics.fcomip(reg: TX87Register);
begin
  emitX86(INST_FCOMIP, reg);
end;

procedure TSerializerIntrinsics.fcos;
begin
  emitX86(INST_FCOS);
end;

procedure TSerializerIntrinsics.fdecstp;
begin
  emitX86(INST_FDECSTP);
end;

procedure TSerializerIntrinsics.fdiv(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FDIV, dst, src);
end;

procedure TSerializerIntrinsics.fdiv(src: TMem);
begin
  emitX86(INST_FDIV, src);
end;

procedure TSerializerIntrinsics.fdivp(reg: TX87Register);
begin
  emitX86(INST_FDIVP, reg);
end;

procedure TSerializerIntrinsics.fdivr(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FDIVR, dst, src);
end;

procedure TSerializerIntrinsics.fdivr(src: TMem);
begin
  emitX86(INST_FDIVR, src);
end;

procedure TSerializerIntrinsics.fdivrp(reg: TX87Register);
begin
  emitX86(INST_FDIVRP, reg);
end;

procedure TSerializerIntrinsics.ffree(reg: TX87Register);
begin
  emitX86(INST_FFREE, reg);
end;

procedure TSerializerIntrinsics.fiadd(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FIADD, src);
end;

procedure TSerializerIntrinsics.ficom(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FICOM, src);
end;

procedure TSerializerIntrinsics.ficomp(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FICOMP, src);
end;

procedure TSerializerIntrinsics.fidiv(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FIDIV, src);
end;

procedure TSerializerIntrinsics.fidivr(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FIDIVR, src);
end;

procedure TSerializerIntrinsics.fild(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4) or (src.size = 8));
  emitX86(INST_FILD, src);
end;

procedure TSerializerIntrinsics.fimul(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FIMUL, src);
end;

procedure TSerializerIntrinsics.fincstp;
begin
  emitX86(INST_FINCSTP);
end;

procedure TSerializerIntrinsics.finit;
begin
  emitX86(INST_FINIT);
end;

procedure TSerializerIntrinsics.fisub(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FISUB, src);
end;

procedure TSerializerIntrinsics.fisubr(src: TMem);
begin
  Assert((src.size = 2) or (src.size = 4));
  emitX86(INST_FISUBR, src);
end;

procedure TSerializerIntrinsics.fninit;
begin
  emitX86(INST_FNINIT);
end;

procedure TSerializerIntrinsics.fist(dst: TMem);
begin
  Assert((dst.size = 2) or (dst.size = 4));
  emitX86(INST_FIST, dst);
end;

procedure TSerializerIntrinsics.fistp(dst: TMem);
begin
  Assert((dst.size = 2) or (dst.size = 4) or (dst.size = 8));
  emitX86(INST_FISTP, dst);
end;

procedure TSerializerIntrinsics.fld(src: TMem);
begin
  Assert((src.size = 4) or (src.size = 8) or (src.size = 10));
  emitX86(INST_FLD, src);
end;

procedure TSerializerIntrinsics.fld(reg: TX87Register);
begin
  emitX86(INST_FLD, reg);
end;

procedure TSerializerIntrinsics.fld1;
begin
  emitX86(INST_FLD1);
end;

procedure TSerializerIntrinsics.fldl2t;
begin
  emitX86(INST_FLDL2T);
end;

procedure TSerializerIntrinsics.fldl2e;
begin
  emitX86(INST_FLDL2E);
end;

procedure TSerializerIntrinsics.fldpi;
begin
  emitX86(INST_FLDPI);
end;

procedure TSerializerIntrinsics.fldlg2;
begin
  emitX86(INST_FLDLG2);
end;

procedure TSerializerIntrinsics.fldln2;
begin
  emitX86(INST_FLDLN2);
end;

procedure TSerializerIntrinsics.fldz;
begin
  emitX86(INST_FLDZ);
end;

procedure TSerializerIntrinsics.fldcw(src: TMem);
begin
  emitX86(INST_FLDCW, src);
end;

procedure TSerializerIntrinsics.fldenv(src: TMem);
begin
  emitX86(INST_FLDENV, src);
end;

procedure TSerializerIntrinsics.fmul(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FMUL, dst, src);
end;

procedure TSerializerIntrinsics.fmul(src: TMem);
begin
  emitX86(INST_FMUL, src);
end;

procedure TSerializerIntrinsics.fmulp(dst: TX87Register);
begin
  emitX86(INST_FMULP, dst);
end;

procedure TSerializerIntrinsics.fnclex;
begin
  emitX86(INST_FNCLEX);
end;

procedure TSerializerIntrinsics.fnop;
begin
  emitX86(INST_FNOP);
end;

procedure TSerializerIntrinsics.fnsave(dst: TMem);
begin
  emitX86(INST_FNSAVE, dst);
end;

procedure TSerializerIntrinsics.fnstenv(dst: TMem);
begin
  emitX86(INST_FNSTENV, dst);
end;

procedure TSerializerIntrinsics.fnstcw(dst: TMem);
begin
  emitX86(INST_FNSTCW, dst);
end;

procedure TSerializerIntrinsics.fnstsw(dst: TRegister);
begin
  Assert(dst.isRegCode(REG_AX));
  emitX86(INST_FNSTSW, dst);
end;

procedure TSerializerIntrinsics.fnstsw(dst: TMem);
begin
  emitX86(INST_FNSTSW, dst);
end;

procedure TSerializerIntrinsics.fpatan;
begin
  emitX86(INST_FPATAN);
end;

procedure TSerializerIntrinsics.fprem;
begin
  emitX86(INST_FPREM);
end;

procedure TSerializerIntrinsics.fprem1;
begin
  emitX86(INST_FPREM1);
end;

procedure TSerializerIntrinsics.fptan;
begin
  emitX86(INST_FPTAN);
end;

procedure TSerializerIntrinsics.frndint;
begin
  emitX86(INST_FRNDINT);
end;

procedure TSerializerIntrinsics.frstor(src: TMem);
begin
  emitX86(INST_FRSTOR, src);
end;

procedure TSerializerIntrinsics.fsave(dst: TMem);
begin
  emitX86(INST_FSAVE, dst);
end;

procedure TSerializerIntrinsics.fscale;
begin
  emitX86(INST_FSCALE);
end;

procedure TSerializerIntrinsics.fsin;
begin
  emitX86(INST_FSIN);
end;

procedure TSerializerIntrinsics.fsincos;
begin
  emitX86(INST_FSINCOS);
end;

procedure TSerializerIntrinsics.fsqrt;
begin
  emitX86(INST_FSQRT);
end;

procedure TSerializerIntrinsics.fst(dst: TMem);
begin
  Assert((dst.size = 4) or (dst.size = 8));
  emitX86(INST_FST, dst);
end;

procedure TSerializerIntrinsics.fst(reg: TX87Register);
begin
  emitX86(INST_FST, reg);
end;

procedure TSerializerIntrinsics.fstp(dst: TMem);
begin
  Assert((dst.size = 4) or (dst.size = 8) or (dst.size = 10));
  emitX86(INST_FSTP, dst);
end;

procedure TSerializerIntrinsics.fstp(reg: TX87Register);
begin
  emitX86(INST_FSTP, reg);
end;

procedure TSerializerIntrinsics.fstcw(dst: TMem);
begin
  emitX86(INST_FSTCW, dst);
end;

procedure TSerializerIntrinsics.fstenv(dst: TMem);
begin
  emitX86(INST_FSTENV, dst);
end;

procedure TSerializerIntrinsics.fstsw(dst: TRegister);
begin
  Assert(dst.isRegCode(REG_AX));
  emitX86(INST_FSTSW, dst);
end;

procedure TSerializerIntrinsics.fstsw(dst: TMem);
begin
  emitX86(INST_FSTSW, dst);
end;

procedure TSerializerIntrinsics.fsub(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FSUB, dst, src);
end;

procedure TSerializerIntrinsics.fsub(src: TMem);
begin
  Assert((src.size = 4) or (src.size = 8));
  emitX86(INST_FSUB, src);
end;

procedure TSerializerIntrinsics.fsubp(dst: TX87Register);
begin
  emitX86(INST_FSUBP, dst);
end;

procedure TSerializerIntrinsics.fsubr(dst: TX87Register; src: TX87Register);
begin
  Assert((dst.index = 0) or (src.index = 0));
  emitX86(INST_FSUBR, dst, src);
end;

procedure TSerializerIntrinsics.fsubr(src: TMem);
begin
  Assert((src.size = 4) or (src.size = 8));
  emitX86(INST_FSUBR, src);
end;

procedure TSerializerIntrinsics.fsubrp(dst: TX87Register);
begin
  emitX86(INST_FSUBRP, dst);
end;

procedure TSerializerIntrinsics.ftst;
begin
  emitX86(INST_FTST);
end;

procedure TSerializerIntrinsics.fucom(reg: TX87Register);
begin
  emitX86(INST_FUCOM, reg);
end;

procedure TSerializerIntrinsics.fucomi(reg: TX87Register);
begin
  emitX86(INST_FUCOMI, reg);
end;

procedure TSerializerIntrinsics.fucomip(reg: TX87Register);
begin
  emitX86(INST_FUCOMIP, reg);
end;

procedure TSerializerIntrinsics.fucomp(reg: TX87Register);
begin
  emitX86(INST_FUCOMP, reg);
end;

procedure TSerializerIntrinsics.fucompp;
begin
  emitX86(INST_FUCOMPP);
end;

procedure TSerializerIntrinsics.fwait;
begin
  emitX86(INST_FWAIT);
end;

procedure TSerializerIntrinsics.fxam;
begin
  emitX86(INST_FXAM);
end;

procedure TSerializerIntrinsics.fxch(reg: TX87Register);
begin
  emitX86(INST_FXCH, reg);
end;

procedure TSerializerIntrinsics.fxrstor(src: TMem);
begin
  emitX86(INST_FXRSTOR, src);
end;

procedure TSerializerIntrinsics.fxsave(dst: TMem);
begin
  emitX86(INST_FXSAVE, dst);
end;

procedure TSerializerIntrinsics.fxtract;
begin
  emitX86(INST_FXTRACT);
end;

procedure TSerializerIntrinsics.fyl2x;
begin
  emitX86(INST_FYL2X);
end;

procedure TSerializerIntrinsics.fyl2xp1;
begin
  emitX86(INST_FYL2XP1);
end;

procedure TSerializerIntrinsics.emms;
begin
  emitX86(INST_EMMS);
end;

procedure TSerializerIntrinsics.movd(dst: TMem; src: TMMRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TRegister; src: TMMRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TMMRegister; src: TRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;

procedure TSerializerIntrinsics.movq(dst: TMem; src: TMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$IFDEF ASMJIT_X64}

procedure TSerializerIntrinsics.movq(dst: TRegister; src: TMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$ENDIF}

procedure TSerializerIntrinsics.movq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$IFDEF ASMJIT_X64}

procedure TSerializerIntrinsics.movq(dst: TMMRegister; src: TRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$ENDIF}

procedure TSerializerIntrinsics.packuswb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PACKUSWB, dst, src);
end;

procedure TSerializerIntrinsics.packuswb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PACKUSWB, dst, src);
end;

procedure TSerializerIntrinsics.paddb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDB, dst, src);
end;

procedure TSerializerIntrinsics.paddb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDB, dst, src);
end;

procedure TSerializerIntrinsics.paddw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDW, dst, src);
end;

procedure TSerializerIntrinsics.paddw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDW, dst, src);
end;

procedure TSerializerIntrinsics.paddd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDD, dst, src);
end;

procedure TSerializerIntrinsics.paddd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDD, dst, src);
end;

procedure TSerializerIntrinsics.paddsb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDSB, dst, src);
end;

procedure TSerializerIntrinsics.paddsb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDSB, dst, src);
end;

procedure TSerializerIntrinsics.paddsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDSW, dst, src);
end;

procedure TSerializerIntrinsics.paddsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDSW, dst, src);
end;

procedure TSerializerIntrinsics.paddusb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDUSB, dst, src);
end;

procedure TSerializerIntrinsics.paddusb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDUSB, dst, src);
end;

procedure TSerializerIntrinsics.paddusw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDUSW, dst, src);
end;

procedure TSerializerIntrinsics.paddusw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDUSW, dst, src);
end;

procedure TSerializerIntrinsics.pand(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PAND, dst, src);
end;

procedure TSerializerIntrinsics.pand(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PAND, dst, src);
end;

procedure TSerializerIntrinsics.pandn(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PANDN, dst, src);
end;

procedure TSerializerIntrinsics.pandn(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PANDN, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPEQB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPEQW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPEQD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPGTB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPGTW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PCMPGTD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTD, dst, src);
end;

procedure TSerializerIntrinsics.pmulhw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMULHW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMULHW, dst, src);
end;

procedure TSerializerIntrinsics.pmullw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMULLW, dst, src);
end;

procedure TSerializerIntrinsics.pmullw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMULLW, dst, src);
end;

procedure TSerializerIntrinsics.por(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_POR, dst, src);
end;

procedure TSerializerIntrinsics.por(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_POR, dst, src);
end;

procedure TSerializerIntrinsics.pmaddwd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMADDWD, dst, src);
end;

procedure TSerializerIntrinsics.pmaddwd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMADDWD, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psrld(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrld(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrld(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psubb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBB, dst, src);
end;

procedure TSerializerIntrinsics.psubb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBB, dst, src);
end;

procedure TSerializerIntrinsics.psubw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBW, dst, src);
end;

procedure TSerializerIntrinsics.psubw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBW, dst, src);
end;

procedure TSerializerIntrinsics.psubd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBD, dst, src);
end;

procedure TSerializerIntrinsics.psubd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBD, dst, src);
end;

procedure TSerializerIntrinsics.psubsb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBSB, dst, src);
end;

procedure TSerializerIntrinsics.psubsb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBSB, dst, src);
end;

procedure TSerializerIntrinsics.psubsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.psubsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.psubusb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBUSB, dst, src);
end;

procedure TSerializerIntrinsics.psubusb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBUSB, dst, src);
end;

procedure TSerializerIntrinsics.psubusw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBUSW, dst, src);
end;

procedure TSerializerIntrinsics.psubusw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBUSW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhbw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKHBW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhbw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHBW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhwd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKHWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckhwd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckhdq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKHDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckhdq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpcklbw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKLBW, dst, src);
end;

procedure TSerializerIntrinsics.punpcklbw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLBW, dst, src);
end;

procedure TSerializerIntrinsics.punpcklwd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKLWD, dst, src);
end;

procedure TSerializerIntrinsics.punpcklwd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckldq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PUNPCKLDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckldq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLDQ, dst, src);
end;

procedure TSerializerIntrinsics.pxor(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PXOR, dst, src);
end;

procedure TSerializerIntrinsics.pxor(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PXOR, dst, src);
end;

procedure TSerializerIntrinsics.femms;
begin
  emitX86(INST_FEMMS);
end;

procedure TSerializerIntrinsics.pf2id(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PF2ID, dst, src);
end;

procedure TSerializerIntrinsics.pf2id(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PF2ID, dst, src);
end;

procedure TSerializerIntrinsics.pf2iw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PF2IW, dst, src);
end;

procedure TSerializerIntrinsics.pf2iw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PF2IW, dst, src);
end;

procedure TSerializerIntrinsics.pfacc(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFACC, dst, src);
end;

procedure TSerializerIntrinsics.pfacc(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFACC, dst, src);
end;

procedure TSerializerIntrinsics.pfadd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFADD, dst, src);
end;

procedure TSerializerIntrinsics.pfadd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFADD, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpeq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFCMPEQ, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpeq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFCMPEQ, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpge(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFCMPGE, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpge(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFCMPGE, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpgt(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFCMPGT, dst, src);
end;

procedure TSerializerIntrinsics.pfcmpgt(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFCMPGT, dst, src);
end;

procedure TSerializerIntrinsics.pfmax(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFMAX, dst, src);
end;

procedure TSerializerIntrinsics.pfmax(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFMAX, dst, src);
end;

procedure TSerializerIntrinsics.pfmin(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFMIN, dst, src);
end;

procedure TSerializerIntrinsics.pfmin(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFMIN, dst, src);
end;

procedure TSerializerIntrinsics.pfmul(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFMUL, dst, src);
end;

procedure TSerializerIntrinsics.pfmul(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFMUL, dst, src);
end;

procedure TSerializerIntrinsics.pfnacc(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFNACC, dst, src);
end;

procedure TSerializerIntrinsics.pfnacc(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFNACC, dst, src);
end;

procedure TSerializerIntrinsics.pfpnaxx(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFPNACC, dst, src);
end;

procedure TSerializerIntrinsics.pfpnacc(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFPNACC, dst, src);
end;

procedure TSerializerIntrinsics.pfrcp(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFRCP, dst, src);
end;

procedure TSerializerIntrinsics.pfrcp(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFRCP, dst, src);
end;

procedure TSerializerIntrinsics.pfrcpit1(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFRCPIT1, dst, src);
end;

procedure TSerializerIntrinsics.pfrcpit1(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFRCPIT1, dst, src);
end;

procedure TSerializerIntrinsics.pfrcpit2(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFRCPIT2, dst, src);
end;

procedure TSerializerIntrinsics.pfrcpit2(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFRCPIT2, dst, src);
end;

procedure TSerializerIntrinsics.pfrsqit1(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFRSQIT1, dst, src);
end;

procedure TSerializerIntrinsics.pfrsqit1(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFRSQIT1, dst, src);
end;

procedure TSerializerIntrinsics.pfrsqrt(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFRSQRT, dst, src);
end;

procedure TSerializerIntrinsics.pfrsqrt(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFRSQRT, dst, src);
end;

procedure TSerializerIntrinsics.pfsub(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFSUB, dst, src);
end;

procedure TSerializerIntrinsics.pfsub(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFSUB, dst, src);
end;

procedure TSerializerIntrinsics.pfsubr(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PFSUBR, dst, src);
end;

procedure TSerializerIntrinsics.pfsubr(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PFSUBR, dst, src);
end;

procedure TSerializerIntrinsics.pi2fd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PI2FD, dst, src);
end;

procedure TSerializerIntrinsics.pi2fd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PI2FD, dst, src);
end;

procedure TSerializerIntrinsics.pi2fw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PI2FW, dst, src);
end;

procedure TSerializerIntrinsics.pi2fw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PI2FW, dst, src);
end;

procedure TSerializerIntrinsics.pswapd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSWAPD, dst, src);
end;

procedure TSerializerIntrinsics.pswapd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSWAPD, dst, src);
end;

procedure TSerializerIntrinsics.addps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDPS, dst, src);
end;

procedure TSerializerIntrinsics.addps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDPS, dst, src);
end;

procedure TSerializerIntrinsics.addss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDSS, dst, src);
end;

procedure TSerializerIntrinsics.addss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDSS, dst, src);
end;

procedure TSerializerIntrinsics.andnps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ANDNPS, dst, src);
end;

procedure TSerializerIntrinsics.andnps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ANDNPS, dst, src);
end;

procedure TSerializerIntrinsics.andps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ANDPS, dst, src);
end;

procedure TSerializerIntrinsics.andps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ANDPS, dst, src);
end;

procedure TSerializerIntrinsics.cmpps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_CMPPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmpps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_CMPPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmpss(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_CMPSS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmpss(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_CMPSS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.comiss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_COMISS, dst, src);
end;

procedure TSerializerIntrinsics.comiss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_COMISS, dst, src);
end;

procedure TSerializerIntrinsics.cvtpi2ps(dst: TXMMRegister; src: TMMRegister);
begin
  emitX86(INST_CVTPI2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtpi2ps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPI2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2pi(dst: TMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPS2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2pi(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_CVTPS2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvtsi2ss(dst: TXMMRegister; src: TRegister);
begin
  emitX86(INST_CVTSI2SS, dst, src);
end;

procedure TSerializerIntrinsics.cvtsi2ss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTSI2SS, dst, src);
end;

procedure TSerializerIntrinsics.cvtss2si(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTSS2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvtss2si(dst: TRegister; src: TMem);
begin
  emitX86(INST_CVTSS2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvttps2pi(dst: TMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTPS2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvttps2pi(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_CVTTPS2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvttss2si(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTSS2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvttss2si(dst: TRegister; src: TMem);
begin
  emitX86(INST_CVTTSS2SI, dst, src);
end;

procedure TSerializerIntrinsics.divps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_DIVPS, dst, src);
end;

procedure TSerializerIntrinsics.divps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_DIVPS, dst, src);
end;

procedure TSerializerIntrinsics.divss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_DIVSS, dst, src);
end;

procedure TSerializerIntrinsics.divss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_DIVSS, dst, src);
end;

procedure TSerializerIntrinsics.ldmxcsr(src: TMem);
begin
  emitX86(INST_LDMXCSR, src);
end;

procedure TSerializerIntrinsics.maskmovq(data: TMMRegister; mask: TMMRegister);
begin
  emitX86(INST_MASKMOVQ, data, mask);
end;

procedure TSerializerIntrinsics.maxps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MAXPS, dst, src);
end;

procedure TSerializerIntrinsics.maxps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MAXPS, dst, src);
end;

procedure TSerializerIntrinsics.maxss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MAXSS, dst, src);
end;

procedure TSerializerIntrinsics.maxss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MAXSS, dst, src);
end;

procedure TSerializerIntrinsics.minps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MINPS, dst, src);
end;

procedure TSerializerIntrinsics.minps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MINPS, dst, src);
end;

procedure TSerializerIntrinsics.minss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MINSS, dst, src);
end;

procedure TSerializerIntrinsics.minss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MINSS, dst, src);
end;

procedure TSerializerIntrinsics.movaps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVAPS, dst, src);
end;

procedure TSerializerIntrinsics.movaps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVAPS, dst, src);
end;

procedure TSerializerIntrinsics.movaps(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVAPS, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movd(dst: TXMMRegister; src: TRegister);
begin
  emitX86(INST_MOVD, dst, src);
end;

procedure TSerializerIntrinsics.movq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;

procedure TSerializerIntrinsics.movq(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$IFDEF ASMJIT_X64}

procedure TSerializerIntrinsics.movq(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$ENDIF}

procedure TSerializerIntrinsics.movq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$IFDEF ASMJIT_X64}

procedure TSerializerIntrinsics.movq(dst: TXMMRegister; src: TRegister);
begin
  emitX86(INST_MOVQ, dst, src);
end;
{$ENDIF}

procedure TSerializerIntrinsics.movntq(dst: TMem; src: TMMRegister);
begin
  emitX86(INST_MOVNTQ, dst, src);
end;

procedure TSerializerIntrinsics.movhlps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVHLPS, dst, src);
end;

procedure TSerializerIntrinsics.movhps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVHPS, dst, src);
end;

procedure TSerializerIntrinsics.movhps(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVHPS, dst, src);
end;

procedure TSerializerIntrinsics.movlhps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVLHPS, dst, src);
end;

procedure TSerializerIntrinsics.movlps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVLPS, dst, src);
end;

procedure TSerializerIntrinsics.movlps(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVLPS, dst, src);
end;

procedure TSerializerIntrinsics.movntps(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVNTPS, dst, src);
end;

procedure TSerializerIntrinsics.movss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVSS, dst, src);
end;

procedure TSerializerIntrinsics.movss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVSS, dst, src);
end;

procedure TSerializerIntrinsics.movss(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVSS, dst, src);
end;

procedure TSerializerIntrinsics.movups(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVUPS, dst, src);
end;

procedure TSerializerIntrinsics.movups(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVUPS, dst, src);
end;

procedure TSerializerIntrinsics.movups(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVUPS, dst, src);
end;

procedure TSerializerIntrinsics.mulps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MULPS, dst, src);
end;

procedure TSerializerIntrinsics.mulps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MULPS, dst, src);
end;

procedure TSerializerIntrinsics.mulss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MULSS, dst, src);
end;

procedure TSerializerIntrinsics.mulss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MULSS, dst, src);
end;

procedure TSerializerIntrinsics.orps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ORPS, dst, src);
end;

procedure TSerializerIntrinsics.orps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ORPS, dst, src);
end;

procedure TSerializerIntrinsics.pavgb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PAVGB, dst, src);
end;

procedure TSerializerIntrinsics.pavgb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PAVGB, dst, src);
end;

procedure TSerializerIntrinsics.pavgw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PAVGW, dst, src);
end;

procedure TSerializerIntrinsics.pavgw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PAVGW, dst, src);
end;

procedure TSerializerIntrinsics.pextrw(dst: TRegister; src: TMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrw(dst: TMMRegister; src: TRegister; imm8: TImmediate);
begin
  emitX86(INST_PINSRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrw(dst: TMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PINSRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pmaxsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMAXSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMAXSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxub(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMAXUB, dst, src);
end;

procedure TSerializerIntrinsics.pmaxub(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMAXUB, dst, src);
end;

procedure TSerializerIntrinsics.pminsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMINSW, dst, src);
end;

procedure TSerializerIntrinsics.pminsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMINSW, dst, src);
end;

procedure TSerializerIntrinsics.pminub(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMINUB, dst, src);
end;

procedure TSerializerIntrinsics.pminub(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMINUB, dst, src);
end;

procedure TSerializerIntrinsics.pmovmskb(dst: TRegister; src: TMMRegister);
begin
  emitX86(INST_PMOVMSKB, dst, src);
end;

procedure TSerializerIntrinsics.pmulhuw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMULHUW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhuw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMULHUW, dst, src);
end;

procedure TSerializerIntrinsics.psadbw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSADBW, dst, src);
end;

procedure TSerializerIntrinsics.psadbw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSADBW, dst, src);
end;

procedure TSerializerIntrinsics.pshufw(dst: TMMRegister; src: TMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PSHUFW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshufw(dst: TMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PSHUFW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.rcpps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_RCPPS, dst, src);
end;

procedure TSerializerIntrinsics.rcpps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_RCPPS, dst, src);
end;

procedure TSerializerIntrinsics.rcpss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_RCPSS, dst, src);
end;

procedure TSerializerIntrinsics.rcpss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_RCPSS, dst, src);
end;

procedure TSerializerIntrinsics.prefetch(mem: TMem; hint: TImmediate);
begin
  emitX86(INST_PREFETCH, mem, hint);
end;

procedure TSerializerIntrinsics.psadbw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSADBW, dst, src);
end;

procedure TSerializerIntrinsics.psadbw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSADBW, dst, src);
end;

procedure TSerializerIntrinsics.rsqrtps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_RSQRTPS, dst, src);
end;

procedure TSerializerIntrinsics.rsqrtps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_RSQRTPS, dst, src);
end;

procedure TSerializerIntrinsics.rsqrtss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_RSQRTSS, dst, src);
end;

procedure TSerializerIntrinsics.rsqrtss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_RSQRTSS, dst, src);
end;

procedure TSerializerIntrinsics.sfence;
begin
  emitX86(INST_SFENCE);
end;

procedure TSerializerIntrinsics.shufps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_SHUFPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.shufps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_SHUFPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.sqrtps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SQRTPS, dst, src);
end;

procedure TSerializerIntrinsics.sqrtps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SQRTPS, dst, src);
end;

procedure TSerializerIntrinsics.sqrtss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SQRTSS, dst, src);
end;

procedure TSerializerIntrinsics.sqrtss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SQRTSS, dst, src);
end;

procedure TSerializerIntrinsics.stmxcsr(dst: TMem);
begin
  emitX86(INST_STMXCSR, dst);
end;

procedure TSerializerIntrinsics.subps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SUBPS, dst, src);
end;

procedure TSerializerIntrinsics.subps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SUBPS, dst, src);
end;

procedure TSerializerIntrinsics.subss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SUBSS, dst, src);
end;

procedure TSerializerIntrinsics.subss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SUBSS, dst, src);
end;

procedure TSerializerIntrinsics.ucomiss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UCOMISS, dst, src);
end;

procedure TSerializerIntrinsics.ucomiss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UCOMISS, dst, src);
end;

procedure TSerializerIntrinsics.unpckhps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UNPCKHPS, dst, src);
end;

procedure TSerializerIntrinsics.unpckhps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UNPCKHPS, dst, src);
end;

procedure TSerializerIntrinsics.unpcklps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UNPCKLPS, dst, src);
end;

procedure TSerializerIntrinsics.unpcklps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UNPCKLPS, dst, src);
end;

procedure TSerializerIntrinsics.xorps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_XORPS, dst, src);
end;

procedure TSerializerIntrinsics.xorps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_XORPS, dst, src);
end;

procedure TSerializerIntrinsics.addpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDPD, dst, src);
end;

procedure TSerializerIntrinsics.addpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDPD, dst, src);
end;

procedure TSerializerIntrinsics.addsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDSD, dst, src);
end;

procedure TSerializerIntrinsics.addsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDSD, dst, src);
end;

procedure TSerializerIntrinsics.andnpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ANDNPD, dst, src);
end;

procedure TSerializerIntrinsics.andnpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ANDNPD, dst, src);
end;

procedure TSerializerIntrinsics.andpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ANDPD, dst, src);
end;

procedure TSerializerIntrinsics.andpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ANDPD, dst, src);
end;

procedure TSerializerIntrinsics.clflush(mem: TMem);
begin
  emitX86(INST_CLFLUSH, mem);
end;

procedure TSerializerIntrinsics.cmppd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_CMPPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmppd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_CMPPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmpsd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_CMPSD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.cmpsd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_CMPSD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.comisd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_COMISD, dst, src);
end;

procedure TSerializerIntrinsics.comisd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_COMISD, dst, src);
end;

procedure TSerializerIntrinsics.cvtdq2pd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTDQ2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtdq2pd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTDQ2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtdq2ps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTDQ2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtdq2ps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTDQ2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2dq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPD2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2dq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPD2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2pi(dst: TMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPD2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2pi(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_CVTPD2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2ps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPD2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtpd2ps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPD2PS, dst, src);
end;

procedure TSerializerIntrinsics.cvtpi2pd(dst: TXMMRegister; src: TMMRegister);
begin
  emitX86(INST_CVTPI2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtpi2pd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPI2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2dq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPS2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2dq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPS2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2pd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTPS2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtps2pd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTPS2PD, dst, src);
end;

procedure TSerializerIntrinsics.cvtsd2si(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTSD2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvtsd2si(dst: TRegister; src: TMem);
begin
  emitX86(INST_CVTSD2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvtsd2ss(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTSD2SS, dst, src);
end;

procedure TSerializerIntrinsics.cvtsd2ss(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTSD2SS, dst, src);
end;

procedure TSerializerIntrinsics.cvtsi2sd(dst: TXMMRegister; src: TRegister);
begin
  emitX86(INST_CVTSI2SD, dst, src);
end;

procedure TSerializerIntrinsics.cvtsi2sd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTSI2SD, dst, src);
end;

procedure TSerializerIntrinsics.cvtss2sd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTSS2SD, dst, src);
end;

procedure TSerializerIntrinsics.cvtss2sd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTSS2SD, dst, src);
end;

procedure TSerializerIntrinsics.cvttpd2pi(dst: TMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTPD2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvttpd2pi(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_CVTTPD2PI, dst, src);
end;

procedure TSerializerIntrinsics.cvttpd2dq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTPD2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvttpd2dq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTTPD2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvttps2dq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTPS2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvttps2dq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_CVTTPS2DQ, dst, src);
end;

procedure TSerializerIntrinsics.cvttsd2si(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_CVTTSD2SI, dst, src);
end;

procedure TSerializerIntrinsics.cvttsd2si(dst: TRegister; src: TMem);
begin
  emitX86(INST_CVTTSD2SI, dst, src);
end;

procedure TSerializerIntrinsics.divpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_DIVPD, dst, src);
end;

procedure TSerializerIntrinsics.divpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_DIVPD, dst, src);
end;

procedure TSerializerIntrinsics.divsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_DIVSD, dst, src);
end;

procedure TSerializerIntrinsics.divsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_DIVSD, dst, src);
end;

procedure TSerializerIntrinsics.lfence;
begin
  emitX86(INST_LFENCE);
end;

procedure TSerializerIntrinsics.maskmovdqu(src: TXMMRegister; mask: TXMMRegister);
begin
  emitX86(INST_MASKMOVDQU, src, mask);
end;

procedure TSerializerIntrinsics.maxpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MAXPD, dst, src);
end;

procedure TSerializerIntrinsics.maxpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MAXPD, dst, src);
end;

procedure TSerializerIntrinsics.maxsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MAXSD, dst, src);
end;

procedure TSerializerIntrinsics.maxsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MAXSD, dst, src);
end;

procedure TSerializerIntrinsics.mfence;
begin
  emitX86(INST_MFENCE);
end;

procedure TSerializerIntrinsics.minpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MINPD, dst, src);
end;

procedure TSerializerIntrinsics.minpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MINPD, dst, src);
end;

procedure TSerializerIntrinsics.minsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MINSD, dst, src);
end;

procedure TSerializerIntrinsics.minsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MINSD, dst, src);
end;

procedure TSerializerIntrinsics.movdqa(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVDQA, dst, src);
end;

procedure TSerializerIntrinsics.movdqa(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVDQA, dst, src);
end;

procedure TSerializerIntrinsics.movdqa(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVDQA, dst, src);
end;

procedure TSerializerIntrinsics.movdqu(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVDQU, dst, src);
end;

procedure TSerializerIntrinsics.movdqu(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVDQU, dst, src);
end;

procedure TSerializerIntrinsics.movdqu(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVDQU, dst, src);
end;

procedure TSerializerIntrinsics.movmskps(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVMSKPS, dst, src);
end;

procedure TSerializerIntrinsics.movmskpd(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVMSKPD, dst, src);
end;

procedure TSerializerIntrinsics.movsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVSD, dst, src);
end;

procedure TSerializerIntrinsics.movsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVSD, dst, src);
end;

procedure TSerializerIntrinsics.movsd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVSD, dst, src);
end;

procedure TSerializerIntrinsics.movapd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVAPD, dst, src);
end;

procedure TSerializerIntrinsics.movapd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVAPD, dst, src);
end;

procedure TSerializerIntrinsics.movapd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVAPD, dst, src);
end;

procedure TSerializerIntrinsics.movdq2q(dst: TMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVDQ2Q, dst, src);
end;

procedure TSerializerIntrinsics.movq2dq(dst: TXMMRegister; src: TMMRegister);
begin
  emitX86(INST_MOVQ2DQ, dst, src);
end;

procedure TSerializerIntrinsics.movhpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVHPD, dst, src);
end;

procedure TSerializerIntrinsics.movhpd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVHPD, dst, src);
end;

procedure TSerializerIntrinsics.movlpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVLPD, dst, src);
end;

procedure TSerializerIntrinsics.movlpd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVLPD, dst, src);
end;

procedure TSerializerIntrinsics.movntdq(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVNTDQ, dst, src);
end;

procedure TSerializerIntrinsics.movnti(dst: TMem; src: TRegister);
begin
  emitX86(INST_MOVNTI, dst, src);
end;

procedure TSerializerIntrinsics.movntpd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVNTPD, dst, src);
end;

procedure TSerializerIntrinsics.movupd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVUPD, dst, src);
end;

procedure TSerializerIntrinsics.movupd(dst: TMem; src: TXMMRegister);
begin
  emitX86(INST_MOVUPD, dst, src);
end;

procedure TSerializerIntrinsics.mulpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MULPD, dst, src);
end;

procedure TSerializerIntrinsics.mulpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MULPD, dst, src);
end;

procedure TSerializerIntrinsics.mulsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MULSD, dst, src);
end;

procedure TSerializerIntrinsics.mulsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MULSD, dst, src);
end;

procedure TSerializerIntrinsics.orpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ORPD, dst, src);
end;

procedure TSerializerIntrinsics.orpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ORPD, dst, src);
end;

procedure TSerializerIntrinsics.packsswb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PACKSSWB, dst, src);
end;

procedure TSerializerIntrinsics.packsswb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PACKSSWB, dst, src);
end;

procedure TSerializerIntrinsics.packssdw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PACKSSDW, dst, src);
end;

procedure TSerializerIntrinsics.packssdw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PACKSSDW, dst, src);
end;

procedure TSerializerIntrinsics.packuswb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PACKUSWB, dst, src);
end;

procedure TSerializerIntrinsics.packuswb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PACKUSWB, dst, src);
end;

procedure TSerializerIntrinsics.paddb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDB, dst, src);
end;

procedure TSerializerIntrinsics.paddb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDB, dst, src);
end;

procedure TSerializerIntrinsics.paddw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDW, dst, src);
end;

procedure TSerializerIntrinsics.paddw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDW, dst, src);
end;

procedure TSerializerIntrinsics.paddd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDD, dst, src);
end;

procedure TSerializerIntrinsics.paddd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDD, dst, src);
end;

procedure TSerializerIntrinsics.paddq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PADDQ, dst, src);
end;

procedure TSerializerIntrinsics.paddq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PADDQ, dst, src);
end;

procedure TSerializerIntrinsics.paddq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDQ, dst, src);
end;

procedure TSerializerIntrinsics.paddq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDQ, dst, src);
end;

procedure TSerializerIntrinsics.paddsb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDSB, dst, src);
end;

procedure TSerializerIntrinsics.paddsb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDSB, dst, src);
end;

procedure TSerializerIntrinsics.paddsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDSW, dst, src);
end;

procedure TSerializerIntrinsics.paddsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDSW, dst, src);
end;

procedure TSerializerIntrinsics.paddusb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDUSB, dst, src);
end;

procedure TSerializerIntrinsics.paddusb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDUSB, dst, src);
end;

procedure TSerializerIntrinsics.paddusw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PADDUSW, dst, src);
end;

procedure TSerializerIntrinsics.paddusw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PADDUSW, dst, src);
end;

procedure TSerializerIntrinsics.pand(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PAND, dst, src);
end;

procedure TSerializerIntrinsics.pand(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PAND, dst, src);
end;

procedure TSerializerIntrinsics.pandn(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PANDN, dst, src);
end;

procedure TSerializerIntrinsics.pandn(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PANDN, dst, src);
end;

procedure TSerializerIntrinsics.pause;
begin
  emitX86(INST_PAUSE);
end;

procedure TSerializerIntrinsics.pavgb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PAVGB, dst, src);
end;

procedure TSerializerIntrinsics.pavgb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PAVGB, dst, src);
end;

procedure TSerializerIntrinsics.pavgw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PAVGW, dst, src);
end;

procedure TSerializerIntrinsics.pavgw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PAVGW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPEQB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPEQW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPEQD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPGTB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTB, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPGTW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTW, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPGTD, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTD, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxub(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXUB, dst, src);
end;

procedure TSerializerIntrinsics.pmaxub(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXUB, dst, src);
end;

procedure TSerializerIntrinsics.pminsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINSW, dst, src);
end;

procedure TSerializerIntrinsics.pminsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINSW, dst, src);
end;

procedure TSerializerIntrinsics.pminub(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINUB, dst, src);
end;

procedure TSerializerIntrinsics.pminub(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINUB, dst, src);
end;

procedure TSerializerIntrinsics.pmovmskb(dst: TRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVMSKB, dst, src);
end;

procedure TSerializerIntrinsics.pmulhw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULHW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULHW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhuw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULHUW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhuw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULHUW, dst, src);
end;

procedure TSerializerIntrinsics.pmullw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULLW, dst, src);
end;

procedure TSerializerIntrinsics.pmullw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULLW, dst, src);
end;

procedure TSerializerIntrinsics.pmuludq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMULUDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmuludq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMULUDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmuludq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULUDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmuludq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULUDQ, dst, src);
end;

procedure TSerializerIntrinsics.por(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_POR, dst, src);
end;

procedure TSerializerIntrinsics.por(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_POR, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.pslld(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLD, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllq(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLQ, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.psllw(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLW, dst, src);
end;

procedure TSerializerIntrinsics.pslldq(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSLLDQ, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psrad(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRAD, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psraw(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRAW, dst, src);
end;

procedure TSerializerIntrinsics.psubb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBB, dst, src);
end;

procedure TSerializerIntrinsics.psubb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBB, dst, src);
end;

procedure TSerializerIntrinsics.psubw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBW, dst, src);
end;

procedure TSerializerIntrinsics.psubw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBW, dst, src);
end;

procedure TSerializerIntrinsics.psubd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBD, dst, src);
end;

procedure TSerializerIntrinsics.psubd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBD, dst, src);
end;

procedure TSerializerIntrinsics.psubq(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSUBQ, dst, src);
end;

procedure TSerializerIntrinsics.psubq(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSUBQ, dst, src);
end;

procedure TSerializerIntrinsics.psubq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBQ, dst, src);
end;

procedure TSerializerIntrinsics.psubq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBQ, dst, src);
end;

procedure TSerializerIntrinsics.pmaddwd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMADDWD, dst, src);
end;

procedure TSerializerIntrinsics.pmaddwd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMADDWD, dst, src);
end;

procedure TSerializerIntrinsics.pshufd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PSHUFD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshufd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PSHUFD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshufhw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PSHUFHW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshufhw(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PSHUFHW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshuflw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PSHUFLW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pshuflw(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PSHUFLW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.psrld(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrld(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrld(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLD, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlq(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLQ, dst, src);
end;

procedure TSerializerIntrinsics.psrldq(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLDQ, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psrlw(dst: TXMMRegister; src: TImmediate);
begin
  emitX86(INST_PSRLW, dst, src);
end;

procedure TSerializerIntrinsics.psubsb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBSB, dst, src);
end;

procedure TSerializerIntrinsics.psubsb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBSB, dst, src);
end;

procedure TSerializerIntrinsics.psubsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.psubsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.psubusb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBUSB, dst, src);
end;

procedure TSerializerIntrinsics.psubusb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBUSB, dst, src);
end;

procedure TSerializerIntrinsics.psubusw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSUBUSW, dst, src);
end;

procedure TSerializerIntrinsics.psubusw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSUBUSW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhbw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKHBW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhbw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHBW, dst, src);
end;

procedure TSerializerIntrinsics.punpckhwd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKHWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckhwd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckhdq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKHDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckhdq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckhqdq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKHQDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckhqdq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKHQDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpcklbw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKLBW, dst, src);
end;

procedure TSerializerIntrinsics.punpcklbw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLBW, dst, src);
end;

procedure TSerializerIntrinsics.punpcklwd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKLWD, dst, src);
end;

procedure TSerializerIntrinsics.punpcklwd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLWD, dst, src);
end;

procedure TSerializerIntrinsics.punpckldq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKLDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpckldq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpcklqdq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PUNPCKLQDQ, dst, src);
end;

procedure TSerializerIntrinsics.punpcklqdq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PUNPCKLQDQ, dst, src);
end;

procedure TSerializerIntrinsics.pxor(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PXOR, dst, src);
end;

procedure TSerializerIntrinsics.pxor(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PXOR, dst, src);
end;

procedure TSerializerIntrinsics.shufpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_SHUFPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.shufpd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_SHUFPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.sqrtpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SQRTPD, dst, src);
end;

procedure TSerializerIntrinsics.sqrtpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SQRTPD, dst, src);
end;

procedure TSerializerIntrinsics.sqrtsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SQRTSD, dst, src);
end;

procedure TSerializerIntrinsics.sqrtsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SQRTSD, dst, src);
end;

procedure TSerializerIntrinsics.subpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SUBPD, dst, src);
end;

procedure TSerializerIntrinsics.subpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SUBPD, dst, src);
end;

procedure TSerializerIntrinsics.subsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_SUBSD, dst, src);
end;

procedure TSerializerIntrinsics.subsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_SUBSD, dst, src);
end;

procedure TSerializerIntrinsics.ucomisd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UCOMISD, dst, src);
end;

procedure TSerializerIntrinsics.ucomisd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UCOMISD, dst, src);
end;

procedure TSerializerIntrinsics.unpckhpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UNPCKHPD, dst, src);
end;

procedure TSerializerIntrinsics.unpckhpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UNPCKHPD, dst, src);
end;

procedure TSerializerIntrinsics.unpcklpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_UNPCKLPD, dst, src);
end;

procedure TSerializerIntrinsics.unpcklpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_UNPCKLPD, dst, src);
end;

procedure TSerializerIntrinsics.xorpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_XORPD, dst, src);
end;

procedure TSerializerIntrinsics.xorpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_XORPD, dst, src);
end;

procedure TSerializerIntrinsics.addsubpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDSUBPD, dst, src);
end;

procedure TSerializerIntrinsics.addsubpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDSUBPD, dst, src);
end;

procedure TSerializerIntrinsics.addsubps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_ADDSUBPS, dst, src);
end;

procedure TSerializerIntrinsics.addsubps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_ADDSUBPS, dst, src);
end;

procedure TSerializerIntrinsics.fisttp(dst: TMem);
begin
  emitX86(INST_FISTTP, dst);
end;

procedure TSerializerIntrinsics.haddpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_HADDPD, dst, src);
end;

procedure TSerializerIntrinsics.haddpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_HADDPD, dst, src);
end;

procedure TSerializerIntrinsics.haddps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_HADDPS, dst, src);
end;

procedure TSerializerIntrinsics.haddps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_HADDPS, dst, src);
end;

procedure TSerializerIntrinsics.hsubpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_HSUBPD, dst, src);
end;

procedure TSerializerIntrinsics.hsubpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_HSUBPD, dst, src);
end;

procedure TSerializerIntrinsics.hsubps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_HSUBPS, dst, src);
end;

procedure TSerializerIntrinsics.hsubps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_HSUBPS, dst, src);
end;

procedure TSerializerIntrinsics.lddqu(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_LDDQU, dst, src);
end;

procedure TSerializerIntrinsics.monitor;
begin
  emitX86(INST_MONITOR);
end;

procedure TSerializerIntrinsics.movddup(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVDDUP, dst, src);
end;

procedure TSerializerIntrinsics.movddup(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVDDUP, dst, src);
end;

procedure TSerializerIntrinsics.movshdup(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVSHDUP, dst, src);
end;

procedure TSerializerIntrinsics.movshdup(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVSHDUP, dst, src);
end;

procedure TSerializerIntrinsics.movsldup(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_MOVSLDUP, dst, src);
end;

procedure TSerializerIntrinsics.movsldup(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVSLDUP, dst, src);
end;

procedure TSerializerIntrinsics.mwait;
begin
  emitX86(INST_MWAIT);
end;

procedure TSerializerIntrinsics.psignb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSIGNB, dst, src);
end;

procedure TSerializerIntrinsics.psignb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSIGNB, dst, src);
end;

procedure TSerializerIntrinsics.psignb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSIGNB, dst, src);
end;

procedure TSerializerIntrinsics.psignb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSIGNB, dst, src);
end;

procedure TSerializerIntrinsics.psignw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSIGNW, dst, src);
end;

procedure TSerializerIntrinsics.psignw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSIGNW, dst, src);
end;

procedure TSerializerIntrinsics.psignw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSIGNW, dst, src);
end;

procedure TSerializerIntrinsics.psignw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSIGNW, dst, src);
end;

procedure TSerializerIntrinsics.psignd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSIGND, dst, src);
end;

procedure TSerializerIntrinsics.psignd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSIGND, dst, src);
end;

procedure TSerializerIntrinsics.psignd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSIGND, dst, src);
end;

procedure TSerializerIntrinsics.psignd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSIGND, dst, src);
end;

procedure TSerializerIntrinsics.phaddw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHADDW, dst, src);
end;

procedure TSerializerIntrinsics.phaddw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHADDW, dst, src);
end;

procedure TSerializerIntrinsics.phaddw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHADDW, dst, src);
end;

procedure TSerializerIntrinsics.phaddw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHADDW, dst, src);
end;

procedure TSerializerIntrinsics.phaddd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHADDD, dst, src);
end;

procedure TSerializerIntrinsics.phaddd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHADDD, dst, src);
end;

procedure TSerializerIntrinsics.phaddd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHADDD, dst, src);
end;

procedure TSerializerIntrinsics.phaddd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHADDD, dst, src);
end;

procedure TSerializerIntrinsics.phaddsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHADDSW, dst, src);
end;

procedure TSerializerIntrinsics.phaddsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHADDSW, dst, src);
end;

procedure TSerializerIntrinsics.phaddsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHADDSW, dst, src);
end;

procedure TSerializerIntrinsics.phaddsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHADDSW, dst, src);
end;

procedure TSerializerIntrinsics.phsubw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHSUBW, dst, src);
end;

procedure TSerializerIntrinsics.phsubw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBW, dst, src);
end;

procedure TSerializerIntrinsics.phsubw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHSUBW, dst, src);
end;

procedure TSerializerIntrinsics.phsubw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBW, dst, src);
end;

procedure TSerializerIntrinsics.phsubd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHSUBD, dst, src);
end;

procedure TSerializerIntrinsics.phsubd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBD, dst, src);
end;

procedure TSerializerIntrinsics.phsubd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHSUBD, dst, src);
end;

procedure TSerializerIntrinsics.phsubd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBD, dst, src);
end;

procedure TSerializerIntrinsics.phsubsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PHSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.phsubsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.phsubsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.phsubsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHSUBSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaddubsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMADDUBSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaddubsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMADDUBSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaddubsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMADDUBSW, dst, src);
end;

procedure TSerializerIntrinsics.pmaddubsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMADDUBSW, dst, src);
end;

procedure TSerializerIntrinsics.pabsb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PABSB, dst, src);
end;

procedure TSerializerIntrinsics.pabsb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PABSB, dst, src);
end;

procedure TSerializerIntrinsics.pabsb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PABSB, dst, src);
end;

procedure TSerializerIntrinsics.pabsb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PABSB, dst, src);
end;

procedure TSerializerIntrinsics.pabsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PABSW, dst, src);
end;

procedure TSerializerIntrinsics.pabsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PABSW, dst, src);
end;

procedure TSerializerIntrinsics.pabsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PABSW, dst, src);
end;

procedure TSerializerIntrinsics.pabsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PABSW, dst, src);
end;

procedure TSerializerIntrinsics.pabsd(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PABSD, dst, src);
end;

procedure TSerializerIntrinsics.pabsd(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PABSD, dst, src);
end;

procedure TSerializerIntrinsics.pabsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PABSD, dst, src);
end;

procedure TSerializerIntrinsics.pabsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PABSD, dst, src);
end;

procedure TSerializerIntrinsics.pmulhrsw(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PMULHRSW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhrsw(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PMULHRSW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhrsw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULHRSW, dst, src);
end;

procedure TSerializerIntrinsics.pmulhrsw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULHRSW, dst, src);
end;

procedure TSerializerIntrinsics.pshufb(dst: TMMRegister; src: TMMRegister);
begin
  emitX86(INST_PSHUFB, dst, src);
end;

procedure TSerializerIntrinsics.pshufb(dst: TMMRegister; src: TMem);
begin
  emitX86(INST_PSHUFB, dst, src);
end;

procedure TSerializerIntrinsics.pshufb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PSHUFB, dst, src);
end;

procedure TSerializerIntrinsics.pshufb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PSHUFB, dst, src);
end;

procedure TSerializerIntrinsics.palignr(dst: TMMRegister; src: TMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PALIGNR, dst, src, imm8);
end;

procedure TSerializerIntrinsics.palignr(dst: TMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PALIGNR, dst, src, imm8);
end;

procedure TSerializerIntrinsics.palignr(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PALIGNR, dst, src, imm8);
end;

procedure TSerializerIntrinsics.palignr(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PALIGNR, dst, src, imm8);
end;

procedure TSerializerIntrinsics.blendpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_BLENDPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.blendpd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_BLENDPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.blendps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_BLENDPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.blendps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_BLENDPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.blendvpd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_BLENDVPD, dst, src);
end;

procedure TSerializerIntrinsics.blendvpd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_BLENDVPD, dst, src);
end;

procedure TSerializerIntrinsics.blendvps(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_BLENDVPS, dst, src);
end;

procedure TSerializerIntrinsics.blendvps(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_BLENDVPS, dst, src);
end;

procedure TSerializerIntrinsics.dppd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_DPPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.dppd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_DPPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.dpps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_DPPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.dpps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_DPPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.extractps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_EXTRACTPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.extractps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_EXTRACTPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.movntdqa(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_MOVNTDQA, dst, src);
end;

procedure TSerializerIntrinsics.mpsadbw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_MPSADBW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.mpsadbw(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_MPSADBW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.packusdw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PACKUSDW, dst, src);
end;

procedure TSerializerIntrinsics.packusdw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PACKUSDW, dst, src);
end;

procedure TSerializerIntrinsics.pblendvb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PBLENDVB, dst, src);
end;

procedure TSerializerIntrinsics.pblendvb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PBLENDVB, dst, src);
end;

procedure TSerializerIntrinsics.pblendw(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PBLENDW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pblendw(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PBLENDW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpeqq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPEQQ, dst, src);
end;

procedure TSerializerIntrinsics.pcmpeqq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPEQQ, dst, src);
end;

procedure TSerializerIntrinsics.pextrb(dst: TRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRB, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrb(dst: TMem; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRB, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrd(dst: TRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrd(dst: TMem; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrq(dst: TRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRQ, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrq(dst: TMem; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRQ, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrw(dst: TRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pextrw(dst: TMem; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PEXTRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.phminposuw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PHMINPOSUW, dst, src);
end;

procedure TSerializerIntrinsics.phminposuw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PHMINPOSUW, dst, src);
end;

procedure TSerializerIntrinsics.pinsrb(dst: TXMMRegister; src: TRegister; imm8: TImmediate);
begin
  emitX86(INST_PINSRB, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrb(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PINSRB, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrd(dst: TXMMRegister; src: TRegister; imm8: TImmediate);
begin
  emitX86(INST_PINSRD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PINSRD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrq(dst: TXMMRegister; src: TRegister; imm8: TImmediate);
begin
  emitX86(INST_PINSRQ, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrq(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PINSRQ, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrw(dst: TXMMRegister; src: TRegister; imm8: TImmediate);
begin
  emitX86(INST_PINSRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pinsrw(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PINSRW, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pmaxuw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXUW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxuw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXUW, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXSB, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXSB, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXSD, dst, src);
end;

procedure TSerializerIntrinsics.pmaxsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXSD, dst, src);
end;

procedure TSerializerIntrinsics.pmaxud(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMAXUD, dst, src);
end;

procedure TSerializerIntrinsics.pmaxud(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMAXUD, dst, src);
end;

procedure TSerializerIntrinsics.pminsb(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINSB, dst, src);
end;

procedure TSerializerIntrinsics.pminsb(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINSB, dst, src);
end;

procedure TSerializerIntrinsics.pminuw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINUW, dst, src);
end;

procedure TSerializerIntrinsics.pminuw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINUW, dst, src);
end;

procedure TSerializerIntrinsics.pminud(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINUD, dst, src);
end;

procedure TSerializerIntrinsics.pminud(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINUD, dst, src);
end;

procedure TSerializerIntrinsics.pminsd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMINSD, dst, src);
end;

procedure TSerializerIntrinsics.pminsd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMINSD, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXBW, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXBW, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXBD, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXBD, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXBQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxbq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXBQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxwd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXWD, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxwd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXWD, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxwq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXWQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxwq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXWQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxdq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVSXDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovsxdq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVSXDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbw(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXBW, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbw(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXBW, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXBD, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXBD, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXBQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxbq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXBQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxwd(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXWD, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxwd(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXWD, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxwq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXWQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxwq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXWQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxdq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMOVZXDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmovzxdq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMOVZXDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmuldq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmuldq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULDQ, dst, src);
end;

procedure TSerializerIntrinsics.pmulld(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PMULLD, dst, src);
end;

procedure TSerializerIntrinsics.pmulld(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PMULLD, dst, src);
end;

procedure TSerializerIntrinsics.ptest(op1, op2: TXMMRegister);
begin
  emitX86(INST_PTEST, op1, op2);
end;

procedure TSerializerIntrinsics.ptest(op1: TXMMRegister; op2: TMem);
begin
  emitX86(INST_PTEST, op1, op2);
end;

procedure TSerializerIntrinsics.roundps(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_ROUNDPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundps(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_ROUNDPS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundss(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_ROUNDSS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundss(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_ROUNDSS, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundpd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_ROUNDPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundpd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_ROUNDPD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundsd(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_ROUNDSD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.roundsd(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_ROUNDSD, dst, src, imm8);
end;

procedure TSerializerIntrinsics.crc32(dst: TRegister; src: TRegister);
begin
  Assert(dst.isRegType(REG_GPD) or dst.isRegType(REG_GPQ));
  emitX86(INST_CRC32, dst, src);
end;

procedure TSerializerIntrinsics.crc32(dst: TRegister; src: TMem);
begin
  Assert(dst.isRegType(REG_GPD) or dst.isRegType(REG_GPQ));
  emitX86(INST_CRC32, dst, src);
end;

procedure TSerializerIntrinsics.pcmpestri(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PCMPESTRI, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpestri(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PCMPESTRI, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpestrm(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PCMPESTRM, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpestrm(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PCMPESTRM, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpistri(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PCMPISTRI, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpistri(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PCMPISTRI, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpistrm(dst: TXMMRegister; src: TXMMRegister; imm8: TImmediate);
begin
  emitX86(INST_PCMPISTRM, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpistrm(dst: TXMMRegister; src: TMem; imm8: TImmediate);
begin
  emitX86(INST_PCMPISTRM, dst, src, imm8);
end;

procedure TSerializerIntrinsics.pcmpgtq(dst: TXMMRegister; src: TXMMRegister);
begin
  emitX86(INST_PCMPGTQ, dst, src);
end;

procedure TSerializerIntrinsics.pcmpgtq(dst: TXMMRegister; src: TMem);
begin
  emitX86(INST_PCMPGTQ, dst, src);
end;

procedure TSerializerIntrinsics.popcnt(dst: TRegister; src: TRegister);
begin
  Assert(not dst.isRegType(REG_GPB));
  Assert(src.typ = dst.typ);
  emitX86(INST_POPCNT, dst, src);
end;

procedure TSerializerIntrinsics.popcnt(dst: TRegister; src: TMem);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_POPCNT, dst, src);
end;

procedure TSerializerIntrinsics.amd_prefetch(mem: TMem);
begin
  emitX86(INST_AMD_PREFETCH, mem);
end;

procedure TSerializerIntrinsics.amd_prefetchw(mem: TMem);
begin
  emitX86(INST_AMD_PREFETCHW, mem);
end;

procedure TSerializerIntrinsics.movbe(dst: TRegister; src: TMem);
begin
  Assert(not dst.isRegType(REG_GPB));
  emitX86(INST_MOVBE, dst, src);
end;

procedure TSerializerIntrinsics.movbe(dst: TMem; src: TRegister);
begin
  Assert(not src.isRegType(REG_GPB));
  emitX86(INST_MOVBE, dst, src);
end;

initialization
  reg_none.Create(_Init, NO_REG);
  al.Create(_Init, REG_AL);
  cl.Create(_Init, REG_CL);
  dl.Create(_Init, REG_DL);
  bl.Create(_Init, REG_BL);
  ah.Create(_Init, REG_AH);
  ch.Create(_Init, REG_CH);
  dh.Create(_Init, REG_DH);
  bh.Create(_Init, REG_BH);
{$IFDEF ASMJIT_X64}
  r8b.Create(_Init, REG_R8B);
  r9b.Create(_Init, REG_R9B);
  r10b.Create(_Init, REG_R10B);
  r11b.Create(_Init, REG_R11B);
  r12b.Create(_Init, REG_R12B);
  r13b.Create(_Init, REG_R13B);
  r14b.Create(_Init, REG_R14B);
  r15b.Create(_Init, REG_R15B);
{$ENDIF}
  ax.Create(_Init, REG_AX);
  cx.Create(_Init, REG_CX);
  dx.Create(_Init, REG_DX);
  bx.Create(_Init, REG_BX);
  sp.Create(_Init, REG_SP);
  bp.Create(_Init, REG_BP);
  si.Create(_Init, REG_SI);
  di.Create(_Init, REG_DI);
{$IFDEF ASMJIT_X64}
  r8w.Create(_Init, REG_R8W);
  r9w.Create(_Init, REG_R9W);
  r10w.Create(_Init, REG_R10W);
  r11w.Create(_Init, REG_R11W);
  r12w.Create(_Init, REG_R12W);
  r13w.Create(_Init, REG_R13W);
  r14w.Create(_Init, REG_R14W);
  r15w.Create(_Init, REG_R15W);
{$ENDIF}
  eax.Create(_Init, REG_EAX);
  ecx.Create(_Init, REG_ECX);
  edx.Create(_Init, REG_EDX);
  ebx.Create(_Init, REG_EBX);
  esp.Create(_Init, REG_ESP);
  ebp.Create(_Init, REG_EBP);
  esi.Create(_Init, REG_ESI);
  edi.Create(_Init, REG_EDI);
{$IFDEF ASMJIT_X64}
  rax.Create(_Init, REG_RAX);
  rcx.Create(_Init, REG_RCX);
  rdx.Create(_Init, REG_RDX);
  rbx.Create(_Init, REG_RBX);
  rsp.Create(_Init, REG_RSP);
  rbp.Create(_Init, REG_RBP);
  rsi.Create(_Init, REG_RSI);
  rdi.Create(_Init, REG_RDI);
  r8.Create(_Init, REG_R8);
  r9.Create(_Init, REG_R9);
  r10.Create(_Init, REG_R10);
  r11.Create(_Init, REG_R11);
  r12.Create(_Init, REG_R12);
  r13.Create(_Init, REG_R13);
  r14.Create(_Init, REG_R14);
  r15.Create(_Init, REG_R15);
{$ENDIF}
  nax.Create(_Init, REG_NAX);
  ncx.Create(_Init, REG_NCX);
  ndx.Create(_Init, REG_NDX);
  nbx.Create(_Init, REG_NBX);
  nsp.Create(_Init, REG_NSP);
  nbp.Create(_Init, REG_NBP);
  nsi.Create(_Init, REG_NSI);
  ndi.Create(_Init, REG_NDI);
  mm0.Create(_Init, REG_MM0);
  mm1.Create(_Init, REG_MM1);
  mm2.Create(_Init, REG_MM2);
  mm3.Create(_Init, REG_MM3);
  mm4.Create(_Init, REG_MM4);
  mm5.Create(_Init, REG_MM5);
  mm6.Create(_Init, REG_MM6);
  mm7.Create(_Init, REG_MM7);
  xmm0.Create(_Init, REG_XMM0);
  xmm1.Create(_Init, REG_XMM1);
  xmm2.Create(_Init, REG_XMM2);
  xmm3.Create(_Init, REG_XMM3);
  xmm4.Create(_Init, REG_XMM4);
  xmm5.Create(_Init, REG_XMM5);
  xmm6.Create(_Init, REG_XMM6);
  xmm7.Create(_Init, REG_XMM7);
{$IFDEF ASMJIT_X64}
  xmm8.Create(_Init, REG_XMM8);
  xmm9.Create(_Init, REG_XMM9);
  xmm10.Create(_Init, REG_XMM10);
  xmm11.Create(_Init, REG_XMM11);
  xmm12.Create(_Init, REG_XMM12);
  xmm13.Create(_Init, REG_XMM13);
  xmm14.Create(_Init, REG_XMM14);
  xmm15.Create(_Init, REG_XMM15);
{$ENDIF}

finalization

end.
