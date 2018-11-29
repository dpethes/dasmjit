unit DAsmJit_Compiler;

{$I DAsmJit.inc}

interface

uses
  DAsmJit, DAsmJit_Serializer, DAsmJit_Util, DAsmJit_Assembler, DAsmJit_Defs, DAsmJit_MemoryManager;

//type
const
  //TEMITTABLE_TYPE_E = (
    EMITTABLE_NONE = 0;
    EMITTABLE_COMMENT = 1;
    EMITTABLE_EMBEDDED_DATA = 2;
    EMITTABLE_ALIGN = 3;
    EMITTABLE_INSTRUCTION = 4;
    EMITTABLE_BLOCK = 5;
    EMITTABLE_FUNCTION = 6;
    EMITTABLE_PROLOGUE = 7;
    EMITTABLE_EPILOGUE = 8;
    EMITTABLE_TARGET = 9;
    EMITTABLE_JUMP_TABLE = 10;
  //);

  //TVARIABLE_STATE_E = (
    VARIABLE_STATE_UNUSED = 0;
    VARIABLE_STATE_REGISTER = 1;
    VARIABLE_STATE_MEMORY = 2;
  //);

  //TVARIABLE_ALLOC_E = (
    VARIABLE_ALLOC_READ = 1;
    VARIABLE_ALLOC_WRITE = 2;
    VARIABLE_ALLOC_READWRITE = 3;
  //);

  //TALLOC_POLICY_E = (
    ALLOC_POLICY_PRESERVED_FIRST = 0;
    ALLOC_POLICY_PRESERVED_LAST = 1;
  //);

  //TARGUMENT_DIR_E = (
    ARGUMENT_DIR_LEFT_TO_RIGHT = 0;
    ARGUMENT_DIR_RIGHT_TO_LEFT = 1;
  //);

  //TCALL_CONV_E = (
    CALL_CONV_NONE = 0;
    CALL_CONV_X64W = 1;
    CALL_CONV_X64U = 2;
    CALL_CONV_CDECL = 3;
    CALL_CONV_STDCALL = 4;
    CALL_CONV_MSTHISCALL = 5;
    CALL_CONV_MSFASTCALL = 6;
    CALL_CONV_BORLANDFASTCALL = 7;
    CALL_CONV_GCCFASTCALL_2 = 8;
    CALL_CONV_GCCFASTCALL_3 = 9;
  //);

//const
  MAX_VARIABLE_LENGTH = 32;

{$IFDEF ASMJIT_X86}
  CALL_CONV_DEFAULT = CALL_CONV_BORLANDFASTCALL;
{$ELSE}
  {$IFDEF WINDOWS}
  CALL_CONV_DEFAULT = CALL_CONV_X64W;
  {$ELSE}
  CALL_CONV_DEFAULT = CALL_CONV_X64U;
  {$ENDIF}
{$ENDIF}

  VARIABLE_TYPE_NONE = 0;
  VARIABLE_TYPE_INT32 = 1;
  VARIABLE_TYPE_UINT32 = 1;
  VARIABLE_TYPE_INT64 = 2;
  VARIABLE_TYPE_UINT64 = 2;
{$IFDEF ASMJIT_X86}
  VARIABLE_TYPE_SYSINT = VARIABLE_TYPE_INT32;
  VARIABLE_TYPE_SYSUINT = VARIABLE_TYPE_UINT32;
{$ELSE}
  //VARIABLE_TYPE_INT64 = 2;
  //VARIABLE_TYPE_UINT64 = 2;
  VARIABLE_TYPE_SYSINT = VARIABLE_TYPE_INT64;
  VARIABLE_TYPE_SYSUINT = VARIABLE_TYPE_UINT64;
{$ENDIF}
  VARIABLE_TYPE_PTR = VARIABLE_TYPE_SYSUINT;
  VARIABLE_TYPE_X87_FLOAT = 3;
  VARIABLE_TYPE_X87_DOUBLE = 4;
  VARIABLE_TYPE_XMM_FLOAT = 5;
  VARIABLE_TYPE_XMM_DOUBLE = 6;
  VARIABLE_TYPE_XMM_FLOAT_4 = 7;
  VARIABLE_TYPE_XMM_DOUBLE_2 = 8;
{$IFDEF ASMJIT_X86}
  VARIABLE_TYPE_FLOAT = VARIABLE_TYPE_X87_FLOAT;
  VARIABLE_TYPE_DOUBLE = VARIABLE_TYPE_X87_DOUBLE;
{$ELSE}
  VARIABLE_TYPE_FLOAT = VARIABLE_TYPE_XMM_FLOAT;
  VARIABLE_TYPE_DOUBLE = VARIABLE_TYPE_XMM_DOUBLE;
{$ENDIF}
  VARIABLE_TYPE_MM = 9;
  VARIABLE_TYPE_XMM = 10;
  _VARIABLE_TYPE_COUNT = 11;

type
  PVariable = ^TVariable;
  TFunction = class;
  TCompiler = class;
  TAllocFn = procedure(v: PVariable);
  TSpillFn = procedure(v: PVariable);

  TVariable = object
  protected
    FCompiler: TCompiler;
    FFunction: TFunction;
    FRefCount: SysUInt;
    FSpillCount: SysUInt;
    FRegisterAccessCount: SysUInt;
    FMemoryAccessCount: SysUInt;
    FLifeId: SysUInt;
    FGlobalSpillCount: SysUInt;
    FGlobalRegisterAccessCount: SysUInt;
    FGlobalMemoryAccessCount: SysUInt;
    FType: UInt8;
    FSize: UInt8;
    FState: UInt8;
    FPriority: UInt8;
    FRegisterCode: UInt8;
    FPreferredRegisterCode: UInt8;
    FHomeRegisterCode: UInt8;
    FChanged: Boolean;
    FReusable: Boolean;
    FCustomMemoryHome: Boolean;
    FStackArgument: Boolean;
    FStackOffset: SysInt;
    FMemoryOperand: TMem;
    FAllocFn: TAllocFn;
    FSpillFn: TSpillFn;
    FDataPtr: Pointer;
    FDataInt: SysInt;
    FName: string{[MAX_VARIABLE_LENGTH]};
  public
    constructor Create(c: TCompiler; f: TFunction; ATyp: UInt8);

    procedure setPriority(APriority: UInt8);
    procedure setMemoryHome(memoryHome: TMem);
    function ref: PVariable;
    procedure deref;
    function alloc(mode: UInt8 = VARIABLE_ALLOC_READWRITE; preferredRegister: UInt8 = NO_REG): Boolean;
    procedure getReg(mode, preferredRegister: UInt8; out dest: TBaseReg; regType: UInt8);
    function m: TMem;
    function spill: Boolean;
    procedure unuse;
    function isCustom: Boolean;
    procedure setAll(ATyp, ASize, Astate, APriority, ARegisterCode, APreferredRegisterCode: UInt8; AStackOffset: SysInt);

    property Compiler: TCompiler read FCompiler;
    property Func: TFunction read FFunction;
    property RefCount: SysUInt read FRefCount;
    property SpillCount: SysUInt read FSpillCount;
    property RegisterAccessCount: SysUInt read FRegisterAccessCount;
    property MemoryAccessCount: SysUInt read FMemoryAccessCount;
    property LifeId: SysUInt read FLifeId;
    property Typ: UInt8 read FType;
    property Size: UInt8 read FSize;
    property State: UInt8 read FState;
    property Priority: UInt8 read FPriority write setPriority;
    property RegisterCode: UInt8 read FRegisterCode;
    property PreferredRegisterCode: UInt8 read FPreferredRegisterCode write FPreferredRegisterCode;
    property HomeRegisterCode: UInt8 read FHomeRegisterCode;
    property Changed: Boolean read FChanged write FChanged;
    property Reusable: Boolean read FReusable;
    property CustomMemoryHome: Boolean read FCustomMemoryHome;
    property StackArgument: Boolean read FStackArgument;
    property StackOffset: SysInt read FStackOffset write FStackOffset;
    property MemoryOperand: TMem read FMemoryOperand write setMemoryHome;
    property AllocFn: TAllocFn read FAllocFn write FAllocFn;
    property SpillFn: TSpillFn read FSpillFn write FSpillFn;
    property DataPtr: Pointer read FDataPtr write FDataPtr;
    property DataInt: SysInt read FDataInt write FDataInt;
    property Name: string read FName write FName;
  end;

  TVariableRef = object
  protected
    Fv: PVariable;
  public
    constructor Create(AVar: PVariable = nil); overload;
    destructor Destroy;

    function Typ: UInt8;
    function Size: UInt8;
    function State: UInt8;
    procedure Use(AVar: PVariable);
    function Alloc(mode: UInt8 = VARIABLE_ALLOC_READWRITE; preferredRegister: UInt8 = NO_REG): Boolean;
    function Spill: Boolean;
    procedure Unuse;
    procedure DestroyVar;
    function preferredRegisterCode: UInt8;
    procedure setPreferredRegisterCode(Code: UInt8);
    function homeRegisterCode: UInt8;
    function priority: UInt8;
    procedure setPriority(APriority: UInt8);
    function Changed: Boolean;
    procedure setChanged(AChanged: Boolean);
    function reusable: Boolean;
    function customMemoryHome: Boolean;
    procedure setMemoryHome(memoryHome: TMem);
    function m: TMem;
    function ref: PVariable;
    function isCustom: Boolean;
    function AllocFn: TAllocFn;
    function SpillFn: TSpillFn;
    function DataPtr: Pointer;
    function DataInt: SysInt;
    procedure setAllocFn(fn: TAllocFn);
    procedure setSpillFn(fn: TSpillFn);
    procedure setDataPtr(data: Pointer);
    procedure setDataInt(data: SysInt);
    function Name: string;
    procedure setName(AName: string);
    procedure Assign(Other: TVariableRef);

    property v: PVariable read Fv;
  end;

  TInt32Ref = object(TVariableRef)
  public
    constructor Create(Other: TInt32Ref); overload;

    function r  (Pref: UInt8 = NO_REG): TRegister;
    function r8 (Pref: UInt8 = NO_REG): TRegister;
    function r16(Pref: UInt8 = NO_REG): TRegister;
    function r32(Pref: UInt8 = NO_REG): TRegister;
{$IFDEF ASMJIT_X64}
    function r64(Pref: UInt8 = NO_REG): TRegister;
{$ENDIF}

    function c  (Pref: UInt8 = NO_REG): TRegister;
    function c8 (Pref: UInt8 = NO_REG): TRegister;
    function c16(Pref: UInt8 = NO_REG): TRegister;
    function c32(Pref: UInt8 = NO_REG): TRegister;
{$IFDEF ASMJIT_X64}
    function c64(Pref: UInt8 = NO_REG): TRegister;
{$ENDIF}

    function x  (Pref: UInt8 = NO_REG): TRegister;
    function x8 (Pref: UInt8 = NO_REG): TRegister;
    function x16(Pref: UInt8 = NO_REG): TRegister;
    function x32(Pref: UInt8 = NO_REG): TRegister;
{$IFDEF ASMJIT_X64}
    function x64(Pref: UInt8 = NO_REG): TRegister;
{$ENDIF}
  end;

{$IFDEF ASMJIT_X64}
  TInt64Ref = object(TVariableRef)
  public
    constructor Create(Other: TInt64Ref); overload;

    function r  (Pref: UInt8 = NO_REG): TRegister;
    function r8 (Pref: UInt8 = NO_REG): TRegister;
    function r16(Pref: UInt8 = NO_REG): TRegister;
    function r32(Pref: UInt8 = NO_REG): TRegister;
    function r64(Pref: UInt8 = NO_REG): TRegister;

    function c  (Pref: UInt8 = NO_REG): TRegister;
    function c8 (Pref: UInt8 = NO_REG): TRegister;
    function c16(Pref: UInt8 = NO_REG): TRegister;
    function c32(Pref: UInt8 = NO_REG): TRegister;
    function c64(Pref: UInt8 = NO_REG): TRegister;

    function x  (Pref: UInt8 = NO_REG): TRegister;
    function x8 (Pref: UInt8 = NO_REG): TRegister;
    function x16(Pref: UInt8 = NO_REG): TRegister;
    function x32(Pref: UInt8 = NO_REG): TRegister;
    function x64(Pref: UInt8 = NO_REG): TRegister;
  end;
{$ENDIF}

  TMMRef = object(TVariableRef)
  public
    constructor Create(Other: TMMRef); overload;

    function r(Pref: UInt8 = NO_REG): TMMRegister;
    function c(Pref: UInt8 = NO_REG): TMMRegister;
    function x(Pref: UInt8 = NO_REG): TMMRegister;
  end;

  TXMMRef = object(TVariableRef)
  public
    constructor Create(Other: TXMMRef); overload;

    function r(Pref: UInt8 = NO_REG): TXMMRegister;
    function c(Pref: UInt8 = NO_REG): TXMMRegister;
    function x(Pref: UInt8 = NO_REG): TXMMRegister;
  end;

{$IFDEF ASMJIT_X86}
  TSysIntRef = TInt32Ref;
{$ELSE}
  TSysIntRef = TInt64Ref;
{$ENDIF}
  TPtrRef = TSysIntRef;

  PEntry = ^TEntry;
  TEntry = record
    v: PVariable;
    LifeID: UInt32;
    State: UInt8;
    Changed: Boolean;
  end;

  PData = ^TData;
  TData = record
    usedGpRegisters: UInt32;
    usedMmRegisters: UInt32;
    usedXmmRegisters: UInt32;
    case Integer of
      0: (regs: array[0..16+8+16-1] of TEntry);
      1: (gp: array[0..15] of TEntry;
          mm: array[0..7] of TEntry;
          xmm: array[0..15] of TEntry)
  end;

  PState = ^TState;
  TState = object
  protected
    FCompiler: TCompiler;
    FFunction: TFunction;
    FData: TData;

    procedure _clear;
    //procedure _save;
    //procedure _set;
  public
    constructor Create(c: TCompiler; f: TFunction);

    //class procedure saveFunctionState(dst: PData; f: TFunction);
  end;

  //PEmittable = ^TEmittable;
  TEmittable = class
  protected
    FCompiler: TCompiler;
    FNext: TEmittable;
    FPrev: TEmittable;
    FType: UInt32;
  public
    constructor Create(c: TCompiler; ATyp: UInt32);

    procedure Prepare; virtual;
    procedure Emit(a: TAssembler); virtual;
    procedure postEmit(a: TAssembler); virtual;

    property Compiler: TCompiler read FCompiler;
    property Prev: TEmittable read FPrev;
    property Next: TEmittable read FNext;
    property Typ: UInt32 read FType;
  end;

  //PComment = ^TComment;
  TComment = class(TEmittable)
  protected
    FStr: string;
  public
    constructor Create(c: TCompiler; AStr: string); reintroduce;
    procedure Emit(a: TAssembler); override;

    property Str: string read FStr;
  end;

  //PEmbeddedData = ^TEmbeddedData;
  TEmbeddedData = class(TEmittable)
  protected
    FSize: SysUInt;
    FCapacity: SysUInt;
    FData: array{[0..SizeOf(Pointer) - 1]} of UInt8;
  public
    constructor Create(c: TCompiler; ACapacity: SysUInt; AData: Pointer; ASize: SysUInt);
    procedure Emit(a: TAssembler); override;
    function Data: PUInt8;

    property Size: SysUInt read FSize;
    property Capacity: SysUInt read FCapacity;
  end;

  //PAlign = ^TAlign;
  TAlign = class(TEmittable)
  protected
    FSize: SysInt;
  public
    constructor Create(c: TCompiler; ASize: SysInt = 0); reintroduce;
    procedure Emit(a: TAssembler); override;

    property Size: SysInt read FSize write FSize;
  end;

  //PInstruction = ^TInstruction;
  TInstruction = class(TEmittable)
  protected
    FCode: UInt32;
    Fo: array[0..2] of POperand;
    Focache: array[0..2] of TOperand;
    FInlineComment: string;
  public
    constructor Create(c: TCompiler; ACode: UInt32; Ao1, Ao2, Ao3: POperand; inlineComment: string = '');

    procedure Emit(a: TAssembler); override;
    function o: PPointer;
    function o1: POperand;
    function o2: POperand;
    function o3: POperand;

    property Code: UInt32 read FCode write FCode;
    //property o1: TOperand index 0 read Fo;
    //property o2: TOperand index 1 read Fo;
    //property o3: TOperand index 2 read Fo;
  end;

  TStateData = record
    case Integer of
      0: (regs: array[0..16+8+16 - 1] of PVariable);
      1: (gp: array[0..15] of PVariable;
          mm: array[0..7] of PVariable;
          xmm: array [0..15] of PVariable);
  end;

  TFunction = class(TEmittable)
  protected
    FCConv: UInt32;
    FCalleePopsStack: Boolean;
    FNaked: Boolean;
    FAllocableEbp: Boolean;
    FPrologEpilogPushPop: Boolean;
    FEmms: Boolean;
    FSFence: Boolean;
    FLFence: Boolean;
    FOptimizedPrologEpilog: Boolean;
    FCConvArgumentsDirection: UInt32;
    FCConvArgumentsGp: array [0..15] of UInt32;
    FCConvArgumentsXmm: array [0..15] of UInt32;
    FCConvPreservedGp: UInt32;
    FCConvPreservedXmm: UInt32;
    FArgumentsCount: UInt32;
    FArgumentsStackSize: UInt32;
    FStackAlignmentSize: SysInt;
    FPrologEpilogStackSize: SysInt;
    FVariablesStackSize: SysInt;
    FUsedGpRegisters: UInt32;
    FUsedMmRegisters: UInt32;
    FUsedXmmRegisters: UInt32;
    FModifiedGpRegisters: UInt32;
    FModifiedMmRegisters: UInt32;
    FModifiedXmmRegisters: UInt32;
    FVariables: TPodVector;
    FPrevented: TPodVector;
    FUsePrevention: Boolean;
    FState: TStateData;
    FEntryLabel: TLabel;
    FPrologLabel: TLabel;
    FExitLabel: TLabel;

    procedure _setCallingConvention(ACConv: UInt32);
    procedure _setArguments(_args: PUInt32; Count: SysUInt);
    //class procedure _jmpAndRestore(c: TCompiler; Lbl: PLabel);
  public
    constructor Create(c: TCompiler);
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Emit(a: TAssembler); override;

    procedure setPrototype(ACConv: UInt32; args: PUInt32; Count: SysUInt);
    function CConvArgumentsGp: PUInt32;
    function CConvArgumentsXmm: PUInt32;
    function Argument(i: SysInt): PVariable;
    function NewVariable(ATyp: UInt8; APriority: UInt8 = 10; APreferredRegisterCode: UInt8 = NO_REG): PVariable;
    function Alloc(v: PVariable; Mode: UInt8 = VARIABLE_ALLOC_READWRITE; preferredRegisterCode: UInt8 = NO_REG): Boolean;
    function Spill(v: PVariable): Boolean;
    procedure Unuse(v: PVariable);
    procedure SpillAll;
    procedure SpillAllGp;
    procedure SpillAllMm;
    procedure SpillAllXmm;
    procedure _SpillAll(sStart, sEnd: SysUInt);
    procedure SpillRegister(reg: TBaseReg);
    function NumFreeGp: SysInt;
    function NumFreeMm: SysInt;
    function NumFreeXmm: SysInt;
    function isPrevented(v: PVariable): Boolean;
    procedure addPrevented(v: PVariable);
    procedure removePrevented(v: PVariable);
    procedure clearPrevented;
    function _getSpillCandidate(ATyp: UInt8): PVariable;
    procedure _AllocAs(v: PVariable; Mode: UInt8; Code: UInt32);
    procedure _AllocReg(Code: UInt8; v: PVariable);
    procedure _FreeReg(Code: UInt8);
    procedure _ExchangeGp(v: PVariable; Mode: UInt8; Other: PVariable);
    procedure _PostAlloc(v: PVariable; Mode: UInt8);
    procedure UseGpRegisters(Mask: UInt32);
    procedure UseMmRegisters(Mask: UInt32);
    procedure UseXmmRegisters(Mask: UInt32);
    procedure UnuseGpRegisters(Mask: UInt32);
    procedure UnuseMmRegisters(Mask: UInt32);
    procedure UnuseXmmRegisters(Mask: UInt32);
    procedure ModifyGpRegisters(Mask: UInt32);
    procedure ModifyMmRegisters(Mask: UInt32);
    procedure ModifyXmmRegisters(Mask: UInt32);
    function CountofGpRegistersToBeSaved: SysInt;
    function CountOfXmmRegistersToBeSaved: SysInt;
    function SaveState: TState;
    procedure RestoreState(s: TState);
    procedure SetState(s: TState);

    property AllocableEbp: Boolean read FAllocableEbp write FAllocableEbp;
    property PrologEpilogPushPop: Boolean read FPrologEpilogPushPop write FPrologEpilogPushPop;
    property Emms: Boolean read FEmms write FEmms;
    property SFence: Boolean read FSFence write FSFence;
    property LFence: Boolean read FLFence write FLFence;
    property OptimizedPrologEpilog: Boolean read FOptimizedPrologEpilog write FOptimizedPrologEpilog;
    property CConv: UInt32 read FCConv;
    property CalleePopsStack: Boolean read FCalleePopsStack;
    property Naked: Boolean read FNaked write FNaked;
    property CConvArgumentsDirection: UInt32 read FCConvArgumentsDirection;
    property CConvPreservedGp: UInt32 read FCConvPreservedGp;
    property CConvPreservedXmm: UInt32 read FCConvPreservedXmm;
    property StackAlignmentSize: SysInt read FStackAlignmentSize;
    property PrologEpilogStackSize: SysInt read FPrologEpilogStackSize;
    property VariablesStackSize: SysInt read FVariablesStackSize;
    property ArgumentsCount: UInt32 read FArgumentsCount;
    property ArgumentsStackSize: UInt32 read FArgumentsStackSize;
    property UsedGpRegisters: UInt32 read FUsedGpRegisters;
    property UsedMmRegisters: UInt32 read FUsedMmRegisters;
    property UsedXmmRegisters: UInt32 read FUsedXmmRegisters;
    property ModifiedGpRegisters: UInt32 read FModifiedGpRegisters;
    property ModifiedMmRegisters: UInt32 read FModifiedMmRegisters;
    property ModifiedXmmRegisters: UInt32 read FModifiedXmmRegisters;
    property EntryLabel: TLabel read FEntryLabel;
    property PrologLabel: TLabel read FPrologLabel;
    property ExitLabel: TLabel read FExitLabel;
  end;

  //PProlog = ^TProlog;
  TProlog = class(TEmittable)
  protected
    FFunction: TFunction;
    FLabel: PLabel;
  public
    constructor Create(c: TCompiler; f: TFunction);
    procedure Emit(a: TAssembler); override;

    property Func: TFunction read FFunction;
  end;

  //PEpilog = ^TEpilog;
  TEpilog = class(TEmittable)
  protected
    FFunction: TFunction;
    FLabel: PLabel;
  public
    constructor Create(c: TCompiler; f: TFunction);
    procedure Emit(a: TAssembler); override;

    property Func: TFunction read FFunction;
  end;

  //PTarget = ^TTarget;
  TTarget = class(TEmittable)
  protected
    FTarget: PLabel;
  public
    constructor Create(c: TCompiler; Target: PLabel);
    procedure Emit(a: TAssembler); override;
  end;

  //PJumpTable = ^TJumpTable;
  TJumpTable = class(TEmittable)
  protected
    FTarget: PLabel;
    FLabels: TPodVector;
  public
    constructor Create(c: TCompiler);
    destructor Destroy; override;

    procedure Emit(a: TAssembler); override;
    procedure postEmit(a: TAssembler); override;
    function AddLabel(ATarget: PLabel = nil; Pos: SysInt = -1): PLabel;

    property Target: PLabel read FTarget;
    property Labels: TPodVector read FLabels;
  end;

  //TVariableList = TPodVector<PVariable>;
  //TOperandList = TPodVector<POperand>;

  TCompilerCore = class(TSerializer)
  protected
    FFirst: TEmittable;
    FLast: TEmittable;
    FCurrent: TEmittable;
    FOperands: TPodVector;
    FCurrentFunction: TFunction;
    FLabelIdCounter: UInt32;
    FJumpTableLabel: TLabel;
    FJumpTableData: TPodVector;
    FInlineCommentBuffer: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure DoFree;
    procedure AddEmittable(Emittable: TEmittable);
    procedure RemoveEmittable(Emittable: TEmittable);
    function setCurrentEmittable(Current: TEmittable): TEmittable;
    procedure Comment(fmt: string; Args: array of const);
    function NewFunction_(CConv: UInt32; Args: PUInt32 = nil; Count: SysUInt = 0): TFunction;
    function EndFunction: TFunction;
    function NewProlog(f: TFunction): TProlog;
    function NewEpilog(f: TFunction): TEpilog;
    function Argument(i: SysInt): PVariable;
    function NewVariable(typ: UInt8; Priority: UInt8 = 10; PreferredRegister: UInt8 = NO_REG): PVariable;
    function Alloc(v: PVariable; Mode: UInt8 = VARIABLE_ALLOC_READWRITE; PreferredRegister: UInt8 = NO_REG): Boolean;
    function Spill(v: PVariable): Boolean;
    procedure Unuse(v: PVariable);
    procedure SpillAll;
    procedure SpillAllGp;
    procedure SpillAllMm;
    procedure SpillAllXmm;
    procedure SpillRegister(reg: TBaseReg);
    function NumFreeGp: SysInt;
    function NumFreeMm: SysInt;
    function NumFreeXmm: SysInt;
    function isPrevented(v: PVariable): Boolean;
    procedure addPrevented(v: PVariable);
    procedure removePrevented(v: PVariable);
    procedure clearPrevented;
    function SaveState: TState;
    procedure RestoreState(State: TState);
    procedure setState(State: TState);
    function NewLabel: PLabel;
    function NewJumpTable: TJumpTable;
    procedure _RegisterOperand(op: POperand);
    procedure JumpToTable(jt: TJumpTable; Index: TRegister);
    function _AddTarget(Target: Pointer): SysInt;
    procedure _jmpAndRestore(Code: UInt32; Lbl: PLabel; State: TState);
    procedure op_var32(Code: UInt32; a: TInt32Ref);
    procedure op_reg32_var32(Code: UInt32; a: TRegister; b: TInt32Ref);
    procedure op_var32_reg32(Code: UInt32; a: TInt32Ref; b: TRegister);
    procedure op_var32_imm(Code: UInt32; a: TInt32Ref; b: TImmediate);
{$IFDEF ASMJIT_X64}
    procedure op_var64(Code: UInt32; a: TInt64Ref);
    procedure op_reg64_var64(Code: UInt32; a: TRegister; b: TInt64Ref);
    procedure op_var64_reg64(Code: UInt32; a: TInt64Ref; b: TRegister);
    procedure op_var64_imm(Code: UInt32; a: TInt64Ref; b: TImmediate);
{$ENDIF}
    procedure _inlineComment(_text: string); override;
    procedure _emitX86(Code: UInt32; o1, o2, o3: POperand); override;
    procedure _embed(data: Pointer; Size: SysUInt); override;
    procedure Align(m: SysInt); override;
    procedure Bind(Lbl: PLabel); override;
    function Make(MemManager: TMemoryManager = nil; AllocType: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer; override;
    procedure Serialize(a: TAssembler);

    property FirstEmittable: TEmittable read FFirst;
    property LastEmittable: TEmittable read FLast;
    property CurrentEmittable: TEmittable read FCurrent;
    property CurrentFunction: TFunction read FCurrentFunction;
  end;

  TCompilerIntrinsics = class(TCompilerCore)
  public
    procedure jAndRestore(cc: SysInt; Lbl: PLabel; State: TState);
    procedure jaAndRestore  (Lbl: PLabel; State: TState);
    procedure jaeAndRestore (Lbl: PLabel; State: TState);
    procedure jbAndRestore  (Lbl: PLabel; State: TState);
    procedure jbeAndRestore (Lbl: PLabel; State: TState);
    procedure jcAndRestore  (Lbl: PLabel; State: TState);
    procedure jeAndRestore  (Lbl: PLabel; State: TState);
    procedure jgAndRestore  (Lbl: PLabel; State: TState);
    procedure jgeAndRestore (Lbl: PLabel; State: TState);
    procedure jlAndRestore  (Lbl: PLabel; State: TState);
    procedure jleAndRestore (Lbl: PLabel; State: TState);
    procedure jnaAndRestore (Lbl: PLabel; State: TState);
    procedure jnaeAndRestore(Lbl: PLabel; State: TState);
    procedure jnbAndRestore (Lbl: PLabel; State: TState);
    procedure jnbeAndRestore(Lbl: PLabel; State: TState);
    procedure jncAndRestore (Lbl: PLabel; State: TState);
    procedure jneAndRestore (Lbl: PLabel; State: TState);
    procedure jngAndRestore (Lbl: PLabel; State: TState);
    procedure jngeAndRestore(Lbl: PLabel; State: TState);
    procedure jnlAndRestore (Lbl: PLabel; State: TState);
    procedure jnleAndRestore(Lbl: PLabel; State: TState);
    procedure jnoAndRestore (Lbl: PLabel; State: TState);
    procedure jnpAndRestore (Lbl: PLabel; State: TState);
    procedure jnsAndRestore (Lbl: PLabel; State: TState);
    procedure jnzAndRestore (Lbl: PLabel; State: TState);
    procedure joAndRestore  (Lbl: PLabel; State: TState);
    procedure jpAndRestore  (Lbl: PLabel; State: TState);
    procedure jpeAndRestore (Lbl: PLabel; State: TState);
    procedure jpoAndRestore (Lbl: PLabel; State: TState);
    procedure jsAndRestore  (Lbl: PLabel; State: TState);
    procedure jzAndRestore  (Lbl: PLabel; State: TState);
    procedure jmpAndRestore (Lbl: PLabel; State: TState);

    procedure adc(dst: TRegister; src: TInt32Ref); overload;
    procedure adc(dst: TInt32Ref; src: TRegister); overload;
    procedure adc(dst: TInt32Ref; src: TImmediate); overload;

    procedure add(dst: TRegister; src: TInt32Ref); overload;
    procedure add(dst: TInt32Ref; src: TRegister); overload;
    procedure add(dst: TInt32Ref; src: TImmediate); overload;

    procedure and_(dst: TRegister; src: TInt32Ref); overload;
    procedure and_(dst: TInt32Ref; src: TRegister); overload;
    procedure and_(dst: TInt32Ref; src: TImmediate); overload;

    procedure cmp(dst: TRegister; src: TInt32Ref); overload;
    procedure cmp(dst: TInt32Ref; src: TRegister); overload;
    procedure cmp(dst: TInt32Ref; src: TImmediate); overload;

    procedure dec_(dst: TInt32Ref); overload;
    procedure inc_(dst: TInt32Ref); overload;
    procedure neg(dst: TInt32Ref); overload;
    procedure not_(dst: TInt32Ref); overload;

    procedure mov(dst: TRegister; src: TInt32Ref); overload;
    procedure mov(dst: TInt32Ref; src: TRegister); overload;
    procedure mov(dst: TInt32Ref; src: TImmediate); overload;

    procedure or_(dst: TRegister; src: TInt32Ref); overload;
    procedure or_(dst: TInt32Ref; src: TRegister); overload;
    procedure or_(dst: TInt32Ref; src: TImmediate); overload;

    procedure sbb(dst: TRegister; src: TInt32Ref); overload;
    procedure sbb(dst: TInt32Ref; src: TRegister); overload;
    procedure sbb(dst: TInt32Ref; src: TImmediate); overload;

    procedure sub(dst: TRegister; src: TInt32Ref); overload;
    procedure sub(dst: TInt32Ref; src: TRegister); overload;
    procedure sub(dst: TInt32Ref; src: TImmediate); overload;

    procedure xor_(dst: TRegister; src: TInt32Ref); overload;
    procedure xor_(dst: TInt32Ref; src: TRegister); overload;
    procedure xor_(dst: TInt32Ref; src: TImmediate); overload;

{$IFDEF ASMJIT_X64}
    procedure adc(dst: TRegister; src: TInt64Ref); overload;
    procedure adc(dst: TInt64Ref; src: TRegister); overload;
    procedure adc(dst: TInt64Ref; src: TImmediate); overload;

    procedure add(dst: TRegister; src: TInt64Ref); overload;
    procedure add(dst: TInt64Ref; src: TRegister); overload;
    procedure add(dst: TInt64Ref; src: TImmediate); overload;

    procedure and_(dst: TRegister; src: TInt64Ref); overload;
    procedure and_(dst: TInt64Ref; src: TRegister); overload;
    procedure and_(dst: TInt64Ref; src: TImmediate); overload;

    procedure cmp(dst: TRegister; src: TInt64Ref); overload;
    procedure cmp(dst: TInt64Ref; src: TRegister); overload;
    procedure cmp(dst: TInt64Ref; src: TImmediate); overload;

    procedure dec_(dst: TInt64Ref); overload;
    procedure inc_(dst: TInt64Ref); overload;
    procedure neg(dst: TInt64Ref); overload;
    procedure not_(dst: TInt64Ref); overload;

    procedure mov(dst: TRegister; src: TInt64Ref); overload;
    procedure mov(dst: TInt64Ref; src: TRegister); overload;
    procedure mov(dst: TInt64Ref; src: TImmediate); overload;

    procedure or_(dst: TRegister; src: TInt64Ref); overload;
    procedure or_(dst: TInt64Ref; src: TRegister); overload;
    procedure or_(dst: TInt64Ref; src: TImmediate); overload;

    procedure sbb(dst: TRegister; src: TInt64Ref); overload;
    procedure sbb(dst: TInt64Ref; src: TRegister); overload;
    procedure sbb(dst: TInt64Ref; src: TImmediate); overload;

    procedure sub(dst: TRegister; src: TInt64Ref); overload;
    procedure sub(dst: TInt64Ref; src: TRegister); overload;
    procedure sub(dst: TInt64Ref; src: TImmediate); overload;

    procedure xor_(dst: TRegister; src: TInt64Ref); overload;
    procedure xor_(dst: TInt64Ref; src: TRegister); overload;
    procedure xor_(dst: TInt64Ref; src: TImmediate); overload;
{$ENDIF}
  end;

  TCompiler = class(TCompilerIntrinsics);

implementation

uses
  SysUtils, DAsmJit_Logger, DAsmJit_CpuInfo;

procedure delAll(first: TEmittable);
var
  cur, t: TEmittable;
begin
  cur := first;
  while (cur <> nil) do
  begin
    t := cur;
    cur := t.Next;
    t.Free;
    //Dispose(t);
  end;
end;

procedure memset32(p: PUInt32; c: UInt32; len: SysUInt);
var
  i: SysUInt;
begin
  if (len <= 0) then
    Exit;
  for i := 0 to len - 1 do
  begin
    p^ := c;
    Inc(p);
  end;
end;

const
//  TCLASS_INFO_E = (
    CLASS_NONE   = $00;
    CLASS_GP     = $01;
    CLASS_X87    = $02;
    CLASS_MM     = $04;
    CLASS_XMM    = $08;
    CLASS_SP_FP  = $10;
    CLASS_DP_FP  = $20;
    CLASS_VECTOR = $40;
//  );

type
  TVariableInfo = record
    size: UInt8;
    regCode: UInt8;
    clazz: UInt8;
    reserved: UInt8;
    name: string;
  end;

const
  VariableInfo: array[0..10] of TVariableInfo = (
    (size: 0 ; regCode: NO_REG ; clazz: CLASS_NONE                              ; reserved: 0; name: 'none'        ),
    (size: 4 ; regCode: REG_GPD; clazz: CLASS_GP                                ; reserved: 0; name: 'int32'       ),
    (size: 8 ; regCode: REG_GPQ; clazz: CLASS_GP                                ; reserved: 0; name: 'int64'       ),
    (size: 4 ; regCode: REG_X87; clazz: CLASS_X87 or CLASS_SP_FP                ; reserved: 0; name: 'x87_float'   ),
    (size: 8 ; regCode: REG_X87; clazz: CLASS_X87 or CLASS_DP_FP                ; reserved: 0; name: 'x87_double'  ),
    (size: 4 ; regCode: REG_XMM; clazz: CLASS_XMM or CLASS_SP_FP                ; reserved: 0; name: 'xmm_float'   ),
    (size: 8 ; regCode: REG_XMM; clazz: CLASS_XMM or CLASS_DP_FP                ; reserved: 0; name: 'xmm_double'  ),
    (size: 16; regCode: REG_XMM; clazz: CLASS_XMM or CLASS_SP_FP or CLASS_VECTOR; reserved: 0; name: 'xmm_float4'  ),
    (size: 16; regCode: REG_XMM; clazz: CLASS_XMM or CLASS_DP_FP or CLASS_VECTOR; reserved: 0; name: 'xmm_double2' ),
    (size: 8 ; regCode: REG_MM ; clazz: CLASS_MM                                ; reserved: 0; name: 'mm'          ),
    (size: 16; regCode: REG_XMM; clazz: CLASS_XMM                               ; reserved: 0; name: 'xmm'         )
  );

function getVariableSize(typ: UInt32): UInt32;
begin
  Assert(typ < UInt32(Length(VariableInfo)));
  Result := variableInfo[typ].size;
end;

function getVariableRegisterCode(typ: UInt32; index: UInt8): UInt8;
begin
  Assert(typ < UInt32(Length(variableInfo)));
  Result := variableInfo[typ].regCode or index;
end;

function isIntegerVariable(typ: UInt32): Boolean;
begin
  Assert(typ < UInt32(Length(variableInfo)));
  Result := (variableInfo[typ].clazz and CLASS_GP) <> 0;
end;

function isFloatArgument(typ: UInt32): Boolean;
begin
  Assert(typ < UInt32(Length(variableInfo)));
  Result := (variableInfo[typ].clazz and (CLASS_SP_FP or CLASS_DP_FP)) <> 0;
end;

constructor TVariable.Create(c: TCompiler; f: TFunction; ATyp: UInt8);
begin
  //inherited Create;

  Assert(f <> nil);
  FCompiler := c;
  FFunction := f;
  FType := ATyp;
  FRefCount := 0;
  FSpillCount := 0;
  FRegisterAccessCount := 0;
  FMemoryAccessCount := 0;
  FLifeId := 0;
  FGlobalSpillCount := 0;
  FGlobalRegisterAccessCount := 0;
  FGlobalMemoryAccessCount := 0;
  FType := typ;
  FSize := getVariableSize(typ);
  FState := VARIABLE_STATE_UNUSED;
  FPriority := 10;
  FRegisterCode := NO_REG;
  FPreferredRegisterCode := $FF;
  FHomeRegisterCode := $FF;
  FChanged := False;
  FReusable := False;
  FCustomMemoryHome := False;
  FStackArgument := False;
  FStackOffset := 0;
  FAllocFn := nil;
  FSpillFn := nil;
  FDataPtr := nil;
  FDataInt := 0;
  FName := '';

  FMemoryOperand.Create(ebp, 0, FSize);
  c._registerOperand(@FMemoryOperand);
end;

procedure TVariable.setPriority(APriority: UInt8);
begin
  FPriority := APriority;

  if (priority = 0) then FFunction.alloc(@Self);
end;

procedure TVariable.setMemoryHome(memoryHome: TMem);
begin
  FReusable := False;
  FCustomMemoryHome := True;
  FMemoryOperand := memoryHome;
end;

function TVariable.ref: PVariable;
begin
  Inc(FRefCount);
  Result := @Self;
end;

procedure TVariable.deref;
begin
  Dec(FRefCount);
  if (FRefCount = 0) then unuse;
end;

procedure TVariable.getReg(mode, preferredRegister: UInt8; out dest: TBaseReg; regType: UInt8);
var
  ASize: UInt8;
begin
  alloc(mode, preferredRegister);

  ASize := UInt8(1) shl (regType shr 4);
  if (regType = REG_X87) then ASize := 10;
  if (regType = REG_MM ) then ASize := 8;
  if (regType = REG_XMM) then ASize := 16;

  dest.Create((FRegisterCode and REGCODE_MASK) or regType, ASize);

  Inc(FRegisterAccessCount);
  Inc(FGlobalRegisterAccessCount);
end;

function TVariable.m: TMem;
begin
  if (not spill) then
  begin
    // TODO: Error handling.
  end;

  Inc(FMemoryAccessCount);
  Inc(FGlobalMemoryAccessCount);

  Result := FMemoryOperand;
end;

function TVariable.alloc(mode, preferredRegister: UInt8): Boolean;
begin
  Result := FFunction.alloc(@Self, mode, preferredRegister);
end;

function TVariable.spill: Boolean;
begin
  Result := FFunction.spill(@Self);
end;

procedure TVariable.unuse;
begin
  FFunction.unuse(@Self);
end;

function TVariable.isCustom: Boolean;
begin
  Result := (@FAllocFn <> nil) or (@FSpillFn <> nil);
end;

procedure TVariable.setAll(ATyp, ASize, Astate, APriority, ARegisterCode, APreferredRegisterCode: UInt8; AStackOffset: SysInt);
begin
  FType := ATyp;
  FSize := ASize;
  FState := Astate;
  FPriority := APriority;
  FRegisterCode := ARegisterCode;
  FPreferredRegisterCode := APreferredRegisterCode;
  FStackOffset := AStackOffset;
end;

constructor TVariableRef.Create(AVar: PVariable = nil);
begin
  //inherited Create;

  if (AVar = nil) then
    Fv := nil
  else
    Fv := AVar.ref;
end;

destructor TVariableRef.Destroy;
begin
  if (Fv <> nil) then
    Fv.deref;

  inherited;
end;


function TVariableRef.Typ: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.Typ;
end;

function TVariableRef.Size: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.Size;
end;

function TVariableRef.State: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.State;
end;

procedure TVariableRef.Use(AVar: PVariable);
var
  tmp: PVariable;
begin
  tmp := AVar.ref;
  if (AVar <> nil) then
    Fv.deref;
  Fv := tmp;
end;

function TVariableRef.Alloc(mode: UInt8 = VARIABLE_ALLOC_READWRITE; preferredRegister: UInt8 = NO_REG): Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.Alloc(mode, preferredRegister);
end;

function TVariableRef.Spill: Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.Spill;
end;

procedure TVariableRef.Unuse;
begin
  if (Fv <> nil) then
    Fv.unuse;
end;

procedure TVariableRef.DestroyVar;
begin
  if (Fv <> nil) then
  begin
    Fv.deref;
    Fv := nil;
  end;
end;

function TVariableRef.PreferredRegisterCode: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.PreferredRegisterCode;
end;

procedure TVariableRef.SetPreferredRegisterCode(Code: UInt8);
begin
  Assert(Fv <> nil);
  Fv.PreferredRegisterCode := Code;
end;

function TVariableRef.HomeRegisterCode: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.HomeRegisterCode;
end;

function TVariableRef.Priority: UInt8;
begin
  Assert(Fv <> nil);
  Result := Fv.Priority;
end;

procedure TVariableRef.setPriority(APriority: UInt8);
begin
  Assert(Fv <> nil);
  Fv.Priority := APriority;
end;

function TVariableRef.Changed: Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.Changed;
end;

procedure TVariableRef.SetChanged(AChanged: Boolean);
begin
  Assert(Fv <> nil);
  Fv.Changed := AChanged;
end;

function TVariableRef.Reusable: Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.Reusable;
end;

function TVariableRef.CustomMemoryHome: Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.CustomMemoryHome;
end;

procedure TVariableRef.setMemoryHome(memoryHome: TMem);
begin
  Assert(Fv <> nil);
  Fv.setMemoryHome(memoryHome);
end;

function TVariableRef.m: TMem;
begin
  Assert(Fv <> nil);
  Result := Fv.m;
end;

function TVariableRef.ref: PVariable;
begin
  Assert(Fv <> nil);
  Result := Fv.Ref;
end;

function TVariableRef.isCustom: Boolean;
begin
  Assert(Fv <> nil);
  Result := Fv.isCustom;
end;

function TVariableRef.AllocFn: TAllocFn;
begin
  Assert(Fv <> nil);
  Result := Fv.AllocFn;
end;

function TVariableRef.SpillFn: TSpillFn;
begin
  Assert(Fv <> nil);
  Result := Fv.SpillFn;
end;

function TVariableRef.DataPtr: Pointer;
begin
  Assert(Fv <> nil);
  Result := Fv.DataPtr;
end;

function TVariableRef.DataInt: SysInt;
begin
  Assert(Fv <> nil);
  Result := Fv.DataInt;
end;

procedure TVariableRef.setAllocFn(fn: TAllocFn);
begin
  Assert(Fv <> nil);
  Fv.AllocFn := fn;
end;

procedure TVariableRef.setSpillFn(fn: TSpillFn);
begin
  Assert(Fv <> nil);
  Fv.SpillFn := fn;
end;

procedure TVariableRef.setDataPtr(data: Pointer);
begin
  Assert(Fv <> nil);
  Fv.DataPtr := data;
end;

procedure TVariableRef.setDataInt(data: SysInt);
begin
  Assert(Fv <> nil);
  Fv.DataInt := data;
end;

function TVariableRef.Name: string;
begin
  Assert(Fv <> nil);
  Result := Fv.Name;
end;

procedure TVariableRef.setName(AName: string);
begin
  Assert(Fv <> nil);
  Fv.Name := AName;
end;

procedure TVariableRef.Assign(Other: TVariableRef);
var
  tmp: PVariable;
begin
  if (Other.v <> nil) then
    tmp := Other.ref
  else
    tmp := nil;
  if (Fv <> nil) then
    Fv.deref;
  Fv := tmp;
end;

constructor TInt32Ref.Create(Other: TInt32Ref);
begin
  inherited Create(Other.v);
end;

function TInt32Ref.r  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_GPD);
end;

function TInt32Ref.r8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_GPB);
end;

function TInt32Ref.r16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_GPW);
end;

function TInt32Ref.r32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_GPD);
end;

{$IFDEF ASMJIT_X64}
function TInt32Ref.r64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_GPQ);
end;
{$ENDIF}

function TInt32Ref.c  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_GPD);
end;

function TInt32Ref.c8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_GPB);
end;

function TInt32Ref.c16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_GPW);
end;

function TInt32Ref.c32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_GPD);
end;

{$IFDEF ASMJIT_X64}
function TInt32Ref.c64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_GPQ);
end;
{$ENDIF}

function TInt32Ref.x  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Assert(Fv.FFunction.Compiler = fv.FCompiler);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_GPD);
end;

function TInt32Ref.x8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_GPB);
end;

function TInt32Ref.x16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_GPW);
end;

function TInt32Ref.x32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_GPD);
end;

{$IFDEF ASMJIT_X64}
function TInt32Ref.x64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_GPQ);
end;
{$ENDIF}

{$IFDEF ASMJIT_X64}
constructor TInt64Ref.Create(Other: TInt64Ref);
begin
  inherited Create(Other.v);
end;

function TInt64Ref.r  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, Result, REG_GPQ);
end;

function TInt64Ref.r8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, Result, REG_GPB);
end;

function TInt64Ref.r16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, Result, REG_GPW);
end;

function TInt64Ref.r32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, Result, REG_GPD);
end;

function TInt64Ref.r64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, Result, REG_GPQ);
end;

function TInt64Ref.c  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, Result, REG_GPQ);
end;

function TInt64Ref.c8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, Result, REG_GPB);
end;

function TInt64Ref.c16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, Result, REG_GPW);
end;

function TInt64Ref.c32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, Result, REG_GPD);
end;

function TInt64Ref.c64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, Result, REG_GPQ);
end;

function TInt64Ref.x  (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, Result, REG_GPQ);
end;

function TInt64Ref.x8 (Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, Result, REG_GPB);
end;

function TInt64Ref.x16(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, Result, REG_GPW);
end;

function TInt64Ref.x32(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, Result, REG_GPD);
end;

function TInt64Ref.x64(Pref: UInt8 = NO_REG): TRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, Result, REG_GPQ);
end;
{$ENDIF}

constructor TMMRef.Create(Other: TMMRef);
begin
  inherited Create(Other.v);
end;

function TMMRef.r(Pref: UInt8 = NO_REG): TMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_MM);
end;

function TMMRef.c(Pref: UInt8 = NO_REG): TMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_MM);
end;

function TMMRef.x(Pref: UInt8 = NO_REG): TMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_MM);
end;

constructor TXMMRef.Create(Other: TXMMRef);
begin
  inherited Create(Other.v);
end;

function TXMMRef.r(Pref: UInt8 = NO_REG): TXMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READWRITE, Pref, TBaseReg(Result), REG_XMM);
end;

function TXMMRef.c(Pref: UInt8 = NO_REG): TXMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_READ, Pref, TBaseReg(Result), REG_XMM);
end;

function TXMMRef.x(Pref: UInt8 = NO_REG): TXMMRegister;
begin
  Assert(Fv <> nil);
  Fv.getReg(VARIABLE_ALLOC_WRITE, Pref, TBaseReg(Result), REG_XMM);
end;

constructor TState.Create(c: TCompiler; f: TFunction);
begin
  //inherited Create;
  FCompiler := c;
  FFunction := f;
  _clear;
end;

type
  PJumpAndRestore = ^TJumpAndRestore;
  TJumpAndRestore = record
    next: PJumpAndRestore;
    instruction: TInstruction;
    sFrom: TState;
    sTo: TState;
  end;

{class} procedure TState__saveFunctionState(dst: PData; f: TFunction);
var
  i: SysUInt;
  v: PVariable;
begin
  for i := 0 to 16+8+16 - 1 do
  begin
    v := f.FState.regs[i];

    if (v <> nil) then
    begin
      dst.regs[i].v := v;
      dst.regs[i].lifeId := v.lifeId;
      dst.regs[i].state := v.state;
      dst.regs[i].changed := v.changed;
    end
    else
      FillChar(dst.regs[i], SizeOf(TEntry), 0);
  end;

  dst.usedGpRegisters  := f.usedGpRegisters;
  dst.usedMmRegisters  := f.usedMmRegisters;
  dst.usedXmmRegisters := f.usedXmmRegisters;
end;

procedure TState._clear;
begin
  FillChar(FData, SizeOf(TData), 0);
end;

{constructor TStateRef.Create(AState: TState);
begin
  //inherited Create;

  FState := AState;
end;}

constructor TEmittable.Create(c: TCompiler; ATyp: UInt32);
begin
  inherited Create;

  FCompiler := c;
  FNext := nil;
  FPrev := nil;
  FType := ATyp;
end;

procedure TEmittable.Prepare;
begin
{nothing}
end;

procedure TEmittable.Emit;
begin
{nothing}
end;

procedure TEmittable.postEmit(a: TAssembler);
begin
{nothing}
end;

constructor TComment.Create(c: TCompiler; AStr: string);
begin
  inherited Create(c, EMITTABLE_COMMENT);

  FStr := AStr;
end;

procedure TComment.Emit(a: TAssembler);
begin
  if (a.Logger <> nil) then
    a.Logger.log(FStr);
end;

constructor TEmbeddedData.Create(c: TCompiler; ACapacity: SysUInt; AData: Pointer; ASize: SysUInt);
begin
  inherited Create(c, EMITTABLE_EMBEDDED_DATA);

  Assert(ACapacity >= ASize);
  FSize := ASize;
  FCapacity := ACapacity;
  SetLength(FData, ASize);
  Move(AData^, FData[0], ASize);
end;

procedure TEmbeddedData.Emit(a: TAssembler);
begin
  a._embed(Data, FSize);
end;

function TEmbeddedData.Data: PUInt8;
begin
  Result := @FData[0];
end;

constructor TAlign.Create(c: TCompiler; ASize: SysInt);
begin
  inherited Create(c, EMITTABLE_ALIGN);
  FSize := ASize;
end;

procedure TAlign.Emit(a: TAssembler);
begin
  a.align(FSize);
end;

constructor TInstruction.Create(c: TCompiler; ACode: UInt32; Ao1, Ao2, Ao3: POperand; inlineComment: string = '');
var
  oId: UInt32;
begin
  inherited Create(c, EMITTABLE_INSTRUCTION);

  FCode := ACode;
  FInlineComment := inlineComment;

  Fo[0] := @FoCache[0];
  Fo[1] := @FoCache[1];
  Fo[2] := @FoCache[2];

  if (Ao1 = nil) then
    //FillChar(Fo[0]^, SizeOf(TOperand), 0)
    Fo[0] := nil
  else
  begin
    oId := Ao1.OperandId;
    if (oId <> 0) then
    begin
      Assert(oId < c.FOperands.length);
      Fo[0] := c.FOperands[oId];
      FoCache[0] := Fo[0]^;
    end
    else
      FoCache[0] := Ao1^
  end;

  if (Ao2 = nil) then
    //FillChar(Fo[1]^, SizeOf(TOperand), 0)
    Fo[1] := nil
  else
  begin
    oId := Ao2.OperandId;
    if (oId <> 0) then
    begin
      Assert(oId < c.FOperands.length);
      Fo[1] := c.FOperands[oId];
    end
    else
      FoCache[1] := Ao2^
  end;

  if (Ao3 = nil) then
    //FillChar(Fo[2]^, SizeOf(TOperand), 0)
    Fo[2] := nil
  else
  begin
    oId := Ao3.OperandId;
    if (oId <> 0) then
    begin
      Assert(oId < c.FOperands.length);
      Fo[2] := c.FOperands[oId];
    end
    else
      FoCache[2] := Ao3^
  end;
end;

procedure TInstruction.Emit(a: TAssembler);
begin
  if (FInlineComment <> '') then
    a._inlineComment(FInlineComment);
  a._emitX86(Code, Fo[0], Fo[1], Fo[2]);
end;

function TInstruction.o: PPointer;
begin
  Result := @Fo[0];
end;

function TInstruction.o1: POperand;
begin
  Result := Fo[0];
end;

function TInstruction.o2: POperand;
begin
  Result := Fo[1];
end;

function TInstruction.o3: POperand;
begin
  Result := Fo[2];
end;

constructor TFunction.Create(c: TCompiler);
begin
  inherited Create(c, EMITTABLE_FUNCTION);

  FVariables := TPodVector.Create{(SizeOf(PVariable))};
  FPrevented := TPodVector.Create{(SizeOf(PVariable))};

  FCompiler := c;
  if (SizeOf(SysInt) = 4) then
    FStackAlignmentSize := 0
  else
    FStackAlignmentSize := 16;
  FVariablesStackSize := 0;
  FCConv := CALL_CONV_NONE;
  FCalleePopsStack := False;
  FNaked := False;
  FAllocableEbp := False;
  FPrologEpilogPushPop := True;
  FEmms := False;
  Fsfence := False;
  Flfence := False;
  FOptimizedPrologEpilog := True;
  FCConvArgumentsDirection := ARGUMENT_DIR_RIGHT_TO_LEFT;
  FCConvPreservedGp := 0;
  FCConvPreservedXmm := 0;
  FArgumentsCount := 0;
  FArgumentsStackSize := 0;
  FUsedGpRegisters := 0;
  FUsedMmRegisters := 0;
  FUsedXmmRegisters := 0;
  FModifiedGpRegisters := 0;
  FModifiedMmRegisters := 0;
  FModifiedXmmRegisters := 0;
  FUsePrevention := True;
  FEntryLabel := c.NewLabel^;
  FPrologLabel := c.NewLabel^;
  FExitLabel := c.NewLabel^;

  memset32(@FCConvArgumentsGp[0], $FFFFFFFF, 16);
  memset32(@FCConvArgumentsXmm[0], $FFFFFFFF, 16);
  //FillChar(FState, SizeOf(TState), 0);
end;

destructor TFunction.Destroy;
var
  i: SysUInt;
begin
  if (FVariables.Length > 0) then
    for i := 0 to FVariables.Length - 1 do
      Dispose(FVariables[i]);

  FVariables.Free;
  FPrevented.Free;

  inherited;
end;

procedure TFunction.prepare;
const
  Sizes: array[0..4] of UInt32 = (16, 8, 4, 2, 1);
var
  i, v: SysUInt;
  sp, pe, peGp, peXmm, argDisp, varDisp: SysInt;
  argMemBase, varMemBase: UInt8;
  alignSize, size: UInt32;
  Variable: PVariable;
  memop: PMem;
begin
  sp := 0;
  //argDisp := 0;
  //varDisp := 0;
  alignSize := 0;

  if (Length(Sizes) > 0) then
    for i := 0 to High(Sizes) do
    begin
      if (FVariables.Length <= 0) then
        Continue;

      Size := Sizes[i];
      for v := 0 to FVariables.length - 1 do
      begin
        Variable := FVariables[v];

        if ((Variable.Size = Size) and (not Variable.stackArgument) and (Variable.FGlobalMemoryAccessCount > 0)) then
        begin
  {$IFDEF ASMJIT_X86}
          if ((Size =  8) and (alignSize <  8)) then alignSize :=  8;
          if ((Size = 16) and (alignSize < 16)) then alignSize := 16;
  {$ENDIF}

          with PVariable(FVariables[v])^ do
            StackOffset := sp;
          Inc(sp, Size);
        end;
      end;
    end;

  sp := (sp + 15) and (not 15);

  peGp := countOfGpRegistersToBeSaved * SizeOf(SysInt);
  peXmm := countOfXmmRegistersToBeSaved * 16;
  pe := peGp + peXmm;

  FPrologEpilogStackSize := pe;
  FVariablesStackSize := sp;
  FStackAlignmentSize := alignSize;

  if (naked) then
  begin
    argMemBase := RID_ESP;
    if (PrologEpilogPushPop) then
      argDisp := peGp
    else
      argDisp := 0;

    varMemBase := RID_ESP;
    varDisp := -sp - SizeOf(SysInt);
  end
  else
  begin
    argMemBase := RID_EBP;
    argDisp := SizeOf(SysInt);

    varMemBase := RID_ESP;
    varDisp := 0;
  end;

  if (FVariables.Length > 0) then
    for v := 0 to FVariables.length - 1 do
    begin
      Variable := FVariables[v];
      memop := @Variable.FMemoryOperand;

      if (Variable.stackArgument) then
      begin
        memop.u._mem.base := argMemBase;
        memop.u._mem.displacement := Variable.stackOffset + argDisp;
      end
      else
      begin
        memop.u._mem.base := varMemBase;
        memop.u._mem.displacement := Variable.stackOffset + varDisp;
      end;
    end;
end;

procedure TFunction.emit(a: TAssembler);
var
  DAsmJit_Logger: TCustomLogger;
  _buf, loc: string;
  i, varLen, r, modifiedRegisters, regs, ATyp: SysUInt;
  v: PVariable;
  vinfo: TVariableInfo;
  first: Boolean;
begin
  first := False;

  DAsmJit_Logger := a.Logger;
  if ((DAsmJit_Logger <> nil) and DAsmJit_Logger.enabled) then
  begin
    varlen := FVariables.Length;

    DAsmJit_Logger.log('; TFunction Prototype:' + LineEnding);
    DAsmJit_Logger.log(';   (');

    if (argumentsCount > 0) then
      for i := 0 to argumentsCount - 1 do
      begin
        v := FVariables[i];

        if (i <> 0) then DAsmJit_Logger.log(', ');
        if (v.typ < _VARIABLE_TYPE_COUNT) then
          DAsmJit_Logger.log(VariableInfo[v.typ].name)
        else
          DAsmJit_Logger.log('unknown');
      end;
    DAsmJit_Logger.log(')' + LineEnding);
    DAsmJit_Logger.log(';' + LineEnding);
    DAsmJit_Logger.log('; Variables:' + LineEnding);

    if (varlen > 0) then
      for i := 0 to varlen - 1 do
      begin
        v := FVariables[i];
        vinfo := VariableInfo[v.typ];

        if (v.FGlobalMemoryAccessCount > 0) then
          loc := _buf + TLogger(DAsmJit_Logger).dumpOperand(@v.FMemoryOperand)
        else
          loc := '[None]';

        if (v.typ < _VARIABLE_TYPE_COUNT) then

          DAsmJit_Logger.logFormat(';   %-2u %-12s (%2uB) at %-20s - reg access: %-3u, mem access: %-3u' + LineEnding, [i,
            vinfo.name,
            v.size,
            loc,
            SysUInt(v.FGlobalRegisterAccessCount),
            SysUInt(v.FGlobalMemoryAccessCount)])
        else
          DAsmJit_Logger.logFormat(';   %-2u %-12s (%2uB) at %-20s - reg access: %-3u, mem access: %-3u' + LineEnding, [i,
            'unknown',
            v.size,
            loc,
            SysUInt(v.FGlobalRegisterAccessCount),
            SysUInt(v.FGlobalMemoryAccessCount)]);
      end;

    modifiedRegisters := 0;

    for r := 0 to 2 do
    begin
      _buf := _buf + ';   ';

      case r of
        0: begin regs := FModifiedGpRegisters ; ATyp := REG_GPN; _buf := _buf + 'GP :'; end;
        1: begin regs := FModifiedMmRegisters ; ATyp := REG_MM ; _buf := _buf + 'MM :'; end;
        2: begin regs := FModifiedXmmRegisters; ATyp := REG_XMM; _buf := _buf + 'XMM:'; end;
        else begin regs := 0;                   ATyp := 0;                              end;
      end;

      for i := 0 to NUM_REGS - 1 do
      begin
        if ((regs and (1 shl i)) <> 0) then
        begin
          if (not first) then
            _buf := _buf + ', ';
          _buf := _buf + TLogger(DAsmJit_Logger).dumpRegister(UInt8(ATyp), UInt8(i));
          first := False;
          Inc(modifiedRegisters);
        end;
      end;
      _buf := _buf + LineEnding;
    end;

    DAsmJit_Logger.logFormat(';' + LineEnding, []);
    DAsmJit_Logger.logFormat('; Modified registers (%u):' + LineEnding,
      [SysUInt(modifiedRegisters)]);
    DAsmJit_Logger.log(_buf);
  end;

  a.bind(@FEntryLabel);
end;

procedure TFunction.setPrototype(ACConv: UInt32; args: PUInt32; Count: SysUInt);
begin
  _setCallingConvention(ACConv);
  _setArguments(Args, Count);
end;

function TFunction.CConvArgumentsGp: PUInt32;
begin
  Result := @FCConvArgumentsGp[0];
end;

function TFunction.CConvArgumentsXmm: PUInt32;
begin
  Result := @FCConvArgumentsXmm[0];
end;

function TFunction.Argument(i: SysInt): PVariable;
begin
  Assert(UInt32(i) < FArgumentsCount);
  Result := FVariables[i];
end;

procedure TFunction._setCallingConvention(ACConv: UInt32);
begin
  FCConv := ACConv;
  FCalleePopsStack := False;

  memset32(@FCConvArgumentsGp[0], $FFFFFFFF, 16);
  memset32(@FCConvArgumentsXmm[0], $FFFFFFFF, 16);
  //FillChar(FState, SizeOf(TState), 0);

  FCConvArgumentsDirection := ARGUMENT_DIR_RIGHT_TO_LEFT;
  FArgumentsStackSize := 0;

{$IFDEF ASMJIT_X86}
  FCConvPreservedGp :=
    (1 shl (REG_EBX and REGCODE_MASK)) or
    (1 shl (REG_ESP and REGCODE_MASK)) or
    (1 shl (REG_EBP and REGCODE_MASK)) or
    (1 shl (REG_ESI and REGCODE_MASK)) or
    (1 shl (REG_EDI and REGCODE_MASK)) ;
  FCConvPreservedXmm := 0;

  case CConv of
    CALL_CONV_CDECL: ;
    CALL_CONV_STDCALL:
      FCalleePopsStack := True;
    CALL_CONV_MSTHISCALL:
    begin
      FCConvArgumentsGp[0] := (REG_ECX and REGCODE_MASK);
      FCalleePopsStack := True;
    end;

    CALL_CONV_MSFASTCALL:
    begin
      FCConvArgumentsGp[0] := (REG_ECX and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_EDX and REGCODE_MASK);
      FCalleePopsStack := True;
    end;

    CALL_CONV_BORLANDFASTCALL:
    begin
      FCConvArgumentsGp[0] := (REG_EAX and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_EDX and REGCODE_MASK);
      FCConvArgumentsGp[2] := (REG_ECX and REGCODE_MASK);
      FCConvArgumentsDirection := ARGUMENT_DIR_LEFT_TO_RIGHT;
      FCalleePopsStack := True;
    end;

    CALL_CONV_GCCFASTCALL_2:
    begin
      FCConvArgumentsGp[0] := (REG_ECX and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_EDX and REGCODE_MASK);
      FCalleePopsStack := False;
    end;

    CALL_CONV_GCCFASTCALL_3:
    begin
      FCConvArgumentsGp[0] := (REG_EDX and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_ECX and REGCODE_MASK);
      FCConvArgumentsGp[2] := (REG_EAX and REGCODE_MASK);
      FCalleePopsStack := False;
    end;
    else Assert(False);
  end;
{$ELSE}
  case CConv of
    CALL_CONV_X64W:
    begin
      FCConvPreservedGp :=
        (1 shl (REG_RBX   and REGCODE_MASK)) or
        (1 shl (REG_RSP   and REGCODE_MASK)) or
        (1 shl (REG_RBP   and REGCODE_MASK)) or
        (1 shl (REG_RSI   and REGCODE_MASK)) or
        (1 shl (REG_RDI   and REGCODE_MASK)) or
        (1 shl (REG_R12   and REGCODE_MASK)) or
        (1 shl (REG_R13   and REGCODE_MASK)) or
        (1 shl (REG_R14   and REGCODE_MASK)) or
        (1 shl (REG_R15   and REGCODE_MASK)) ;
      FCConvPreservedXmm :=
        (1 shl (REG_XMM6  and REGCODE_MASK)) or
        (1 shl (REG_XMM7  and REGCODE_MASK)) or
        (1 shl (REG_XMM8  and REGCODE_MASK)) or
        (1 shl (REG_XMM9  and REGCODE_MASK)) or
        (1 shl (REG_XMM10 and REGCODE_MASK)) or
        (1 shl (REG_XMM11 and REGCODE_MASK)) or
        (1 shl (REG_XMM12 and REGCODE_MASK)) or
        (1 shl (REG_XMM13 and REGCODE_MASK)) or
        (1 shl (REG_XMM14 and REGCODE_MASK)) or
        (1 shl (REG_XMM15 and REGCODE_MASK)) ;

      FCConvArgumentsGp[0] := (REG_RCX  and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_RDX  and REGCODE_MASK);
      FCConvArgumentsGp[2] := (REG_R8   and REGCODE_MASK);
      FCConvArgumentsGp[3] := (REG_R9   and REGCODE_MASK);
      FCConvArgumentsXmm[0] := (REG_XMM0 and REGCODE_MASK);
      FCConvArgumentsXmm[1] := (REG_XMM1 and REGCODE_MASK);
      FCConvArgumentsXmm[2] := (REG_XMM2 and REGCODE_MASK);
      FCConvArgumentsXmm[3] := (REG_XMM3 and REGCODE_MASK);
    end;

    CALL_CONV_X64U:
    begin
      FCConvPreservedGp :=
        (1 shl (REG_RBX   and REGCODE_MASK)) or
        (1 shl (REG_RSP   and REGCODE_MASK)) or
        (1 shl (REG_RBP   and REGCODE_MASK)) or
        (1 shl (REG_R12   and REGCODE_MASK)) or
        (1 shl (REG_R13   and REGCODE_MASK)) or
        (1 shl (REG_R14   and REGCODE_MASK)) or
        (1 shl (REG_R15   and REGCODE_MASK)) ;
      FCConvPreservedXmm := 0;

      FCConvArgumentsGp[0] := (REG_RDI  and REGCODE_MASK);
      FCConvArgumentsGp[1] := (REG_RSI  and REGCODE_MASK);
      FCConvArgumentsGp[2] := (REG_RDX  and REGCODE_MASK);
      FCConvArgumentsGp[3] := (REG_RCX  and REGCODE_MASK);
      FCConvArgumentsGp[4] := (REG_R8   and REGCODE_MASK);
      FCConvArgumentsGp[5] := (REG_R9   and REGCODE_MASK);
      FCConvArgumentsXmm[0] := (REG_XMM0 and REGCODE_MASK);
      FCConvArgumentsXmm[1] := (REG_XMM1 and REGCODE_MASK);
      FCConvArgumentsXmm[2] := (REG_XMM2 and REGCODE_MASK);
      FCConvArgumentsXmm[3] := (REG_XMM3 and REGCODE_MASK);
      FCConvArgumentsXmm[4] := (REG_XMM4 and REGCODE_MASK);
      FCConvArgumentsXmm[5] := (REG_XMM5 and REGCODE_MASK);
      FCConvArgumentsXmm[6] := (REG_XMM6 and REGCODE_MASK);
      FCConvArgumentsXmm[7] := (REG_XMM7 and REGCODE_MASK);
    end;

    else Assert(False);
  end;
{$ENDIF}
end;

procedure TFunction._setArguments(_args: PUInt32; count: SysUInt);
var
  i, gpnPos, AStackOffset, istart, iend, istep: SysInt;
{$IFNDEF ASMJIT_X86}
  xmmPos, max: SysInt;
{$ENDIF}
  args: array[0..31] of UInt32;
  v: PVariable;
  a: UInt32;
  reg, size: UInt8;
begin
  Assert(Count <= 32);

  gpnPos := 0;
{$IFNDEF ASMJIT_X86}
  xmmPos := 0;
{$ENDIF}
  AStackOffset := 0;

  Move(_args^, args[0], count * SizeOf(UInt32));
  FVariables.clear;

  for i := 0 to SysInt(Count) - 1 do
  begin
    New(v);
    v.Create(FCompiler, Self, args[i]);
    v.FRefCount := 1;

    if (FCompiler.Logger <> nil) then
      v.Name := Format('arg%d', [i]);

    FVariables.append(v);
  end;

  FArgumentsCount := count;
  if (_args = nil) then Exit;

{$IFDEF ASMJIT_X86}
  for i := 0 to SysInt(Count) - 1 do
  begin
    a := args[i];
    if (isIntegerVariable(a) and (gpnPos < 32) and (FCConvArgumentsGp[gpnPos] <> $FFFFFFFF)) then
    begin
      reg := FCConvArgumentsGp[gpnPos] or REG_GPN;
      Inc(gpnPos);
      size := variableInfo[a].size;
      v := FVariables[i];

      v.setAll(a, size, VARIABLE_STATE_REGISTER, 10, reg, NO_REG, 0);
      v.Changed := True;
      _AllocReg(reg, v);

      FState.gp[reg and $0F] := v;
      args[i] := VARIABLE_TYPE_NONE;
    end;
  end;

  if (FCConvArgumentsDirection = ARGUMENT_DIR_LEFT_TO_RIGHT) then
  begin
    istart := 0;
    iend   := SysInt(Count);
    istep  := 1;
  end
  else
  begin
    istart := SysInt(Count) - 1;
    iend   := -1;
    istep  := -1;
  end;

  i := istart;
  while (i <> iend) do
  begin
    a := args[i];

    if (isIntegerVariable(a)) then
    begin
      size := variableInfo[a].size;
      v := FVariables[i];
      Dec(AStackOffset, 4);

      v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
      v.FStackArgument := True;
      args[i] := VARIABLE_TYPE_NONE;
    end
    else if (isFloatArgument(a)) then
    begin
      size := variableInfo[a].size;
      v := FVariables[i];
      Dec(AStackOffset, size);

      v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
      v.FStackArgument := True;
      args[i] := VARIABLE_TYPE_NONE;
    end;
    i := i + istep;
  end;
{$ELSE}
  if (CConv = CALL_CONV_X64W) then
  begin
    if (Count < 4) then
      max := Count
    else
      max := 4;

    for i := 0 to max - 1 do
    begin
      a := args[i];
      if (isIntegerVariable(a)) then
      begin
        reg := FCConvArgumentsGp[i] or REG_GPN;
        size := variableInfo[a].size;
        v := FVariables[i];

        v.setAll(a, size, VARIABLE_STATE_REGISTER, 20, reg, NO_REG, 0);
        v.FChanged := true;
        _AllocReg(reg, v);

        FState.gp[reg and $0F] := v;
        args[i] := VARIABLE_TYPE_NONE;
      end
      else if (isFloatArgument(a)) then
      begin
        reg := FCConvArgumentsXmm[i] or REG_XMM;
        size := variableInfo[a].size;
        v := FVariables[i];

        v.setAll(a, size, VARIABLE_STATE_REGISTER, 20, reg, NO_REG, 0);
        v.Changed := true;
        _AllocReg(reg, v);

        Fstate.xmm[reg and $0F] := v;
        args[i] := VARIABLE_TYPE_NONE;
      end;
    end;

    for i := SysInt(count) - 1 downto 0 do
    begin
      a := args[i];
      if (isIntegerVariable(a)) then
      begin
        size := variableInfo[a].size;
        v := FVariables[i];

        Dec(AStackOffset, 8);

        v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
        v.FStackArgument := True;
        args[i] := VARIABLE_TYPE_NONE;
      end
      else if (isFloatArgument(a)) then
      begin
        size := variableInfo[a].size;
        v := FVariables[i];

        Dec(AStackOffset, size);

        v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
        v.FStackArgument := True;
        args[i] := VARIABLE_TYPE_NONE;
      end;
    end;

    Dec(AStackOffset, 4 * 8);
  end
  else
  begin
    for i := 0 to SysInt(count) - 1 do
    begin
      a := args[i];
      if (isIntegerVariable(a) and (gpnPos < 32) and (FCConvArgumentsGp[gpnPos] <> $FFFFFFFF)) then
      begin
        reg := FCConvArgumentsGp[gpnPos] or REG_GPN;
        Inc(gpnPos);
        size := variableInfo[a].size;
        v := FVariables[i];

        v.setAll(a, size, VARIABLE_STATE_REGISTER, 20, reg, NO_REG, 0);
        v.FChanged := true;
        _allocReg(reg, v);

        FState.gp[reg and $0F] := v;
        args[i] := VARIABLE_TYPE_NONE;
      end;
    end;

    for i := 0 to SysInt(count) -1 do
    begin
      a := args[i];
      if (isFloatArgument(a)) then
      begin
        reg := FCConvArgumentsXmm[xmmPos] or REG_XMM;
        Inc(xmmPos);
        size := variableInfo[a].size;
        v := FVariables[i];

        v.setAll(a, size, VARIABLE_STATE_REGISTER, 20, reg, NO_REG, 0);
        v.FChanged := true;
        _allocReg(reg, v);

        FState.xmm[reg and $0F] := v;
        args[i] := VARIABLE_TYPE_NONE;
      end;
    end;

    for i := SysInt(count) - 1 downto 0 do
    begin
      a := args[i];
      if (isIntegerVariable(a)) then
      begin
        size := variableInfo[a].size;
        v := FVariables[i];

        Dec(AStackOffset, 8);

        v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
        v.FStackArgument := true;
        args[i] := VARIABLE_TYPE_NONE;
      end
      else if (isFloatArgument(a)) then
      begin
        size := variableInfo[a].size;
        v := FVariables[i];

        Dec(AStackOffset, size);

        v.setAll(a, size, VARIABLE_STATE_MEMORY, 20, NO_REG, NO_REG, AStackOffset);
        v.FStackArgument := true;
        args[i] := VARIABLE_TYPE_NONE;
      end;
    end;
  end;
{$ENDIF}

  for i := 0 to SysInt(count) - 1 do
    with PVariable(FVariables[i])^ do
      Inc(FStackOffset, SizeOf(SysInt) - AStackOffset);

  FArgumentsStackSize := UInt32(-AStackOffset);
end;

function TFunction.newVariable(ATyp, APriority, APreferredRegisterCode: UInt8): PVariable;
var
  v: PVariable;
  i: SysUInt;
begin
  if (FVariables.Length > 0) then
    for i := 0 to FVariables.length - 1 do
    begin
      v := FVariables[i];
      if ((v.refCount = 0) and (v.reusable) and (v.typ = Atyp)) then
      begin
        v.FPreferredRegisterCode := APreferredRegisterCode;
        v.FPriority := APriority;
        Result := v;
        Exit;
      end;
    end;

  New(v);
  v.Create(FCompiler, Self, ATyp);
  v.FPreferredRegisterCode := APreferredRegisterCode;
  v.FPriority := APriority;

  if (compiler.Logger <> nil) then
    v.Name := Format('var%d', [FVariables.length]);

  FVariables.append(v);
  if (APriority = 0) then alloc(v);

  Result := v;
end;

function TFunction.alloc(v: PVariable; mode, preferredRegisterCode: UInt8): Boolean;
var
  i, mask: UInt32;
  pref, home, code, oldIndex, newIndex, clazz: UInt8;
  spillCandidate, other: PVariable;
label
  L_spill;
begin
  Assert(compiler = v.compiler);

  if (preferredRegisterCode <> NO_REG) then
    pref := preferredRegisterCode
  else
    pref := v.FPreferredRegisterCode;

  home := v.homeRegisterCode;
  code := NO_REG;

  spillCandidate := nil;

  if (v.state = VARIABLE_STATE_REGISTER) then
  begin
    oldIndex := v.registerCode and $F;
    newIndex := pref and $F;


    if ((pref = NO_REG) or (oldIndex = newIndex)) then
    begin
      _postAlloc(v, mode);
      Result := True;
      Exit;
    end;

    if (isIntegerVariable(v.typ)) then
    begin
      other := FState.gp[newIndex];

      if (other.priority = 0) then
        // TODO: Error handling
        Assert(False);

      _exchangeGp(v, mode, other);
      _postAlloc(v, mode);
      Result := true;
      Exit;
    end;
  end;

  clazz := variableInfo[v.typ].clazz;
  if ((clazz and CLASS_GP) <> 0) then
  begin
    if (pref <> NO_REG) then
    begin
      Assert((pref and REGCODE_MASK) <> RID_ESP);

      if ((FUsedGpRegisters and (SysUInt(1) shl (pref and REGCODE_MASK))) = 0) then
        code := pref
      else
      begin
        spillCandidate := FState.gp[pref and REGCODE_MASK];

        if (spillCandidate = nil) then
          // TODO: Error handling
          Assert(False);

        goto L_spill;
      end;
    end;

    if ((code = NO_REG) and (home <> NO_REG)) then
      if ((FUsedGpRegisters and (SysUInt(1) shl (home and REGCODE_MASK))) = 0) then
        code := home;

    if (code = NO_REG) then
      for i := 1 to NUM_REGS - 1 do
      begin
        mask := (SysUInt(1) shl i);
        if (((FUsedGpRegisters and mask) = 0) and ((i <> RID_EBP) or (allocableEbp)) and (i <> RID_ESP)) then
        begin
          if ((code <> NO_REG) and ((FCConvPreservedGp and mask) = 1)) then
            Continue;

          if (v.typ = VARIABLE_TYPE_INT32) then
            code := i or REG_GPD
          else
            code := i or REG_GPQ;

          if ((FCConvPreservedGp and mask) = 0) then
            Break;
        end;
      end;

    if ((code = NO_REG) and ((FUsedGpRegisters and 1) = 0)) then
      if (v.typ = VARIABLE_TYPE_INT32) then
        code := RID_EAX or REG_GPD
      else
        code := RID_EAX or REG_GPQ;
  end

  else if ((clazz and CLASS_MM) <> 0) then
  begin
    if (pref <> NO_REG) then
    begin
      if ((FUsedMmRegisters and (SysUInt(1) shl (pref and $7))) = 0) then
        code := pref
      else
      begin
        spillCandidate := FState.mm[pref and REGCODE_MASK];

        if (spillCandidate = nil) then
          // TODO: Error handling
          Assert(False);

        goto L_spill;
      end;
    end;

    if ((code = NO_REG) and (home <> NO_REG)) then
      if ((FUsedMmRegisters and (SysUInt(1) shl (home and REGCODE_MASK))) = 0) then
        code := home;

    if (code = NO_REG) then
      for i := 0 to 7 do
      begin
        mask := (SysUInt(1) shl i);
        if ((FUsedMmRegisters and mask) = 0) then
        begin
          code := i or REG_MM;
          break;
        end;
      end;
  end

  else if ((clazz and CLASS_XMM) <> 0) then
  begin
    if (pref <> NO_REG) then
    begin
      if ((FUsedXmmRegisters and (SysUInt(1) shl (pref and REGCODE_MASK))) = 0) then
        code := pref
      else
      begin
        spillCandidate := FState.xmm[pref and REGCODE_MASK];

        if (spillCandidate = nil) then
          // TODO: Error handling
          Assert(False);

        goto L_spill;
      end;
    end;

    if ((code = NO_REG) and (home <> NO_REG)) then
      if ((FUsedXmmRegisters and (SysUInt(1) shl (home and REGCODE_MASK))) = 0) then
        code := home;

    if (code = NO_REG) then
      for i := 0 to NUM_REGS - 1 do
      begin
        mask := (SysUInt(1) shl i);
        if ((FUsedXmmRegisters and mask) = 0) then
        begin
          if ((code <> NO_REG) and ((FCConvPreservedXmm and mask) = 1)) then
            Continue;

          code := i or REG_XMM;

          if ((FCConvPreservedXmm and mask) = 0) then
            Break;
        end;
      end;
  end;

  if (code = NO_REG) then
  begin
    if (spillCandidate = nil) then
      spillCandidate := _getSpillCandidate(v.typ);

    if (spillCandidate = nil) then
      // TODO: Error handling
      Assert(False);

L_spill:

    Assert(not isPrevented(spillCandidate));

    if (spillCandidate.priority = 0) then
      // TODO: Error handling
      Assert(False);

    code := spillCandidate.registerCode;
    spill(spillCandidate);
  end;

  _allocAs(v, mode, code);
  _postAlloc(v, mode);
  Result := true;
end;

function TFunction.spill(v: PVariable): Boolean;
begin
  Assert(compiler = v.compiler);
  removePrevented(v);

  if (v.state = VARIABLE_STATE_UNUSED) then
  begin
    Result := true;
    Exit;
  end;
  if (v.state = VARIABLE_STATE_MEMORY) then
  begin
    Result := true;
    Exit;
  end;

  if (v.state = VARIABLE_STATE_REGISTER) then
  begin
    if (v.priority = 0) then
    begin
      Result := false;
      Exit;
    end;

    if (v.changed) then
    begin
      if (v.isCustom)  then
      begin
        if (Assigned(v.FSpillFn)) then
          v.FSpillFn(v);
      end
      else
      begin
        if (compiler.Logger <> nil) then
          compiler._inlineComment(Format('spill %s', [v.name]));

        case v.typ of
          VARIABLE_TYPE_INT32:
            compiler.mov(v.FMemoryOperand, mk_gpd(v.registerCode));

{$IFDEF ASMJIT_X64}
          VARIABLE_TYPE_INT64:
            compiler.mov(v.FMemoryOperand, mk_gpq(v.registerCode));
{$ENDIF}

          VARIABLE_TYPE_X87_FLOAT:  ; // TODO: NOT IMPLEMENTED
          VARIABLE_TYPE_X87_DOUBLE: ; // TODO: NOT IMPLEMENTED

          VARIABLE_TYPE_XMM_FLOAT:
            compiler.movss(v.FMemoryOperand, mk_xmm(v.registerCode));

          VARIABLE_TYPE_XMM_DOUBLE:
            compiler.movsd(v.FMemoryOperand, mk_xmm(v.registerCode));

          VARIABLE_TYPE_XMM_FLOAT_4:
            if (Naked) then
              compiler.movups(v.FMemoryOperand, mk_xmm(v.registerCode))
            else
              compiler.movaps(v.FMemoryOperand, mk_xmm(v.registerCode));

          VARIABLE_TYPE_XMM_DOUBLE_2:
            if (Naked) then
              compiler.movupd(v.FMemoryOperand, mk_xmm(v.registerCode))
            else
              compiler.movapd(v.FMemoryOperand, mk_xmm(v.registerCode));

          VARIABLE_TYPE_MM:
            compiler.movq(v.FMemoryOperand, mk_mm(v.registerCode));

          VARIABLE_TYPE_XMM:
            if (Naked) then
              compiler.movdqu(v.FMemoryOperand, mk_xmm(v.registerCode))
            else
              compiler.movdqa(v.FMemoryOperand, mk_xmm(v.registerCode));
        end;

        if (compiler.Logger <> nil) then
          compiler._inlineComment('');

        Inc(v.FMemoryAccessCount);
        Inc(v.FGlobalMemoryAccessCount);
      end;

      v.Changed := False;
    end;

    _freeReg(v.registerCode);
    v.FRegisterCode := NO_REG;

    v.FState := VARIABLE_STATE_MEMORY;
    Inc(v.FSpillCount);
    Inc(v.FGlobalSpillCount);
  end;

  Result := true;
end;

procedure TFunction.unuse(v: PVariable);
begin
  Assert(compiler = v.compiler);
  if (v.state = VARIABLE_STATE_UNUSED) then
    Exit;

  if (v.state = VARIABLE_STATE_REGISTER) then
  begin
    _freeReg(v.registerCode);
    v.FRegisterCode := NO_REG;
  end;

  v.FState := VARIABLE_STATE_UNUSED;

  v.FSpillCount := 0;
  v.FRegisterAccessCount := 0;
  v.FMemoryAccessCount := 0;

  Inc(v.FLifeId);

  v.FPreferredRegisterCode := NO_REG;
  v.FHomeRegisterCode := NO_REG;
  v.FPriority := 10;
  v.FChanged := False;

  v.FAllocFn := nil;
  v.FSpillFn := nil;
  v.FDataPtr := nil;
  v.FDataInt := 0;
end;

procedure TFunction.spillAll;
begin
  _spillAll(0, 16+8+16);
end;

procedure TFunction.spillAllGp;
begin
  _spillAll(0, 16);
end;

procedure TFunction.spillAllMm;
begin
  _spillAll(16, 8);
end;

procedure TFunction.spillAllXmm;
begin
  _spillAll(16+8, 16);
end;

procedure TFunction._spillAll(sStart, sEnd: SysUInt);
var
  i: SysUInt;
  v: PVariable;
begin
  Assert(sStart < sEnd);
  for i := sStart to sEnd -1 do
  begin
    v := FState.regs[i];
    if (v <> nil) then
      spill(v);
  end;
end;

procedure TFunction.spillRegister(reg: TBaseReg);
var
  i: SysUInt;
  v: PVariable;
begin
  i := reg.index;

  case reg.typ of
    REG_GPB, REG_GPW, REG_GPD, REG_GPQ:
      v := FState.gp[i];
    REG_MM:
      v := FState.mm[i];
    REG_XMM:
      v := FState.xmm[i];
    else
      Exit;
  end;

  if (v <> nil) then spill(v);
end;

function getFreeRegs(regs: UInt32; max: SysUInt): SysInt;
var
  i, n: SysUInt;
  mask: UInt32;
begin
  n := 0;
  mask := 1;

  if (max > 0) then
    for i := 0 to max - 1 do
    begin
      if ((regs and mask) = 0) then Inc(n);
      mask := mask shl 1;
    end;

  Result := n;
end;

function TFunction.numFreeGp: SysInt;
var
  n: SysInt;
begin
  n := getFreeRegs(FUsedGpRegisters, NUM_REGS);

  if ((FUsedGpRegisters and (1 shl RID_ESP)) = 0) then Dec(n);
  if (((FUsedGpRegisters and (1 shl RID_EBP)) = 0) and (not allocableEbp)) then Dec(n);

  Result := n;
end;

function TFunction.numFreeMm: SysInt;
begin
  Result := getFreeRegs(FUsedMmRegisters, 8);
end;

function TFunction.numFreeXmm: SysInt;
begin
  Result := getFreeRegs(FUsedXmmRegisters, NUM_REGS);
end;

function TFunction.isPrevented(v: PVariable): Boolean;
begin
  Result := FUsePrevention and (FPrevented.indexOf(v) <> SysUInt(-1));
end;

procedure TFunction.addPrevented(v: PVariable);
var
  i: SysUInt;
begin
  if (not FUsePrevention) then Exit;

  i := FPrevented.indexOf(v);
  if (i = SysUInt(-1)) then FPrevented.append(v);
end;

procedure TFunction.removePrevented(v: PVariable);
var
  i: SysUInt;
begin
  if (not FUsePrevention) then Exit;

  i := FPrevented.indexOf(v);
  if (i <> SysUInt(-1)) then FPrevented.removeAt(i);
end;

procedure TFunction.clearPrevented;
begin
  FPrevented.clear;
end;

function getSpillScore(v: PVariable): UInt32;
var
  p: UInt32;
begin
  if (v.priority = 0) then
  begin
    Result := 0;
    Exit;
  end;

  p := (UInt32(v.priority) shl 24) - ((SysUInt(1) shl 24) div 2);

  p := p - UInt32(v.registerAccessCount);
  p := p + UInt32(v.memoryAccessCount);

  Result := p;
end;

function TFunction._getSpillCandidate(ATyp: UInt8): PVariable;
var
  candidate, v: PVariable;
  i, len: SysUInt;
  candidateScore, variableScore: UInt32;
  clazz: UInt8;
begin
  candidate := nil;
  len := FVariables.length;

  candidateScore := 0;

  clazz := variableInfo[ATyp].clazz;

  if ((clazz and CLASS_GP) <> 0) then
  begin
    if (len > 0) then
      for i := 0 to len - 1 do
      begin
        v := FVariables[i];
        if (((v.typ = VARIABLE_TYPE_INT32) or (v.typ = VARIABLE_TYPE_INT64)) and
            ((v.state = VARIABLE_STATE_REGISTER) and (v.priority > 0)) and
            (not isPrevented(v))) then
        begin
          variableScore := getSpillScore(v);
          if (variableScore > candidateScore) then
          begin
            candidateScore := variableScore;
            candidate := v;
          end;
        end;
      end;
  end
  else if ((clazz and CLASS_X87) <> 0) then
  begin
    // TODO: Not implemented.
  end
  else if ((clazz and CLASS_MM) <> 0) then
  begin
    if (len > 0) then
      for i := 0 to len - 1 do
      begin
        v := FVariables[i];
        if ((v.typ = VARIABLE_TYPE_MM) and
            ((v.state = VARIABLE_STATE_REGISTER) and (v.priority > 0)) and
            (not isPrevented(v)))then
        begin
          variableScore := getSpillScore(v);
          if (variableScore > candidateScore) then
          begin
            candidateScore := variableScore;
            candidate := v;
          end;
        end;
      end;
  end
  else if ((clazz and CLASS_XMM) <> 0) then
  begin
    if (len > 0) then
      for i := 0 to len - 1 do
      begin
        v := FVariables[i];
        if ((v.typ = VARIABLE_TYPE_XMM) and
            ((v.state = VARIABLE_STATE_REGISTER) and (v.priority > 0)) and
            (not isPrevented(v))) then
        begin
          variableScore := getSpillScore(v);
          if (variableScore > candidateScore) then
          begin
            candidateScore := variableScore;
            candidate := v;
          end;
        end;
      end;
  end;

  Result := candidate;
end;

procedure TFunction._allocAs(v: PVariable; mode: UInt8; code: UInt32);
var
  copy: Boolean;
  old: UInt8;
  reg: TRegister;
  xmm: TXMMRegister;
  mm: TMMRegister;
begin
  copy := (v.state = VARIABLE_STATE_MEMORY);
  old := v.FRegisterCode;

  v.FState := VARIABLE_STATE_REGISTER;
  v.FRegisterCode := code;

  _allocReg(code, v);

  if (compiler.Logger <> nil) then
    compiler._inlineComment(Format('alloc %s', [v.name]));

  if (v.isCustom) then
  begin
    if ((@v.FAllocFn <> nil) and (mode <> VARIABLE_ALLOC_WRITE)) then v.FAllocFn(v);
  end
  else if (copy and (mode <> VARIABLE_ALLOC_WRITE)) then
  begin
    case v.typ of
      VARIABLE_TYPE_INT32:
      begin
        reg := mk_gpd(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.mov(reg, mk_gpd(old))
        else
          compiler.mov(reg, v.FMemoryOperand);
      end;

{$IFDEF ASMJIT_X64}
      VARIABLE_TYPE_INT64:
      begin
        reg := mk_gpq(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.mov(reg, mk_gpq(old))
        else
          compiler.mov(reg, v.FMemoryOperand);
      end;
{$ENDIF}

      VARIABLE_TYPE_X87_FLOAT:  ;  // TODO: NOT IMPLEMENTED
      VARIABLE_TYPE_X87_DOUBLE: ;  // TODO: NOT IMPLEMENTED

      VARIABLE_TYPE_XMM_FLOAT:
      begin
        xmm := mk_xmm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movss(xmm, mk_xmm(old))
        else
          compiler.movss(xmm, v.FMemoryOperand);
      end;

      VARIABLE_TYPE_XMM_DOUBLE:
      begin
        xmm := mk_xmm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movsd(xmm, mk_xmm(old))
        else
          compiler.movsd(xmm, v.FMemoryOperand);
      end;

      VARIABLE_TYPE_XMM_FLOAT_4:
      begin
        xmm := mk_xmm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movaps(xmm, mk_xmm(old))
        else if (Naked) then
          compiler.movups(xmm, v.FMemoryOperand)
        else
          compiler.movaps(xmm, v.FMemoryOperand);
      end;

      VARIABLE_TYPE_XMM_DOUBLE_2:
      begin
        xmm := mk_xmm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movapd(xmm, mk_xmm(old))
        else if (Naked) then
          compiler.movupd(xmm, v.FMemoryOperand)
        else
          compiler.movapd(xmm, v.FMemoryOperand);
      end;

      VARIABLE_TYPE_MM:
      begin
        mm := mk_mm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movq(mm, mk_mm(old))
        else
          compiler.movq(mm, v.FMemoryOperand);
      end;

      VARIABLE_TYPE_XMM:
      begin
        xmm := mk_xmm(v.FRegisterCode);
        if (old <> NO_REG) then
          compiler.movdqa(xmm, mk_xmm(old))
        else if (Naked) then
          compiler.movdqu(xmm, v.FMemoryOperand)
        else
          compiler.movdqa(xmm, v.FMemoryOperand);
      end;
    end;

    if (old <> NO_REG) then
    begin
      Inc(v.FRegisterAccessCount);
      Inc(v.FGlobalRegisterAccessCount);
    end
    else
    begin
      Inc(v.FMemoryAccessCount);
      Inc(v.FGlobalMemoryAccessCount);
    end;
  end;

  if (compiler.Logger <> nil) then
    compiler._inlineComment('');
end;

procedure TFunction._allocReg(code: UInt8; v: PVariable);
var
  ATyp, mask: UInt32;
begin
  ATyp := code and REGTYPE_MASK;
  mask := SysUInt(1) shl (code and REGCODE_MASK);

  case ATyp of
    REG_GPB, REG_GPW, REG_GPD, REG_GPQ:
    begin
      useGpRegisters(mask);
      modifyGpRegisters(mask);
      FState.gp[code and $0F] := v;
    end;

    REG_MM:
    begin
      useMmRegisters(mask);
      modifyMmRegisters(mask);
      FState.mm[code and $0F] := v;
    end;

    REG_XMM:
    begin
      useXmmRegisters(mask);
      modifyXmmRegisters(mask);
      FState.xmm[code and $0F] := v;
    end;
  end;

  v.FHomeRegisterCode := code;
end;

procedure TFunction._freeReg(code: UInt8);
var
  ATyp, mask: UInt32;
begin
  ATyp := code and REGTYPE_MASK;
  mask := SysUInt(1) shl (code and REGCODE_MASK);

  case ATyp of
    REG_GPB, REG_GPW, REG_GPD, REG_GPQ:
    begin
      unuseGpRegisters(mask);
      FState.gp[code and $0F] := nil;
    end;

    REG_MM:
    begin
      unuseMmRegisters(mask);
      FState.mm[code and $0F] := nil;
    end;

    REG_XMM:
    begin
      unuseXmmRegisters(mask);
      FState.xmm[code and $0F] := nil;
    end;
  end;
end;

procedure TFunction._exchangeGp(v: PVariable; mode: UInt8; other: PVariable);
var
  code1, code2, type1, type2, index1, index2: UInt8;
  reg1, reg2: TRegister;
begin
  Assert(v.state = VARIABLE_STATE_REGISTER);
  Assert(other.state = VARIABLE_STATE_REGISTER);

  code1 := v.registerCode;
  code2 := other.registerCode;

  type1 := code1 and REGTYPE_MASK;
  type2 := code2 and REGTYPE_MASK;

  index1 := code1 and REGCODE_MASK;
  index2 := code2 and REGCODE_MASK;

  Assert((type1 <= REG_GPQ) and (type2 <= REG_GPQ));

  reg1 := mk_gpn(index1);
  reg2 := mk_gpn(index2);

  if (mode = VARIABLE_ALLOC_WRITE) then
    compiler.mov(reg1, reg2)
  else
    compiler.xchg(reg1, reg2);

  v.FRegisterCode := index2 or type1;
  other.FRegisterCode := index1 or type2;

  FState.gp[index1] := other;
  FState.gp[index2] := v;

  Inc(v.FRegisterAccessCount);
  Inc(v.FGlobalRegisterAccessCount);

  Inc(other.FRegisterAccessCount);
  Inc(other.FGlobalRegisterAccessCount);
end;

procedure TFunction._postAlloc(v: PVariable; mode: UInt8);
begin
  if ((mode and VARIABLE_ALLOC_WRITE) <> 0) then
    v.FChanged := True;

  addPrevented(v);
end;

procedure TFunction.UseGpRegisters(Mask: UInt32);
begin
  FUsedGpRegisters := FUsedGpRegisters or Mask;
end;

procedure TFunction.UseMmRegisters(Mask: UInt32);
begin
  FUsedMmRegisters := FUsedMmRegisters or Mask;
end;

procedure TFunction.UseXmmRegisters(Mask: UInt32);
begin
  FUsedXmmRegisters := FUsedXmmRegisters or Mask;
end;

procedure TFunction.UnuseGpRegisters(Mask: UInt32);
begin
  FUsedGpRegisters := FUsedGpRegisters and (not Mask);
end;

procedure TFunction.UnuseMmRegisters(Mask: UInt32);
begin
  FUsedMmRegisters := FUsedMmRegisters and (not Mask);
end;

procedure TFunction.UnuseXmmRegisters(Mask: UInt32);
begin
  FUsedXmmRegisters := FUsedXmmRegisters and (not Mask);
end;

procedure TFunction.ModifyGpRegisters(Mask: UInt32);
begin
  FModifiedGpRegisters := FModifiedGpRegisters or Mask;
end;

procedure TFunction.ModifyMmRegisters(Mask: UInt32);
begin
  FModifiedMmRegisters := FModifiedMmRegisters or Mask;
end;

procedure TFunction.ModifyXmmRegisters(Mask: UInt32);
begin
  FModifiedXmmRegisters := FModifiedXmmRegisters or Mask;
end;

function TFunction.countOfGpRegistersToBeSaved: SysInt;
var
  count, i: SysInt;
begin
  count := 0;

  for i := 0 to NUM_REGS - 1 do
    if (((modifiedGpRegisters and (SysUInt(1) shl i)) <> 0) and ((cconvPreservedGp and (SysUInt(1) shl i)) <> 0) and (i <> (REG_NSP and REGCODE_MASK)) ) then
      Inc(count);

  Result := count;
end;

function TFunction.countOfXmmRegistersToBeSaved: SysInt;
var
  count, i: SysInt;
begin
  count := 0;

  for i := 0 to NUM_REGS - 1 do
    if (((modifiedXmmRegisters and (SysUInt(1) shl i)) <> 0) and ((cconvPreservedXmm and (SysUInt(1) shl i)) <> 0)) then
      Inc(count);

  Result := count;
end;

function TFunction.saveState: TState;
begin
  Result.Create(FCompiler, Self);
  TState__saveFunctionState(@Result.FData, Self);
end;

procedure TFunction.restoreState(s: TState);
var
  f_d, s_d: TData;
  base, i: SysInt;
  entryFrom, entryTo: TEntry;
  from_v, to_v: PVariable;
  regIndex, code: UInt8;
begin
  Assert(s.FFunction = Self);
  FUsePrevention := false;

  s_d := s.FData;

  TState__saveFunctionState(@f_d, Self);
  Base := 0;

  for i := 0 to 16+8+16 - 1 do
  begin
    if ((i = 16) or (i = 24)) then base := i;

    entryFrom := f_d.regs[i];
    entryTo   := s_d.regs[i];

    from_v := entryFrom.v;
    to_v := entryTo.v;

    if (from_v <> to_v) then
    begin
      regIndex := UInt8(i - base);

      if (from_v <> nil) then
        if ((entryFrom.lifeId <> from_v.lifeId) or (from_v.state = VARIABLE_STATE_UNUSED)) then
        begin
          _freeReg(getVariableRegisterCode(from_v.typ, regIndex));
          if (from_v.state = VARIABLE_STATE_REGISTER) then
            from_v.FState := VARIABLE_STATE_MEMORY;
        end
        else
          spill(from_v);
    end;
  end;

  base := 0;
  for i := 0 to 16+8+16 - 1 do
  begin
    if ((i = 16) or (i = 24)) then base := i;

    entryFrom := f_d.regs[i];
    entryTo   := s_d.regs[i];

    from_v := entryFrom.v;
    to_v := entryTo.v;

    if (from_v <> to_v) then
    begin
      regIndex := UInt8(i - base);

      if (to_v <> nil) then
      begin
        code := getVariableRegisterCode(to_v.typ, regIndex);
        _allocAs(to_v, VARIABLE_ALLOC_READ, code);
      end;
    end;

    if (to_v <> nil) then
      to_v.FChanged := entryTo.changed;
  end;

  FUsedGpRegisters  := s.FData.usedGpRegisters;
  FUsedMmRegisters  := s.FData.usedMmRegisters;
  FUsedXmmRegisters := s.FData.usedXmmRegisters;

  FUsePrevention := false;
  clearPrevented;
end;

procedure TFunction.setState(s: TState);
var
  i: SysUInt;
  old, v: PVariable;
begin
  Assert(s.FFunction = Self);

  for i := 0 to 16+8+16 - 1 do
  begin
    old := FState.regs[i];
    v := s.FData.regs[i].v;

    if ((v <> old) and (old <> nil)) then
    begin
      if (old.state = VARIABLE_STATE_REGISTER) then
      begin
        old.FState := VARIABLE_STATE_MEMORY;
        old.FRegisterCode := NO_REG;
        old.FChanged := True;
      end;
    end;

    if (v <> nil) then
    begin
      v.FState := s.FData.regs[i].state;
      v.FChanged := s.FData.regs[i].changed;
    end;

    FState.regs[i] := v;
  end;

  FUsedGpRegisters  := s.FData.usedGpRegisters;
  FUsedMmRegisters  := s.FData.usedMmRegisters;
  FUsedXmmRegisters := s.FData.usedXmmRegisters;

  s.FFunction.clearPrevented;
end;

{class} procedure TFunction__jmpAndRestore(c: TCompiler; Lbl: PLabel);
var
  jr: PJumpAndRestore;
  f: TFunction;
  backup: TState;
  L_block: PLabel;
  old, first, last: TEmittable;
  sFrom, sTo: TState;
  isJmp, modifiedState: Boolean;
begin
  jr := PJumpAndRestore(Lbl.CompilerData);
  f := jr.sFrom.FFunction;

  backup.Create(c, f);
  TState__saveFunctionState(@backup.FData, f);

  repeat
    sFrom := jr.sFrom;
    sTo := jr.sTo;

    isJmp := jr.instruction.code = INST_JMP;

    if isJmp then
      old := c.setCurrentEmittable(jr.instruction.prev)
    else
      old := c.setCurrentEmittable(c.lastEmittable);
    first := c.currentEmittable;

    f.setState(sFrom);
    f.restoreState(sTo);

    last := c.currentEmittable;
    modifiedState := old <> last;

    if (modifiedState and (not isJmp)) then
    begin
      L_block := c.newLabel;

      c.setCurrentEmittable(first);
      c.align(SizeOf(SysInt));
      c.bind(L_block);

      c.setCurrentEmittable(last);
      c.jmp(Lbl);

      jr.instruction.Fo[0] := L_block;
    end;

    c.setCurrentEmittable(old);
    jr := jr.next;
  until (jr = nil);

  Lbl.CompilerData := nil;
  f.setState(backup);
  //backup.Free;
end;

function alignTo16Bytes(x: SysInt): SysInt;
begin
  Result := (x + 15) and (not 15);
end;

function getStackSize(f: TFunction; stackAdjust: SysInt): SysInt;
var
  stackSize, stackAlignment: SysInt;
begin
  stackSize := alignTo16Bytes(f.variablesStackSize) + f.prologEpilogStackSize;

{$IFDEF ASMJIT_X86}
  stackAlignment := f.stackAlignmentSize;
{$ELSE}
  stackAlignment := 16;
{$ENDIF}

  if (stackAlignment > 0) then
    stackSize := (stackSize + stackAlignment - 1) and (not (stackAlignment - 1));

  Result := stackSize;
end;

constructor TProlog.Create(c: TCompiler; f: TFunction);
begin
  inherited Create(c, EMITTABLE_PROLOGUE);
  FFunction := f;
end;

procedure TProlog.emit(a: TAssembler);
var
  f: TFunction;
  isStackAlignedTo16Bytes: Boolean;
  stackAdjust, stackSize, stackSubtract, i, nspPos: SysInt;
begin
  //f.Create(FCompiler);
  //f := TFunction.Create(FCompiler);
  f := FFunction;

  isStackAlignedTo16Bytes := SizeOf(SysInt) = 8;
  if (f.Naked) then
    if (SizeOf(SysInt) = 8) then
      stackAdjust := 8
    else
      stackAdjust := 12
  else
    if (SizeOf(SysInt) = 8) then
      stackAdjust := 0
    else
      stackAdjust := 8;

  stackSize := getStackSize(f, stackAdjust);
  stackSubtract := stackSize;

  if (not f.Naked) then
  begin
    a.push(nbp);
    a.mov(nbp, nsp);
  end;

  if (f.PrologEpilogPushPop) then
  begin
    for i := 0 to NUM_REGS - 1 do
      if (((f.modifiedGpRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedGp and (SysUInt(1) shl i)) <> 0) and (i <> (REG_NSP and REGCODE_MASK)) ) then
        a.push(mk_gpn(i));
    Dec(stackSubtract, f.countOfGpRegistersToBeSaved * SizeOf(SysInt));
  end;

  if (not f.naked) then
  begin
    if (stackSubtract <> 0) then a.sub(nsp, imm(stackSubtract));

{$IFDEF ASMJIT_X86}
    if ((stackSize <> 0) and (f.stackAlignmentSize <> 0)) then
    begin
      a.and_(nsp, imm(-(Int32(f.stackAlignmentSize))));
      isStackAlignedTo16Bytes := True;
    end;
{$ENDIF}
  end;

  nspPos := alignTo16Bytes(f.variablesStackSize);
  if (f.Naked) then nspPos := nspPos - stackSize;

  for i := 0 to NUM_REGS - 1 do
  begin
    if (((f.modifiedXmmRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedXmm and (SysUInt(1) shl i)) <> 0)) then
    begin
      if (isStackAlignedTo16Bytes) then
        a.movdqa(dqword_ptr(nsp, nspPos), mk_xmm(i))
      else
        a.movdqu(dqword_ptr(nsp, nspPos), mk_xmm(i));
      nspPos := nspPos + 16;
    end;
  end;

  if (not f.prologEpilogPushPop) then
  begin
    for i := 0 to NUM_REGS - 1 do
      if (((f.modifiedGpRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedGp and (SysUInt(1) shl i)) <> 0) and (i <> (REG_NSP and REGCODE_MASK)) ) then
      begin
        a.mov(sysint_ptr(nsp, nspPos), mk_gpn(i));
        Inc(nspPos, SizeOf(SysInt));
      end;
  end;

  if (FLabel <> nil) then a.bind(FLabel);
  //f.Free;
end;

constructor TEpilog.Create(c: TCompiler; f: TFunction);
begin
  inherited Create(c, EMITTABLE_EPILOGUE);
  FFunction := f;
end;

procedure TEpilog.emit(a: TAssembler);
var
  f: TFunction;
  ci: TCpuInfo;
  isStackAlignedTo16Bytes, emitLeave: Boolean;
  stackAdjust, stackSize, i, nspPos, stackAdd: SysInt;
begin
  //f.Create(FCompiler);
  //f := TFunction.Create(FCompiler);
  f := FFunction;
  ci := getCpuInfo;

  isStackAlignedTo16Bytes := SizeOf(SysInt) = 8;

  if ((f.naked) and (SizeOf(SysInt) = 8)) then
    stackAdjust := 8
  else
    stackAdjust := 0;
  stackSize := getStackSize(f, stackAdjust);

{$IFDEF ASMJIT_X86}
  if ((not f.naked) and (stackSize <> 0) and (f.stackAlignmentSize <> 0)) then
    isStackAlignedTo16Bytes := true;
{$ENDIF}

  if (FLabel <> nil) then a.bind(FLabel);

  nspPos := alignTo16Bytes(f.variablesStackSize);
  if (f.Naked) then nspPos := nspPos - stackSize;

  for i := 0 to NUM_REGS - 1 do
    if (((f.modifiedXmmRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedXmm and (SysUInt(1) shl i)) <> 0)) then
    begin
      if (isStackAlignedTo16Bytes) then
        a.movdqa(mk_xmm(i), dqword_ptr(nsp, nspPos))
      else
        a.movdqu(mk_xmm(i), dqword_ptr(nsp, nspPos));
      nspPos := nspPos + 16;
    end;

  if (not f.prologEpilogPushPop) then
  begin
    for i := 0 to NUM_REGS - 1 do
      if (((f.modifiedGpRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedGp and (SysUInt(1) shl i)) <> 0) and (i <> (REG_NSP and REGCODE_MASK)) ) then
      begin
        a.mov(mk_gpn(i), sysint_ptr(nsp, nspPos));
        nspPos := nspPos + SizeOf(SysInt);
      end;
  end
  else
  begin
    if (not f.naked) then
    begin
      stackAdd := stackSize - (f.countOfGpRegistersToBeSaved * SizeOf(SysInt));
      if (stackAdd <> 0) then a.add(nsp, imm(stackAdd));
    end;

    for i := NUM_REGS downto 0 do
      if (((f.modifiedGpRegisters and (SysUInt(1) shl i)) <> 0) and ((f.cconvPreservedGp and (SysUInt(1) shl i)) <> 0) and (i <> (REG_NSP and REGCODE_MASK)) ) then
        a.pop(mk_gpn(i));
  end;

  if (f.emms) then a.emms;

  if ((f.sfence) and (not f.lfence)) then a.sfence;
  if ((not f.sfence) and (f.lfence)) then a.lfence;
  if ((f.sfence) and (f.lfence)) then a.mfence;

  if (not f.naked) then
  begin
    emitLeave := ((f.optimizedPrologEpilog) and (ci.vendorId = DAsmJit_CpuInfo.Vendor_AMD));

    if (emitLeave) then
      a.leave
    else
    begin
      a.mov(nsp, nbp);
      a.pop(nbp);
    end;
  end;

  if (f.calleePopsStack) then
    a.ret(imm(Int16(f.argumentsStackSize)))
  else
    a.ret;

  //f.Free;
end;

constructor TTarget.Create(c: TCompiler; target: PLabel);
begin
  inherited Create(c, EMITTABLE_TARGET);
  FTarget := target;
end;

procedure TTarget.emit(a: TAssembler);
begin
  a.bind(FTarget);
end;

constructor TJumpTable.Create(c: TCompiler);
begin
  inherited Create(c, EMITTABLE_TARGET);
  FTarget := c.newLabel;
  FLabels := TPodVector.Create{(SizeOf(PLabel))};
end;

destructor TJumpTable.Destroy;
begin
  FLabels.Free;

  inherited Destroy;
end;

procedure TJumpTable.emit(a: TAssembler);
begin
end;

procedure TJumpTable.postEmit(a: TAssembler);
var
  i, len: SysUInt;
  Lbl: PLabel;
begin
  a.align(SizeOf(SysInt));

{$IFDEF ASMJIT_X64}
  a._embedLabel(FTarget);
{$ENDIF}

  a.bind(FTarget);

  len := FLabels.length;
  if (len > 0) then
    for i := 0 to len - 1 do
    begin
      Lbl := FLabels[i];
      if (Lbl <> nil) then
        a._embedLabel(Lbl)
      else
        a.dsysint(0);
    end;
end;

function TJumpTable.addLabel(ATarget: PLabel; pos: SysInt): PLabel;
begin
  if (ATarget = nil) then
    ATarget := compiler.newLabel;

  if (pos > -1) then
  begin
    while (FLabels.length <= SysUInt(pos)) do FLabels.append(nil);
    FLabels[SysUInt(pos)] := ATarget;
  end
  else
    FLabels.append(ATarget);

  Result := ATarget;
end;

constructor TCompilerCore.Create;
begin
  inherited Create;

  FOperands := TPodVector.Create{(SizeOf(POperand))};
  FJumpTableData := TPodVector.Create{(SizeOf(Pointer))};

  FFirst := nil;
  FLast := nil;
  FCurrent := nil;
  FCurrentFunction := nil;
  FLabelIdCounter := 1;
  FInlineCommentBuffer := '';
  FJumpTableLabel := newLabel^;
end;

destructor TCompilerCore.Destroy;
var
  i: Integer;
begin
  delAll(FFirst);

  FOperands.Free;
  FJumpTableData.Free;

  inherited Destroy;
end;

procedure TCompilerCore.clear;
begin
  delAll(FFirst);

  FFirst := nil;
  FLast := nil;
  FCurrent := nil;

  //FZone.freeAll;

  FOperands.clear;
  FJumpTableLabel := newLabel^;
  FJumpTableData.clear;
end;

procedure TCompilerCore.DoFree;
begin
  clear;
  FOperands.DoFree;
  FJumpTableData.DoFree;
end;

procedure TCompilerCore.addEmittable(emittable: TEmittable);
var
  prev, next: TEmittable;
begin
  Assert(emittable <> nil);
  Assert(emittable.FPrev = nil);
  Assert(emittable.FNext = nil);

  if (FCurrent = nil) then
  begin
    if (FFirst = nil) then
    begin
      FFirst := emittable;
      FLast := emittable;
    end
    else
    begin
      emittable.FNext := FFirst;
      FFirst.FPrev := emittable;
      FFirst := emittable;
    end;
  end
  else
  begin
    prev := FCurrent;
    next := FCurrent.FNext;

    emittable.FPrev := prev;
    emittable.FNext := next;

    prev.FNext := emittable;
    if (next <> nil) then
      next.FPrev := emittable
    else
      FLast := emittable;
  end;

  FCurrent := emittable;
end;

procedure TCompilerCore.removeEmittable(emittable: TEmittable);
var
  prev, next: TEmittable;
begin
  prev := emittable.FPrev;
  next := emittable.FNext;

  if (FFirst = emittable) then begin FFirst := next; end else begin prev.FNext := next; end;
  if (FLast  = emittable) then begin FLast  := prev; end else begin next.FPrev := prev; end;

  emittable.FPrev := nil;
  emittable.FNext := nil;

  if (emittable = FCurrent) then FCurrent := prev;

  emittable.Free;
end;

function TCompilerCore.setCurrentEmittable(current: TEmittable): TEmittable;
var
  old: TEmittable;
begin
  old := FCurrent;
  FCurrent := current;
  Result := old;
end;

procedure TCompilerCore.comment(fmt: string; Args: array of const);
var
  buf: string;
  c: TComment;
begin
  if (fmt <> '') then
    buf := buf + '; ' + Format(fmt, Args);

  buf := buf + LineEnding;
  //new(c);
  //c.Create(TCompiler(Self), buf);
  c := TComment.Create(TCompiler(Self), buf);
  addEmittable(c);
end;

function TCompilerCore.newFunction_(CConv: UInt32; args: PUInt32 = nil; count: SysUInt = 0): TFunction;
var
  f: TFunction;
  e: TProlog;
begin
  Assert(FCurrentFunction = nil);

  //new(FCurrentFunction);
  //FCurrentFunction.Create(TCompiler(Self));
  FCurrentFunction := TFunction.Create(TCompiler(Self));
  f := FCurrentFunction;
  f.setPrototype(CConv, args, count);

  addEmittable(f);

  e := newProlog(f);
  e.FLabel := @f.FPrologLabel;

  Result := f;
end;

function TCompilerCore.endFunction: TFunction;
var
  f: TFunction;
  e: TEpilog;
begin
  Assert(FCurrentFunction <> nil);
  f := FCurrentFunction;

  f.clearPrevented;

  e := newEpilog(f);
  e.FLabel := @f.FExitLabel;

  FCurrentFunction := nil;
  Result := f;
end;

function TCompilerCore.newProlog(f: TFunction): TProlog;
begin
  //New(Result);
  //Result.Create(TCompiler(Self), f);
  Result := TProlog.Create(TCompiler(Self), f);
  addEmittable(Result);
end;

function TCompilerCore.newEpilog(f: TFunction): TEpilog;
begin
  //New(Result);
  //Result.Create(TCompiler(Self), f);
  Result := TEpilog.Create(TCompiler(Self), f);
  addEmittable(Result);
end;

function TCompilerCore.argument(i: SysInt): PVariable;
begin
  Result := currentFunction.argument(i);
end;

function TCompilerCore.newVariable(typ, priority, preferredRegister: UInt8): PVariable;
begin
  Result := currentFunction.newVariable(typ, priority, preferredRegister);
end;

function TCompilerCore.alloc(v: PVariable; mode, preferredRegister: UInt8): Boolean;
begin
  Result := currentFunction.alloc(v, mode, preferredRegister);
end;

function TCompilerCore.spill(v: PVariable): Boolean;
begin
  Result := currentFunction.spill(v);
end;

procedure TCompilerCore.unuse(v: PVariable);
begin
  currentFunction.unuse(v);
end;

procedure TCompilerCore.spillAll;
begin
  currentFunction.spillAll;
end;

procedure TCompilerCore.spillAllGp;
begin
  currentFunction.SpillAllGp;
end;

procedure TCompilerCore.spillAllMm;
begin
  currentFunction.spillAllMm;
end;

procedure TCompilerCore.spillAllXmm;
begin
  currentFunction.spillAllXmm;
end;

procedure TCompilerCore.spillRegister(reg: TBaseReg);
begin
  currentFunction.spillRegister(reg);
end;

function TCompilerCore.numFreeGp: SysInt;
begin
  Result := FCurrentFunction.numFreeGp;
end;

function TCompilerCore.numFreeMm: SysInt;
begin
  Result := FCurrentFunction.numFreeMm;
end;

function TCompilerCore.numFreeXmm: SysInt;
begin
  Result := FCurrentFunction.numFreeXmm;
end;

function TCompilerCore.isPrevented(v: PVariable): Boolean;
begin
  Result := currentFunction.isPrevented(v);
end;

procedure TCompilerCore.addPrevented(v: PVariable);
begin
  currentFunction.addPrevented(v);
end;

procedure TCompilerCore.removePrevented(v: PVariable);
begin
  currentFunction.removePrevented(v);
end;

procedure TCompilerCore.clearPrevented;
begin
  currentFunction.clearPrevented;
end;

function TCompilerCore.saveState: TState;
begin
  Result := currentFunction.saveState;
end;

procedure TCompilerCore.restoreState(state: TState);
begin
  currentFunction.restoreState(state);
end;

procedure TCompilerCore.setState(state: TState);
begin
  currentFunction.setState(state);
end;

function TCompilerCore.newLabel: PLabel;
begin
  New(Result);
  FMemZone.append(Result);
  Result.Create(UInt16(FLabelIdCounter));
  Inc(FLabelIdCounter);
  _registerOperand(Result);
end;

function TCompilerCore.newJumpTable: TJumpTable;
begin
  //New(Result);
  //Result.Create(TCompiler(Self));
  Result := TJumpTable.Create(TCompiler(Self));
  addEmittable(Result);
end;

procedure TCompilerCore._registerOperand(op: POperand);
begin
  op.OperandID := FOperands.length;
  FOperands.append(op);
end;

procedure TCompilerCore.jumpToTable(jt: TJumpTable; index: TRegister);
begin
{$IFDEF ASMJIT_X64}
  shl_(index, imm(3));
  add(index, ptr(jt.target, -8));
  jmp(ptr(index));
{$ELSE}
  jmp(ptr(jt.target, index, TIMES_4));
{$ENDIF}
end;

function TCompilerCore._addTarget(target: Pointer): SysInt;
begin
  Result := FJumpTableData.length * SizeOf(SysInt);
  FJumpTableData.append(target);
end;

procedure TCompilerCore._jmpAndRestore(code: UInt32; Lbl: PLabel; state: TState);
var
  jr: PJumpAndRestore;
begin
  //GetMem(jr, SizeOf(TJumpAndRestore));
  New(jr);
  jr.next := PJumpAndRestore(Lbl.CompilerData);
  jr.sFrom := currentFunction.saveState;
  jr.sTo := state;
  Lbl.CompilerData := jr;

  emitX86(code, Lbl^);
  jr.instruction := TInstruction(FCurrent);
  FMemZone.append(jr);
end;

procedure TCompilerCore.op_var32(code: UInt32; a: TInt32Ref);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r32;
    emitX86(code, ar);
  end
  else
    emitX86(code, a.m);
end;

procedure TCompilerCore.op_reg32_var32(code: UInt32; a: TRegister; b: TInt32Ref);
var
  br: TRegister;
begin
  if (b.state = VARIABLE_STATE_REGISTER) then
  begin
    br := b.r32;
    emitX86(code, a, br);
  end
  else
    emitX86(code, a, b.m);
end;

procedure TCompilerCore.op_var32_reg32(code: UInt32; a: TInt32Ref; b: TRegister);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r32;
    emitX86(code, ar, b);
  end
  else
    emitX86(code, a.m, b);
end;

procedure TCompilerCore.op_var32_imm(code: UInt32; a: TInt32Ref; b: TImmediate);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r32;
    emitX86(code, ar, b);
  end
  else
    emitX86(code, a.m, b);
end;

{$IFDEF ASMJIT_X64}
procedure TCompilerCore.op_var64(code: UInt32; a: TInt64Ref);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r64;
    emitX86(code, ar);
  end
  else
    emitX86(code, a.m);
end;

procedure TCompilerCore.op_reg64_var64(code: UInt32; a: TRegister; b: TInt64Ref);
var
  br: TRegister;
begin
  if (b.state = VARIABLE_STATE_REGISTER) then
  begin
    br := b.r64;
    emitX86(code, a, br);
  end
  else
    emitX86(code, a, b.m);
end;

procedure TCompilerCore.op_var64_reg64(code: UInt32; a: TInt64Ref; b: TRegister);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r64;
    emitX86(code, ar, b);
  end
  else
    emitX86(code, a.m, b);
end;

procedure TCompilerCore.op_var64_imm(code: UInt32; a: TInt64Ref; b: TImmediate);
var
  ar: TRegister;
begin
  if (a.state = VARIABLE_STATE_REGISTER) then
  begin
    ar := a.r64;
    emitX86(code, ar, b);
  end
  else
    emitX86(code, a.m, b);
end;
{$ENDIF}

procedure TCompilerCore._inlineComment(_text: string);
begin
  FInlineCommentBuffer := _text;
end;

procedure TCompilerCore._emitX86(code: UInt32; o1, o2, o3: POperand);
var
  i: TInstruction;
begin
  //new(i);
  //i.Create(TCompiler(Self), code, o1, o2, o3, FInlineCommentBuffer);
  i := TInstruction.Create(TCompiler(Self), code, o1, o2, o3, FInlineCommentBuffer);

  addEmittable(i);
  FInlineCommentBuffer := '';

  if (currentFunction <> nil) then currentFunction.clearPrevented;
end;

procedure TCompilerCore._embed(data: Pointer; size: SysUInt);
var
  capacity: SysUInt;
  e: TEmbeddedData;
begin
  if ((data <> nil) and (size > 0)) then
  begin
    capacity := (size + 15) and (not 15);

    //New(e);
    //e.Create(TCompiler(Self), capacity, data, size);
    e := TEmbeddedData.Create(TCompiler(Self), capacity, data, size);
    addEmittable(e);
  end;
end;

procedure TCompilerCore.align(m: SysInt);
var
  a: TAlign;
begin
  //new(a);
  //a.Create(TCompiler(Self), m);
  a := TAlign.Create(TCompiler(Self), m);
  addEmittable(a);
end;

procedure TCompilerCore.bind(Lbl: PLabel);
var
  t: TTarget;
begin
  if (Lbl.compilerData <> nil) then TFunction__jmpAndRestore(TCompiler(Self), Lbl);

  //New(t);
  //t.Create(TCompiler(Self), Lbl);
  t := TTarget.Create(TCompiler(Self), Lbl);
  addEmittable(t);
end;

function TCompilerCore.make(MemManager: TMemoryManager = nil; AllocType: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer;
var
  a: TAssembler;
begin
  a := TAssembler.Create;
  a.Properties := FProperties;
  Serialize(a);

  if (a.Error > 0) then
  begin
    if (FLogger <> nil) then
      FLogger.logFormat('; Compiler failed: %s (%u).' + LineEnding + LineEnding, [errorCodeToString(a.error), SysUInt(a.error)]);

    setError(a.error);
    Result := nil;
  end
  else
  begin
    if (FLogger <> nil) then
      FLogger.logFormat('; Compiler successful (wrote %u bytes).' + LineEnding + LineEnding, [SysUInt(a.codeSize)]);

    Result := a.make(MemManager, allocType);
  end;
  a.Free;
end;

type
  TLoggerSwitcher = class
  public
    a: TAssembler;
    DAsmJit_Logger: TCustomLogger;

    constructor Create(AAssembler: TAssembler; c: TCompiler);
    destructor Destroy; override;
  end;

constructor TLoggerSwitcher.Create(AAssembler: TAssembler; c: TCompiler);
begin
  inherited Create;

  a := AAssembler;
  DAsmJit_Logger := AAssembler.Logger;
  if (DAsmJit_Logger = nil) and (c.Logger <> nil) then
    AAssembler.Logger := c.Logger;
end;

destructor TLoggerSwitcher.Destroy;
begin
  a.Logger := DAsmJit_Logger;

  inherited;
end;

procedure TCompilerCore.Serialize(a: TAssembler);
var
  LoggerSwitcher: TLoggerSwitcher;
  cur: TEmittable;
  i, len: SysUInt;
begin
  LoggerSwitcher := TLoggerSwitcher.Create(a, TCompiler(Self));

  cur := FFirst;
  while (cur <> nil) do
  begin
    cur.prepare;
    cur := cur.next;
  end;

  cur := FFirst;
  while (cur <> nil) do
  begin
    cur.emit(a);
    cur := cur.next;
  end;

  cur := FFirst;
  while (cur <> nil) do
  begin
    cur.postemit(a);
    cur := cur.next;
  end;

  a.bind(@FJumpTableLabel);

  len := FJumpTableData.length;
  if (len > 0) then
    for i := 0 to len - 1 do
      a.dptr(FJumpTableData[i]);

  LoggerSwitcher.Free;
end;

procedure TCompilerIntrinsics.jAndRestore(cc: SysInt; Lbl: PLabel; State: TState);
begin
  Assert(cc <= $F);
  _jmpAndRestore(_jcctable[cc], Lbl, State);
end;

procedure TCompilerIntrinsics.jaAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JA  , Lbl, state);
end;

procedure TCompilerIntrinsics.jaeAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JAE , Lbl, state);
end;

procedure TCompilerIntrinsics.jbAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JB  , Lbl, state);
end;

procedure TCompilerIntrinsics.jbeAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JBE , Lbl, state);
end;

procedure TCompilerIntrinsics.jcAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JC  , Lbl, state);
end;

procedure TCompilerIntrinsics.jeAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JE  , Lbl, state);
end;

procedure TCompilerIntrinsics.jgAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JG  , Lbl, state);
end;

procedure TCompilerIntrinsics.jgeAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JGE , Lbl, state);
end;

procedure TCompilerIntrinsics.jlAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JL  , Lbl, state);
end;

procedure TCompilerIntrinsics.jleAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JLE , Lbl, state);
end;

procedure TCompilerIntrinsics.jnaAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNA , Lbl, state);
end;

procedure TCompilerIntrinsics.jnaeAndRestore(Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNAE, Lbl, state);
end;

procedure TCompilerIntrinsics.jnbAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNB , Lbl, state);
end;

procedure TCompilerIntrinsics.jnbeAndRestore(Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNBE, Lbl, state);
end;

procedure TCompilerIntrinsics.jncAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNC , Lbl, state);
end;

procedure TCompilerIntrinsics.jneAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNE , Lbl, state);
end;

procedure TCompilerIntrinsics.jngAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNG , Lbl, state);
end;

procedure TCompilerIntrinsics.jngeAndRestore(Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNGE, Lbl, state);
end;

procedure TCompilerIntrinsics.jnlAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNL , Lbl, state);
end;

procedure TCompilerIntrinsics.jnleAndRestore(Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNLE, Lbl, state);
end;

procedure TCompilerIntrinsics.jnoAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNO , Lbl, state);
end;

procedure TCompilerIntrinsics.jnpAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNP , Lbl, state);
end;

procedure TCompilerIntrinsics.jnsAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNS , Lbl, state);
end;

procedure TCompilerIntrinsics.jnzAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JNZ , Lbl, state);
end;

procedure TCompilerIntrinsics.joAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JO  , Lbl, state);
end;

procedure TCompilerIntrinsics.jpAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JP  , Lbl, state);
end;

procedure TCompilerIntrinsics.jpeAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JPE , Lbl, state);
end;

procedure TCompilerIntrinsics.jpoAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JPO , Lbl, state);
end;

procedure TCompilerIntrinsics.jsAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JS  , Lbl, state);
end;

procedure TCompilerIntrinsics.jzAndRestore  (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JZ  , Lbl, state);
end;

procedure TCompilerIntrinsics.jmpAndRestore (Lbl: PLabel; State: TState);
begin
  _jmpAndRestore(INST_JMP , Lbl, state);
end;


procedure TCompilerIntrinsics.adc(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_ADC, dst, src);
end;

procedure TCompilerIntrinsics.adc(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_ADC, dst, src);
end;

procedure TCompilerIntrinsics.adc(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_ADC, dst, src);
end;


procedure TCompilerIntrinsics.add(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_ADD, dst, src);
end;

procedure TCompilerIntrinsics.add(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_ADD, dst, src);
end;

procedure TCompilerIntrinsics.add(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_ADD, dst, src);
end;


procedure TCompilerIntrinsics.and_(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_AND, dst, src);
end;

procedure TCompilerIntrinsics.and_(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_AND, dst, src);
end;

procedure TCompilerIntrinsics.and_(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_AND, dst, src);
end;


procedure TCompilerIntrinsics.cmp(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_CMP, dst, src);
end;

procedure TCompilerIntrinsics.cmp(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_CMP, dst, src);
end;

procedure TCompilerIntrinsics.cmp(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_CMP, dst, src);
end;


procedure TCompilerIntrinsics.dec_(dst: TInt32Ref);
begin
  op_var32(INST_DEC, dst);
end;

procedure TCompilerIntrinsics.inc_(dst: TInt32Ref);
begin
  op_var32(INST_INC, dst);
end;

procedure TCompilerIntrinsics.neg(dst: TInt32Ref);
begin
  op_var32(INST_NEG, dst);
end;

procedure TCompilerIntrinsics.not_(dst: TInt32Ref);
begin
  op_var32(INST_NOT, dst);
end;

procedure TCompilerIntrinsics.mov(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.mov(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.mov(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TRegister; src: TInt32Ref);
begin
  op_reg32_var32(INST_XOR, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TInt32Ref; src: TRegister);
begin
  op_var32_reg32(INST_XOR, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TInt32Ref; src: TImmediate);
begin
  op_var32_imm(INST_XOR, dst, src);
end;

{$IFDEF ASMJIT_X64}
procedure TCompilerIntrinsics.adc(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_ADC, dst, src);
end;

procedure TCompilerIntrinsics.adc(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_ADC, dst, src);
end;

procedure TCompilerIntrinsics.adc(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_ADC, dst, src);
end;

procedure TCompilerIntrinsics.add(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_ADD, dst, src);
end;

procedure TCompilerIntrinsics.add(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_ADD, dst, src);
end;

procedure TCompilerIntrinsics.add(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_ADD, dst, src);
end;

procedure TCompilerIntrinsics.and_(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_AND, dst, src);
end;

procedure TCompilerIntrinsics.and_(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_AND, dst, src);
end;

procedure TCompilerIntrinsics.and_(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_AND, dst, src);
end;

procedure TCompilerIntrinsics.cmp(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_CMP, dst, src);
end;

procedure TCompilerIntrinsics.cmp(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_CMP, dst, src);
end;

procedure TCompilerIntrinsics.cmp(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_CMP, dst, src);
end;

procedure TCompilerIntrinsics.dec_(dst: TInt64Ref);
begin
  op_var64(INST_DEC, dst);
end;

procedure TCompilerIntrinsics.inc_(dst: TInt64Ref);
begin
  op_var64(INST_INC, dst);
end;

procedure TCompilerIntrinsics.neg(dst: TInt64Ref);
begin
  op_var64(INST_NEG, dst);
end;

procedure TCompilerIntrinsics.not_(dst: TInt64Ref);
begin
  op_var64(INST_NOT, dst);
end;

procedure TCompilerIntrinsics.mov(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.mov(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.mov(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_MOV, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.or_(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_OR, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sbb(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_SBB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.sub(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_SUB, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TRegister; src: TInt64Ref);
begin
  op_reg64_var64(INST_XOR, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TInt64Ref; src: TRegister);
begin
  op_var64_reg64(INST_XOR, dst, src);
end;

procedure TCompilerIntrinsics.xor_(dst: TInt64Ref; src: TImmediate);
begin
  op_var64_imm(INST_XOR, dst, src);
end;
{$ENDIF}

end.
