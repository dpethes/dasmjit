unit DAsmJit_Assembler;

{$I DAsmJit.inc}

interface

uses
  DAsmJit, DAsmJit_Serializer, DAsmJit_Util, DAsmJit_Defs, DAsmJit_CpuInfo,
  DAsmJit_Logger, DAsmJit_MemoryManager, DAsmJit_VirtualMemory;

{$IFDEF ASMJIT_X64}
const
  TRAMPOLINE_JMP = 6;
  TRAMPOLINE_ADDR = SizeOf(SysInt);
  TRAMPOLINE_SIZE = TRAMPOLINE_JMP + TRAMPOLINE_ADDR;
{$ENDIF}

type
  PLinkData = ^TLinkData;
  TLinkData = record
    Prev: PLinkData;
    OffSet: SysInt;
    Displacement: SysInt;
    RelocID: SysInt;
  end;

  TRelocDataE = (ABSOLUTE_TO_ABSOLUTE, RELATIVE_TO_ABSOLUTE, ABSOLUTE_TO_RELATIVE, ABSOLUTE_TO_RELATIVE_TRAMPOLINE);
  PRelocData = ^TRelocData;
  TRelocData = record
    Typ: TRelocDataE;
    Size: UInt32;
    Offset: SysInt;
    case SysInt of
      0: (Destination: SysInt);
      1: (Address: Pointer);
  end;

  TAssembler = class(TSerializer)
  protected
    FLabel: TLabel;
    FLinkData: PLinkData;
    FBuffer: TBuffer;
    FTrampolineSize: SysInt;
    FUnUsedLinks: PLinkdata;
    FRelocData: TPodVector;
    FInlineCommentBuffer: string{[MAX_INLINE_COMMENT_SIZE]};
  public
    constructor Create; override;
    destructor Destroy; override;

    function Code: PUInt8;
    function EnsureSpace: Boolean;
    function Offset: SysInt;
    function CodeSize: SysInt;
    //function TrampolineSize: SysInt;
    function toOffset(o: SysInt): SysInt;
    function Capacity: SysInt;
    function ReAlloc(AllocTo: SysInt): Boolean;
    function Grow: Boolean;
    procedure Clear;
    procedure setVarAt(Pos, i: SysInt; isUnsigned: Boolean; Size: UInt32);
    function TakeCode: PUInt8;
    function getByteAt(Pos: SysInt): UInt8;
    function getWordAt(Pos: SysInt): UInt16;
    function getDWordAt(Pos: SysInt): UInt32;
    function getQWordAt(Pos: SysInt): UInt64;
    procedure setByteAt(Pos: SysInt; x: UInt8);
    procedure setWordAt(Pos: SysInt; x: UInt16);
    procedure setDWordAt(Pos: SysInt; x: UInt32);
    procedure setQWordAt(Pos: SysInt; x: UInt64);
    function getInt32At(Pos: SysInt): Int32;
    procedure setInt32At(Pos: SysInt; x: Int32);
    function canEmit: Boolean;
    procedure _emitByte(x: UInt8);
    procedure _emitWord(x: UInt16);
    procedure _emitDWord(x: UInt32);
    procedure _emitQWord(x: UInt64);
    procedure _emitInt32(x: Int32);
    procedure _emitSysInt(x: SysInt);
    procedure _emitSysUInt(x: SysUInt);
    procedure _emitImmediate(imm: TImmediate; Size: UInt32);
    procedure _emitOpCode(opCode: UInt32);
    procedure _emitSegmentPrefix(rm: TOperand);
    procedure _emitMod(m, o, r: UInt8);
    procedure _emitSib(s, i, b: UInt8);
    procedure _emitRexR(w: Boolean; opReg, regCode: UInt8);
    procedure _emitRexRM(w: Boolean; opReg: UInt8; rm: TOperand);
    procedure _emitModR(opReg, r: UInt8); overload;
    procedure _emitModR(opReg: UInt8; r: TBaseReg); overload;
    procedure _emitModM(opReg: UInt8; Mem: TMem; immSize: SysInt);
    procedure _emitModRM(opReg: UInt8; op: TOperand; immSize: SysInt);
    procedure _emitX86Inl(opCode: UInt32; i16bit, rexw: Boolean; reg: UInt8);
    procedure _emitX86RM(opCode: UInt32; i16bit, rexw: Boolean; o: UInt8; op: TOperand; immSize: SysInt);
    procedure _emitFpu(opCode: UInt32);
    procedure _emitFpuSTI(opCode, sti: UInt32);
    procedure _emitFpuMEM(opCode: UInt32; opReg: UInt8; Mem: TMem);
    procedure _emitMmu(opCode: UInt32; rexw: Boolean; opReg: UInt8; src: TOperand; immSize: SysInt);
    function _emitDisplacement(lbl: PLabel; inlinedDisplacement: SysInt; Size: Integer): PLinkData;
    procedure _emitJmpOrCallReloc(Instruction: UInt32; Target: Pointer);
    procedure relocCode(_dst: Pointer); virtual;
    procedure _inlineComment(Text: string); override;
    procedure _emitX86(Code: UInt32; o1, o2, o3: POperand); override;
    procedure _embed(data: Pointer; Size: SysUInt); override;
    procedure _EmbedLabel(lbl: PLabel); virtual;
    procedure Align(m: SysInt); override;
    function newLabel: TLabel;
    procedure Bind(Lbl: PLabel); override;
    procedure bindTo(Lbl: PLabel; Pos: SysInt);
    function Make(memManager: TMemoryManager = nil; AllocType: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer; override;
    function _newLinkData: PLinkData;
    procedure _freeLinkData(Link: PLinkData);

    property TrampolineSize: SysInt read FTrampolineSize;
    property Buffer: TBuffer read FBuffer;
  end;

implementation

uses
  SysUtils;

{$IFDEF ASMJIT_X64}
procedure WriteTrampoline(Code: PUInt8; Target: Pointer);
begin
  Code^ := $FF;
  PUInt8(PtrInt(Code) + 1)^ := $25;

  PUInt32(PtrInt(Code) + 2)^ := 0;
  PSysUInt(PtrInt(Code) + TRAMPOLINE_JMP)^ := SysUInt(target);
end;
{$ENDIF}

constructor TAssembler.Create;
begin
  inherited Create;

  FBuffer := TBuffer.Create(32);
  FRelocData := TPodVector.Create{(SizeOf(PRelocData))};
  FTrampolineSize := 0;
  FUnUsedLinks := nil;
  FInlineCommentBuffer := '';

  //FLabel := nil; initialize?
  //FLinkData := nil;
end;

destructor TAssembler.Destroy;
var
  i: Integer;
  a: PLinkData;
begin
  FRelocData.Free;
  FBuffer.Free;

  if (Error > 0) then
    ClearError;

  inherited;
end;

function TAssembler.Code: PUInt8;
begin
  Result := FBuffer.Data;
end;

function TAssembler.EnsureSpace: Boolean;
begin
  Result := FBuffer.EnsureSpace;
end;

function TAssembler.Offset: SysInt;
begin
  Result := FBuffer.Offset;
end;

function TAssembler.CodeSize: SysInt;
begin
  Result := FBuffer.Offset + TrampolineSize;
end;

{function TAssembler.TrampolineSize: SysInt;
begin
  Result := FTrampolineSize;
end;}

function TAssembler.toOffset(o: SysInt): SysInt;
begin
  Result := FBuffer.toOffset(o);
end;

function TAssembler.Capacity: SysInt;
begin
  Result := FBuffer.Capacity;
end;

function TAssembler.ReAlloc(AllocTo: SysInt): Boolean;
begin
  Result := FBuffer.realloc(AllocTo);
end;

function TAssembler.Grow: Boolean;
begin
  Result := FBuffer.grow;
end;

function TAssembler.getByteAt(Pos: SysInt): UInt8;
begin
  Result := FBuffer.getByteAt(Pos);
end;

function TAssembler.getWordAt(Pos: SysInt): UInt16;
begin
  Result := FBuffer.getWordAt(Pos);
end;

function TAssembler.getDWordAt(Pos: SysInt): UInt32;
begin
  Result := FBuffer.getDWordAt(Pos);
end;

function TAssembler.getQWordAt(Pos: SysInt): UInt64;
begin
  Result := FBuffer.getQWordAt(Pos);
end;

procedure TAssembler.setByteAt(Pos: SysInt; x: UInt8);
begin
  FBuffer.setByteAt(Pos, x);
end;

procedure TAssembler.setWordAt(Pos: SysInt; x: UInt16);
begin
  FBuffer.setWordAt(Pos, x);
end;

procedure TAssembler.setDWordAt(Pos: SysInt; x: UInt32);
begin
  FBuffer.setDWordAt(Pos, x);
end;

procedure TAssembler.setQWordAt(Pos: SysInt; x: UInt64);
begin
  FBuffer.setQWordAt(Pos, x);
end;

function TAssembler.getInt32At(Pos: SysInt): Int32;
begin
  Result := Int32(FBuffer.getDWordAt(Pos));
end;

procedure TAssembler.setInt32At(Pos: SysInt; x: Int32);
begin
  FBuffer.setDWordAt(Pos, Int32(x));
end;

procedure TAssembler._emitByte(x: UInt8);
begin
  FBuffer.emitByte(x);
end;

procedure TAssembler._emitWord(x: UInt16);
begin
  FBuffer.emitWord(x);
end;

procedure TAssembler._emitDWord(x: UInt32);
begin
  FBuffer.emitDWord(x);
end;

procedure TAssembler._emitQWord(x: UInt64);
begin
  FBuffer.emitQWord(x);
end;

procedure TAssembler._emitInt32(x: Int32);
begin
  FBuffer.emitDWord(UInt32(x));
end;

procedure TAssembler._emitSysInt(x: SysInt);
begin
  FBuffer.emitSysInt(x);
end;

procedure TAssembler._emitSysUInt(x: SysUInt);
begin
  FBuffer.emitSysUInt(x);
end;

procedure TAssembler._emitOpCode(opCode: UInt32);
begin
  if ((opCode and $FF000000) > 0) then _emitByte(UInt8((opCode and $FF000000) shr 24));
  if ((opCode and $00FF0000) > 0) then _emitByte(UInt8((opCode and $00FF0000) shr 16));
  if ((opCode and $0000FF00) > 0) then _emitByte(UInt8((opCode and $0000FF00) shr  8));
  _emitByte(UInt8(opCode and $000000FF));
end;

procedure TAssembler._emitMod(m, o, r: UInt8);
begin
  _emitByte(((m and $03) shl 6) or ((o and $07) shl 3) or (r and $07));
end;

procedure TAssembler._emitSib(s, i, b: UInt8);
begin
  _emitByte(((s and $03) shl 6) or ((i and $07) shl 3) or (b and $07));
end;

procedure TAssembler._emitRexR(w: Boolean; opReg, regCode: UInt8);
{$IFDEF ASMJIT_X64}
var
  r, b: Boolean;
{$ENDIF}
begin
{$IFDEF ASMJIT_X64}
  r := (opReg and $8) <> 0;
  b := (regCode and $8) <> 0;

  if (w or r or b or ((FProperties and (1 shl PROPERTY_X86_FORCE_REX)) <> 0)) then
    _emitByte($40 or (UInt8(w) shl 3) or (UInt8(r) shl 2) or UInt8(b));
{$ELSE}
  //Pointer(w);
  //Pointer(opReg);
  //Pointer(regCode);
{$ENDIF}
end;

procedure TAssembler._emitRexRM(w: Boolean; opReg: UInt8; rm: TOperand);
{$IFDEF ASMJIT_X64}
var
  r, x, b: Boolean;
{$ENDIF}
begin
{$IFDEF ASMJIT_X64}
  r := (opReg and $8) <> 0;
  x := False;
  b := False;

  if (rm.isReg) then
    b := (TBaseReg(rm).Code and $8) <> 0
  else if (rm.isMem) then
  begin
    x := ((TMem(rm).Index and $8) <> 0) and (TMem(rm).Index <> NO_REG);
    b := ((TMem(rm).Base and $8) <> 0) and (TMem(rm).Base <> NO_REG);
  end;

  if (w or r or x or b or ((FProperties and (1 shl PROPERTY_X86_FORCE_REX)) <> 0)) then
    _emitByte($40 or (UInt8(w) shl 3) or (UInt8(r) shl 2) or (UInt8(x) shl 1) or UInt8(b));
{$ELSE}
  //Pointer(w);
  //Pointer(opReg);
  //Pointer(rm);
{$ENDIF}
end;

procedure TAssembler._emitModR(opReg, r: UInt8);
begin
  _emitMod(3, opReg, r);
end;

procedure TAssembler._emitModR(opReg: UInt8; r: TBaseReg);
begin
  _emitMod(3, opReg, r.code);
end;

function TAssembler.TakeCode: PUInt8;
begin
  Result := FBuffer.Take;
  FRelocData.Clear;
  //FZone.Clear;

  if (Error > 0) then
    ClearError;
end;

procedure TAssembler.Clear;
begin
  FBuffer.Clear;
  FRelocData.Clear;
  //FZone.Clear;

  if (Error > 0) then
    ClearError;
end;

procedure TAssembler.setVarAt(Pos, i: SysInt; isUnsigned: Boolean; Size: UInt32);
begin
  case Size of
    1: if isUnsigned then
         setByteAt(Pos, UInt8(i))
       else
         setByteAt(Pos, Int8(i));
    2: if isUnsigned then
         setWordAt(Pos, UInt16(i))
       else
         setWordAt(Pos, Int16(i));
    4: if isUnsigned then
         setDWordAt(Pos, UInt32(i))
       else
         setDWordAt(Pos, Int32(i));
{$IFDEF ASMJIT_X64}
    8: if isUnsigned then
         setQWordAt(Pos, UInt64(i))
       else
         setQWordAt(Pos, Int64(i));
{$ENDIF}
    else Assert(False);
  end;
end;

function TAssembler.canEmit: Boolean;
begin
  if (Error > 0) then
  begin
    Result := False;
    Exit;
  end;

  // The ensureSpace method Result :=s true on success and false on failure. We
  // are catching Result := value and setting error code here.
  if (EnsureSpace) then
    Result := True
  else
  begin
    setError(ERROR_NO_HEAP_MEMORY);
    Result := False;
  end;
end;

procedure TAssembler._emitSegmentPrefix(rm: TOperand);
const
  Prefixes: array[0..6] of UInt8 = ($00, $2E, $36, $3E, $26, $64, $65);
var
  segmentPrefix: SysUInt;
begin
  if (rm.isMem) then
  begin
    segmentPrefix := TMem(rm).SegmentPrefix;
    if (segmentPrefix > 0) then
      _emitByte(prefixes[segmentPrefix]);
  end;
end;

procedure TAssembler._emitImmediate(imm: TImmediate; Size: UInt32);
var
  isUnsigned: Boolean;
  i: SysInt;
begin
  isUnsigned := imm.isUnsigned;
  i := imm.Value;

  if (imm.relocMode <> RELOC_NONE) then
  begin
    // TODO: I don't know why there is this condition.
  end;

  case Size of
    1: if isUnsigned then
         _emitByte(UInt8(i))
       else
         _emitByte(Int8(i));
    2: if isUnsigned then
         _emitWord(UInt16(i))
       else
         _emitWord(Int16(i));
    4: if isUnsigned then
         _emitDWord(UInt32(i))
       else
         _emitDWord(Int32(i));
{$IFDEF ASMJIT_X64}
    8: if isUnsigned then
         _emitQWord(UInt64(i))
       else
         _emitQWord(Int64(i));
{$ENDIF}
    else Assert(False);
  end;
end;

procedure TAssembler._emitModM(opReg: UInt8; Mem: TMem; immSize: SysInt);
var
  baseReg, indexReg, AMod: UInt8;
  Disp: SysInt;
{$IFDEF ASMJIT_X86}
  relocId: UInt32;
  rd: PRelocData;
{$ELSE}
  Target: SysUInt;
{$ENDIF}
  Shift: UInt32;
  Lbl: PLabel;
begin
  Assert(Mem.op = OP_MEM);

  baseReg := Mem.Base and $7;
  indexReg := Mem.Index and $7;
  Disp := Mem.Displacement;
  Shift := Mem.Shift;

  if (Mem.hasBase and (not Mem.hasIndex)) then
  begin
    if (baseReg = 4) then
    begin
      AMod := 0;

      if (Disp <> 0) then
        if isInt8(Disp) then
          AMod := 1
        else
          AMod := 2;

      _emitMod(AMod, opReg, 4);
      _emitSib(0, 4, 4);

      if (Disp <> 0) then
        if (isInt8(Disp)) then
          _emitByte(UInt8(Disp))
        else
          _emitInt32(Int32(Disp));
    end
    else if ((baseReg <> 5) and (Disp = 0)) then
      _emitMod(0, opReg, baseReg)
    else if (isInt8(Disp)) then
    begin
      _emitMod(1, opReg, baseReg);
      _emitByte(UInt8(Disp));
    end
    else
    begin
      _emitMod(2, opReg, baseReg);
      _emitInt32(Int32(Disp));
    end;
  end
  else if (Mem.hasBase and Mem.hasIndex) then
  begin
    if ((baseReg <> 5) and (Disp = 0)) then
    begin
      _emitMod(0, opReg, 4);
      _emitSib(Shift, indexReg, baseReg);
    end
    else if (isInt8(Disp)) then
    begin
      _emitMod(1, opReg, 4);
      _emitSib(Shift, indexReg, baseReg);
      _emitByte(UInt8(Disp));
    end
    else
    begin
      _emitMod(2, opReg, 4);
      _emitSib(Shift, indexReg, baseReg);
      _emitInt32(Int32(Disp));
    end
  end
  else
  begin

{$IFDEF ASMJIT_X86}
    if (Mem.hasIndex) then
    begin
      _emitMod(0, opReg, 4);
      _emitSib(Shift, indexReg, 5);
    end
    else
      _emitMod(0, opReg, 5);

    if (Mem.hasLabel) then
    begin
      Lbl := Mem.u._mem.Lbl;
      relocId := FRelocData.length;

      new(rd);
      rd.Typ := RELATIVE_TO_ABSOLUTE;
      rd.Size := 4;
      rd.Offset := Offset;
      rd.Destination := Disp;

      if (Lbl.isBound) then
      begin
        rd.Destination := rd.Destination + Lbl.Position;
        _emitInt32(0);
      end
      else
        _emitDisplacement(Lbl, -4 - immSize, 4).relocId := relocId;

      FRelocData.append(rd);
      FMemZone.append(rd);
    end
    else
      _emitInt32(Int32(PUInt8(PtrInt(Mem.u._mem.target) + Disp)^));

{$ELSE}

    if (Mem.hasLabel) then
    begin
      Lbl := Mem.u._mem.Lbl;

      if (Mem.hasIndex) then
      begin
        setError(ERROR_ILLEGAL_ADDRESING);
        Exit;
      end;

      _emitMod(0, opReg, 5);
      Disp := Disp - (4 + immSize);

      if (Lbl.isBound) then
      begin
        Disp := Disp + Lbl.Position - Offset;
        _emitInt32(Int32(Disp));
      end
      else
        _emitDisplacement(Lbl, Disp, 4);
    end
    else
    begin
      _emitMod(0, opReg, 4);

      if (Mem.hasIndex) then
        _emitSib(Shift, indexReg, 5)
      else
        _emitSib(0, 4, 5);

      Target := SysUInt(PUInt8(PtrInt(Mem.u._mem.target) + Disp)^);

      if (Target > SysUInt($FFFFFFFF)) and (FLogger <> nil) then
        FLogger.log('; Warning: Absolute address truncated to 32 bits' + LineEnding);

      _emitInt32( Int32(UInt32(target)) );
    end;

{$ENDIF}

  end;
end;

procedure TAssembler._emitModRM(opReg: UInt8; op: TOperand; immSize: SysInt);
begin
  Assert((op.op = OP_REG) or (op.op = OP_MEM));

  if (op.op = OP_REG) then
    _emitModR(opReg, TBaseReg(op).Code)
  else
    _emitModM(opReg, TMem(op), immSize);
end;

procedure TAssembler._emitX86Inl(opCode: UInt32; i16bit, rexw: Boolean; reg: UInt8);
begin
  if i16bit then
    _emitByte($66);

  if ((opCode and $FF000000) <> 0) then
    _emitByte(UInt8((opCode and $FF000000) shr 24));

{$IFDEF ASMJIT_X64}
  _emitRexR(rexw, 0, reg);
{$ENDIF}

  if ((opCode and $00FF0000) <> 0) then
    _emitByte(UInt8((opCode and $00FF0000) shr 16));
  if ((opCode and $0000FF00) <> 0) then
    _emitByte(UInt8((opCode and $0000FF00) shr  8));

  _emitByte(UInt8(opCode and $000000FF) + (reg and $7));
end;

procedure TAssembler._emitX86RM(opCode: UInt32; i16bit, rexw: Boolean; o: UInt8; op: TOperand; immSize: SysInt);
begin
  if i16bit then
    _emitByte($66);

  _emitSegmentPrefix(op);

  if ((opCode and $FF000000) <> 0) then
    _emitByte(UInt8((opCode and $FF000000) shr 24));

{$IFDEF ASMJIT_X64}
  _emitRexRM(rexw, o, op);
{$ENDIF}

  if ((opCode and $00FF0000) <> 0) then
    _emitByte(UInt8((opCode and $00FF0000) shr 16));
  if ((opCode and $0000FF00) <> 0) then
    _emitByte(UInt8((opCode and $0000FF00) shr  8));
  _emitByte(UInt8(opCode and $000000FF));

  _emitModRM(o, op, immSize);
end;

procedure TAssembler._emitFpu(opCode: UInt32);
begin
  _emitOpCode(opCode);
end;

procedure TAssembler._emitFpuSTI(opCode, sti: UInt32);
begin
  //Assert((0 <= sti) and (sti < 8));
  _emitOpCode(opCode + sti);
end;

procedure TAssembler._emitFpuMEM(opCode: UInt32; opReg: UInt8; Mem: TMem);
begin
  _emitSegmentPrefix(Mem);

  if ((opCode and $FF000000) <> 0) then
    _emitByte(UInt8((opCode and $FF000000) shr 24));

{$IFDEF ASMJIT_X64}
  _emitRexRM(False, opReg, Mem);
{$ENDIF}

  if ((opCode and $00FF0000) <> 0) then
    _emitByte(UInt8((opCode and $00FF0000) shr 16));
  if ((opCode and $0000FF00) <> 0) then
    _emitByte(UInt8((opCode and $0000FF00) shr  8));

  _emitByte(UInt8((opCode and $000000FF)));
  _emitModM(opReg, Mem, 0);
end;

procedure TAssembler._emitMmu(opCode: UInt32; rexw: Boolean; opReg: UInt8; src: TOperand; immSize: SysInt);
begin
  _emitSegmentPrefix(src);
  if ((opCode and $FF000000) <> 0) then
    _emitByte(UInt8((opCode and $FF000000) shr 24));

{$IFDEF ASMJIT_X64}
  _emitRexRM(rexw, opReg, src);
{$ENDIF}

  if ((opCode and $00FF0000) <> 0) then
    _emitByte(UInt8((opCode and $00FF0000) shr 16));

  _emitByte(UInt8((opCode and $0000FF00) shr 8));
  _emitByte(UInt8((opCode and $000000FF)));

  if (src.isReg) then
    _emitModR(opReg, TBaseReg(src).Code)
  else
    _emitModM(opReg, TMem(src), immSize);
end;

function TAssembler._emitDisplacement(Lbl: PLabel; inlinedDisplacement: SysInt; Size: Integer): PLinkData;
begin
  Assert(not Lbl.isBound);
  Assert((Size = 1) or (Size = 4));

  Result := _newLinkData;
  Result.Prev := PLinkData(Lbl.u._lbl.link);
  Result.Offset := Offset;
  Result.Displacement := InlinedDisplacement;

  Lbl.u._lbl.link := Result;
  Lbl.u._lbl.state := LABEL_STATE_LINKED;

  if (Size = 1) then
    _emitByte($01)
  else
    _emitDWord($04040404);
end;

procedure TAssembler._emitJmpOrCallReloc(Instruction: UInt32; Target: Pointer);
var
  rd: PRelocData;
begin
  New(rd);
  rd.Typ := ABSOLUTE_TO_RELATIVE_TRAMPOLINE;

{$IFDEF ASMJIT_X64}
  FTrampolineSize := FTrampolineSize + TRAMPOLINE_SIZE;
{$ENDIF}

  rd.Size := 4;
  rd.Offset := Offset;
  rd.Address := Target;

  FMemZone.append(rd);
  FRelocData.append(rd);

  _emitInt32(0);
end;

procedure TAssembler.relocCode(_dst: Pointer);
var
  dst: PUInt8;
  {$IFDEF ASMJIT_X64}tramp: PUInt8;{$ENDIF}
  Coff, cSize, i, Len, Val: SysInt;
  {$IFDEF ASMJIT_X64}useTrampoline: Boolean;{$ENDIF}
  r: PRelocData;
begin
  Coff := FBuffer.Offset;
  cSize := CodeSize;

  dst := FBuffer.Data;
  Move(dst^, _dst^, Coff);

  dst := PUInt8(_dst);

{$IFDEF ASMJIT_X64}
  Tramp := PUInt8(PtrInt(dst) + coff);
{$ENDIF}

  Len := FRelocData.Length;
  Val := 0;

  for i := 0 to Len - 1 do
  begin
    r := FRelocData[i];

{$IFDEF ASMJIT_X64}
    useTrampoline := False;
{$ENDIF}

    Assert(r.offset + SysInt(r.Size) <= cSize);

    case r.typ of
      ABSOLUTE_TO_ABSOLUTE: Val := SysInt(r.address);
      RELATIVE_TO_ABSOLUTE: Val := SysInt(PtrInt(dst) + r.Destination);
      ABSOLUTE_TO_RELATIVE, ABSOLUTE_TO_RELATIVE_TRAMPOLINE:
        begin
          val := SysInt( SysUInt(r.Address) - (SysUInt(dst) + SysUInt(r.Offset) + 4) );

{$IFDEF ASMJIT_X64}
          if ((r.Typ = ABSOLUTE_TO_RELATIVE_TRAMPOLINE) and (not isInt32(Val))) then
          begin
            Val := SysInt( SysUInt(Tramp) - (SysUInt(dst) + SysUInt(r.Offset) + 4) );
            useTrampoline := True;
          end;
{$ENDIF}
        end;
      else Assert(False);
    end;

    case r.Size of
      4: PInt32(PtrInt(dst) + r.Offset)^ := Int32(Val);
      8: PInt64(PtrInt(dst) + r.offset)^ := Int64(Val);
      else
        Assert(False);
    end;

{$IFDEF ASMJIT_X64}
    if useTrampoline then
    begin
      if ((FLogger <> nil) and FLogger.enabled) then
        FLogger.logFormat('; Trampoline from %p . %p' + LineEnding, [PtrInt(dst) + r.offset, PtrInt(r.address)]);

      WriteTrampoline(tramp, r.address);
      Tramp := PUint8(PtrUInt(Tramp) + TRAMPOLINE_SIZE);
    end;
{$ENDIF}
  end;
end;

procedure TAssembler._inlineComment(Text: string);
begin
  if (FLogger = nil) then
    Exit;

  FInlineCommentBuffer := FInlineCommentBuffer + Text;
end;


type
  TInstructionDescription = record
{$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP}
    Instruction: UInt32;
    Name: PChar;
{$ENDIF}

    Group: UInt8;
    o1Flags: UInt8;
    o2Flags: UInt8;
    opCodeR: UInt8;
    opCode1: UInt32;
    opCode2: UInt32;
  end;

const
  //TInstructionE = (
    I_EMIT = 0;

    I_ALU = 1;
    I_BSWAP = 2;
    I_BT = 3;
    I_CALL = 4;
    I_CRC32 = 5;
    I_ENTER = 6;
    I_IMUL = 7;
    I_INC_DEC = 8;
    I_J = 9;
    I_JMP = 10;
    I_LEA = 11;
    I_M = 12;
    I_MOV = 13;
    I_MOV_PTR = 14;
    I_MOVSX_MOVZX = 15;
    I_MOVSXD = 16;
    I_PUSH = 17;
    I_POP = 18;
    I_R_RM = 19;
    I_RM_B = 20;
    I_RM = 21;
    I_RM_R = 22;
    I_RET = 23;
    I_ROT = 24;
    I_SHLD_SHRD = 25;
    I_TEST = 26;
    I_XCHG = 27;

    I_X87_FPU = 28;
    I_X87_STI = 29;
    I_X87_MEM_STI = 30;
    I_X87_MEM = 31;
    I_X87_FSTSW = 32;

    I_MOVBE = 33;

    I_MMU_MOV = 34;

    I_MMU_MOVD = 35;
    I_MMU_MOVQ = 36;

    I_MMU_PEXTR = 37;

    I_MMU_PREFETCH = 38;

    I_MMU_RMI = 39;
    I_MMU_RM_IMM8 = 40;

    I_MMU_RM_3DNOW = 41;
  //);

//const
  O_G8          = $01;
  O_G16         = $02;
  O_G32         = $04;
  O_G64         = $08;
  O_MEM         = $40;
  O_IMM         = $80;

  O_G8_16_32_64 = O_G64 or O_G32 or O_G16  or O_G8;
  O_G16_32_64   = O_G64 or O_G32 or O_G16;
  O_G32_64      = O_G64 or O_G32;

  // x87
  O_FM_1        = $01;
  O_FM_2        = $02;
  O_FM_4        = $04;
  O_FM_8        = $08;
  O_FM_10       = $10;

  O_FM_2_4      = O_FM_2 or O_FM_4;
  O_FM_2_4_8    = O_FM_2 or O_FM_4 or O_FM_8;
  O_FM_4_8      = O_FM_4 or O_FM_8;
  O_FM_4_8_10   = O_FM_4 or O_FM_8 or O_FM_10;

  O_NOREX       = $01;
  O_MM          = $10;
  O_XMM         = $20;

  O_MM_MEM      = O_MM  or O_MEM;
  O_XMM_MEM     = O_XMM or O_MEM;
  O_MM_XMM      = O_MM  or O_XMM;
  O_MM_XMM_MEM  = O_MM  or O_XMM  or O_MEM;

  x86instructions: array[0..577] of TInstructionDescription = (

  // Instruction code (enum)      | instruction name   | group           | operator 1 flags| operator 2 flags| r| opCode1   | opcode2
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADC              ; Name: 'adc'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 2; opCode1: $00000010; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADD              ; Name: 'add'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000000; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDPD            ; Name: 'addpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F58; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDPS            ; Name: 'addps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F58; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDSD            ; Name: 'addsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F58; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDSS            ; Name: 'addss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F58; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDSUBPD         ; Name: 'addsubpd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000FD0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ADDSUBPS         ; Name: 'addsubps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000FD0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_AMD_PREFETCH     ; Name: 'amd_prefetch'; {$ENDIF}    Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F0D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_AMD_PREFETCHW    ; Name: 'amd_prefetchw'; {$ENDIF}   Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000F0D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_AND              ; Name: 'and'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 4; opCode1: $00000020; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ANDNPD           ; Name: 'andnpd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F55; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ANDNPS           ; Name: 'andnps'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F55; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ANDPD            ; Name: 'andpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F54; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ANDPS            ; Name: 'andps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F54; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BLENDPD          ; Name: 'blendpd'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A0D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BLENDPS          ; Name: 'blendps'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A0C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BLENDVPD         ; Name: 'blendvpd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3815; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BLENDVPS         ; Name: 'blendvps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3814; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BSF              ; Name: 'bsf'; {$ENDIF}             Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FBC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BSR              ; Name: 'bsr'; {$ENDIF}             Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FBD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BSWAP            ; Name: 'bswap'; {$ENDIF}           Group: I_BSWAP         ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BT               ; Name: 'bt'; {$ENDIF}              Group: I_BT            ; o1Flags:O_G16_32_64 or O_MEM; o2Flags:O_G16_32_64 or O_IMM; opCodeR: 4; opCode1: $00000FA3; opCode2: $00000FBA),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BTC              ; Name: 'btc'; {$ENDIF}             Group: I_BT            ; o1Flags:O_G16_32_64 or O_MEM; o2Flags:O_G16_32_64 or O_IMM; opCodeR: 7; opCode1: $00000FBB; opCode2: $00000FBA),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BTR              ; Name: 'btr'; {$ENDIF}             Group: I_BT            ; o1Flags:O_G16_32_64 or O_MEM; o2Flags:O_G16_32_64 or O_IMM; opCodeR: 6; opCode1: $00000FB3; opCode2: $00000FBA),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_BTS              ; Name: 'bts'; {$ENDIF}             Group: I_BT            ; o1Flags:O_G16_32_64 or O_MEM; o2Flags:O_G16_32_64 or O_IMM; opCodeR: 5; opCode1: $00000FAB; opCode2: $00000FBA),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CALL             ; Name: 'call'; {$ENDIF}            Group: I_CALL          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CBW              ; Name: 'cbw'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $66000099; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CDQE             ; Name: 'cdqe'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $48000099; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CLC              ; Name: 'clc'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000F8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CLD              ; Name: 'cld'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000FC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CLFLUSH          ; Name: 'clflush'; {$ENDIF}         Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 7; opCode1: $00000FAE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMC              ; Name: 'cmc'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000F5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVA            ; Name: 'cmova'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F47; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVAE           ; Name: 'cmovae'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F43; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVB            ; Name: 'cmovb'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F42; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVBE           ; Name: 'cmovbe'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F46; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVC            ; Name: 'cmovc'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F42; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVE            ; Name: 'cmove'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F44; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVG            ; Name: 'cmovg'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVGE           ; Name: 'cmovge'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVL            ; Name: 'cmovl'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVLE           ; Name: 'cmovle'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNA           ; Name: 'cmovna'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F46; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNAE          ; Name: 'cmovnae'; {$ENDIF}         Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F42; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNB           ; Name: 'cmovnb'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F43; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNBE          ; Name: 'cmovnbe'; {$ENDIF}         Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F47; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNC           ; Name: 'cmovnc'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F43; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNE           ; Name: 'cmovne'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F45; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNG           ; Name: 'cmovng'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNGE          ; Name: 'cmovnge'; {$ENDIF}         Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNL           ; Name: 'cmovnl'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNLE          ; Name: 'cmovnle'; {$ENDIF}         Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNO           ; Name: 'cmovno'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F41; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNP           ; Name: 'cmovnp'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNS           ; Name: 'cmovns'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F49; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVNZ           ; Name: 'cmovnz'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F45; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVO            ; Name: 'cmovo'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F40; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVP            ; Name: 'cmovp'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVPE           ; Name: 'cmovpe'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVPO           ; Name: 'cmovpo'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F4B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVS            ; Name: 'cmovs'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F48; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMOVZ            ; Name: 'cmovz'; {$ENDIF}           Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F44; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMP              ; Name: 'cmp'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 7; opCode1: $00000038; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPPD            ; Name: 'cmppd'; {$ENDIF}           Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000FC2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPPS            ; Name: 'cmpps'; {$ENDIF}           Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000FC2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPSD            ; Name: 'cmpsd'; {$ENDIF}           Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000FC2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPSS            ; Name: 'cmpss'; {$ENDIF}           Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000FC2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPXCHG          ; Name: 'cmpxchg'; {$ENDIF}         Group: I_RM_R          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FB0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPXCHG16B       ; Name: 'cmpxchg16b'; {$ENDIF}      Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000FC7; opCode2: 1),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CMPXCHG8B        ; Name: 'cmpxchg8b'; {$ENDIF}       Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000FC7; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_COMISD           ; Name: 'comisd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F2F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_COMISS           ; Name: 'comiss'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F2F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CPUID            ; Name: 'cpuid'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FA2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CRC32            ; Name: 'crc32'; {$ENDIF}           Group: I_CRC32         ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F20F38F0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTDQ2PD         ; Name: 'cvtdq2pd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000FE6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTDQ2PS         ; Name: 'cvtdq2ps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPD2DQ         ; Name: 'cvtpd2dq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000FE6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPD2PI         ; Name: 'cvtpd2pi'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM            ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F2D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPD2PS         ; Name: 'cvtpd2ps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPI2PD         ; Name: 'cvtpi2pd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $66000F2A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPI2PS         ; Name: 'cvtpi2ps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F2A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPS2DQ         ; Name: 'cvtps2dq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPS2PD         ; Name: 'cvtps2pd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTPS2PI         ; Name: 'cvtps2pi'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM            ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F2D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSD2SI         ; Name: 'cvtsd2si'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_G32_64        ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F2D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSD2SS         ; Name: 'cvtsd2ss'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F5A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSI2SD         ; Name: 'cvtsi2sd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_G32_64 or O_MEM  ; opCodeR: 0; opCode1: $F2000F2A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSI2SS         ; Name: 'cvtsi2ss'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_G32_64 or O_MEM  ; opCodeR: 0; opCode1: $F3000F2A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSS2SD         ; Name: 'cvtss2sd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTSS2SI         ; Name: 'cvtss2si'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_G32_64        ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F2D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTPD2DQ        ; Name: 'cvttpd2dq'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000FE6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTPD2PI        ; Name: 'cvttpd2pi'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM            ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F2C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTPS2DQ        ; Name: 'cvttps2dq'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTPS2PI        ; Name: 'cvttps2pi'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM            ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F2C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTSD2SI        ; Name: 'cvttsd2si'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_G32_64        ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F2C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CVTTSS2SI        ; Name: 'cvttss2si'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_G32_64        ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F2C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_CWDE             ; Name: 'cwde'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000099; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DAA              ; Name: 'daa'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000027; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DAS              ; Name: 'das'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000002F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DEC              ; Name: 'dec'; {$ENDIF}             Group: I_INC_DEC       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000048; opCode2: $000000FE),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DIV              ; Name: 'div'; {$ENDIF}             Group: I_RM            ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 6; opCode1: $000000F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DIVPD            ; Name: 'divpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DIVPS            ; Name: 'divps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DIVSD            ; Name: 'divsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F5E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DIVSS            ; Name: 'divss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DPPD             ; Name: 'dppd'; {$ENDIF}            Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A41; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_DPPS             ; Name: 'dpps'; {$ENDIF}            Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A40; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_EMMS             ; Name: 'emms'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F77; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ENTER            ; Name: 'enter'; {$ENDIF}           Group: I_ENTER         ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000C8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_EXTRACTPS        ; Name: 'extractps'; {$ENDIF}       Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A17; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_F2XM1            ; Name: 'f2xm1'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FABS             ; Name: 'fabs'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E1; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FADD             ; Name: 'fadd'; {$ENDIF}            Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $D8C0DCC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FADDP            ; Name: 'faddp'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FBLD             ; Name: 'fbld'; {$ENDIF}            Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 4; opCode1: $000000DF; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FBSTP            ; Name: 'fbstp'; {$ENDIF}           Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 6; opCode1: $000000DF; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCHS             ; Name: 'fchs'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCLEX            ; Name: 'fclex'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $9B00DBE2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVB           ; Name: 'fcmovb'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DAC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVBE          ; Name: 'fcmovbe'; {$ENDIF}         Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DAD0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVE           ; Name: 'fcmove'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DAC8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVNB          ; Name: 'fcmovnb'; {$ENDIF}         Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVNBE         ; Name: 'fcmovnbe'; {$ENDIF}        Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBD0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVNE          ; Name: 'fcmovne'; {$ENDIF}         Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBC8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVNU          ; Name: 'fcmovnu'; {$ENDIF}         Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBD8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCMOVU           ; Name: 'fcmovu'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DAD8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOM             ; Name: 'fcom'; {$ENDIF}            Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 2; opCode1: $D8DCD0D0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOMI            ; Name: 'fcomi'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBF0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOMIP           ; Name: 'fcomip'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DFF0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOMP            ; Name: 'fcomp'; {$ENDIF}           Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 3; opCode1: $D8DCD8D8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOMPP           ; Name: 'fcompp'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DED9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FCOS             ; Name: 'fcos'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FF; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FDECSTP          ; Name: 'fdecstp'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FDIV             ; Name: 'fdiv'; {$ENDIF}            Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 6; opCode1: $D8DCF0F8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FDIVP            ; Name: 'fdivp'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEF8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FDIVR            ; Name: 'fdivr'; {$ENDIF}           Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 7; opCode1: $D8DCF8F0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FDIVRP           ; Name: 'fdivrp'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEF0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FEMMS            ; Name: 'femms'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F0E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FFREE            ; Name: 'ffree'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DDC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FIADD            ; Name: 'fiadd'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 0; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FICOM            ; Name: 'ficom'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 2; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FICOMP           ; Name: 'ficomp'; {$ENDIF}          Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 3; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FIDIV            ; Name: 'fidiv'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 6; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FIDIVR           ; Name: 'fidivr'; {$ENDIF}          Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 7; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FILD             ; Name: 'fild'; {$ENDIF}            Group: I_X87_MEM       ; o1Flags: O_FM_2_4_8      ; o2Flags: 0               ; opCodeR: 0; opCode1: $DFDBDF05; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FIMUL            ; Name: 'fimul'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 1; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FINCSTP          ; Name: 'fincstp'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F7; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FINIT            ; Name: 'finit'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $9B00DBE3; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FIST             ; Name: 'fist'; {$ENDIF}            Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 2; opCode1: $DFDB0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FISTP            ; Name: 'fistp'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4_8      ; o2Flags: 0               ; opCodeR: 3; opCode1: $DFDBDF07; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FISTTP           ; Name: 'fisttp'; {$ENDIF}          Group: I_X87_MEM       ; o1Flags: O_FM_2_4_8      ; o2Flags: 0               ; opCodeR: 1; opCode1: $DFDBDD01; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FISUB            ; Name: 'fisub'; {$ENDIF}           Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 4; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FISUBR           ; Name: 'fisubr'; {$ENDIF}          Group: I_X87_MEM       ; o1Flags: O_FM_2_4        ; o2Flags: 0               ; opCodeR: 5; opCode1: $DEDA0000; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLD              ; Name: 'fld'; {$ENDIF}             Group: I_X87_MEM_STI   ; o1Flags: O_FM_4_8_10     ; o2Flags: 0               ; opCodeR: 0; opCode1: $00D9DD00; opCode2: $D9C0DB05),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLD1             ; Name: 'fld1'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDCW            ; Name: 'fldcw'; {$ENDIF}           Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 5; opCode1: $000000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDENV           ; Name: 'fldenv'; {$ENDIF}          Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 4; opCode1: $000000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDL2E           ; Name: 'fldl2e'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9EA; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDL2T           ; Name: 'fldl2t'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDLG2           ; Name: 'fldlg2'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9EC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDLN2           ; Name: 'fldln2'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9ED; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDPI            ; Name: 'fldpi'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9EB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FLDZ             ; Name: 'fldz'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9EE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FMUL             ; Name: 'fmul'; {$ENDIF}            Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 1; opCode1: $D8DCC8C8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FMULP            ; Name: 'fmulp'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEC8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNCLEX           ; Name: 'fnclex'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBE2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNINIT           ; Name: 'fninit'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBE3; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNOP             ; Name: 'fnop'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9D0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNSAVE           ; Name: 'fnsave'; {$ENDIF}          Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 6; opCode1: $000000DD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNSTCW           ; Name: 'fnstcw'; {$ENDIF}          Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 7; opCode1: $000000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNSTENV          ; Name: 'fnstenv'; {$ENDIF}         Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 6; opCode1: $000000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FNSTSW           ; Name: 'fnstsw'; {$ENDIF}          Group: I_X87_FSTSW     ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 7; opCode1: $000000DD; opCode2: $0000DFE0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FPATAN           ; Name: 'fpatan'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F3; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FPREM            ; Name: 'fprem'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FPREM1           ; Name: 'fprem1'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FPTAN            ; Name: 'fptan'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F2; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FRNDINT          ; Name: 'frndint'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FRSTOR           ; Name: 'frstor'; {$ENDIF}          Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 4; opCode1: $000000DD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSAVE            ; Name: 'fsave'; {$ENDIF}           Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 6; opCode1: $9B0000DD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSCALE           ; Name: 'fscale'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSIN             ; Name: 'fsin'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSINCOS          ; Name: 'fsincos'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSQRT            ; Name: 'fsqrt'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9FA; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FST              ; Name: 'fst'; {$ENDIF}             Group: I_X87_MEM_STI   ; o1Flags: O_FM_4_8        ; o2Flags: 0               ; opCodeR: 2; opCode1: $00D9DD02; opCode2: $DDD00000),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSTCW            ; Name: 'fstcw'; {$ENDIF}           Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 7; opCode1: $9B0000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSTENV           ; Name: 'fstenv'; {$ENDIF}          Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 6; opCode1: $9B0000D9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSTP             ; Name: 'fstp'; {$ENDIF}            Group: I_X87_MEM_STI   ; o1Flags: O_FM_4_8_10     ; o2Flags: 0               ; opCodeR: 3; opCode1: $00D9DD03; opCode2: $DDD8DB07),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSTSW            ; Name: 'fstsw'; {$ENDIF}           Group: I_X87_FSTSW     ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 7; opCode1: $9B0000DD; opCode2: $9B00DFE0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSUB             ; Name: 'fsub'; {$ENDIF}            Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 4; opCode1: $D8DCE0E8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSUBP            ; Name: 'fsubp'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSUBR            ; Name: 'fsubr'; {$ENDIF}           Group: I_X87_FPU       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 5; opCode1: $D8DCE8E0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FSUBRP           ; Name: 'fsubrp'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DEE0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FTST             ; Name: 'ftst'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FUCOM            ; Name: 'fucom'; {$ENDIF}           Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DDE0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FUCOMI           ; Name: 'fucomi'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DBE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FUCOMIP          ; Name: 'fucomip'; {$ENDIF}         Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DFE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FUCOMP           ; Name: 'fucomp'; {$ENDIF}          Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DDE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FUCOMPP          ; Name: 'fucompp'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000DAE9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FWAIT            ; Name: 'fwait'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000DB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FXAM             ; Name: 'fxam'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9E5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FXCH             ; Name: 'fxch'; {$ENDIF}            Group: I_X87_STI       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9C8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FXRSTOR          ; Name: 'fxrstor'; {$ENDIF}         Group: I_M             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000FAE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FXSAVE           ; Name: 'fxsave'; {$ENDIF}          Group: I_M             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FAE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FXTRACT          ; Name: 'fxtract'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FYL2X            ; Name: 'fyl2x'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F1; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_FYL2XP1          ; Name: 'fyl2xp1'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000D9F9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_HADDPD           ; Name: 'haddpd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F7C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_HADDPS           ; Name: 'haddps'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F7C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_HSUBPD           ; Name: 'hsubpd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F7D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_HSUBPS           ; Name: 'hsubps'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F7D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_IDIV             ; Name: 'idiv'; {$ENDIF}            Group: I_RM            ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 7; opCode1: $000000F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_IMUL             ; Name: 'imul'; {$ENDIF}            Group: I_IMUL          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_INC              ; Name: 'inc'; {$ENDIF}             Group: I_INC_DEC       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000040; opCode2: $000000FE),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_INT3             ; Name: 'int3'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000CC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JA               ; Name: 'ja'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $7       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JAE              ; Name: 'jae'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JB               ; Name: 'jb'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JBE              ; Name: 'jbe'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $6       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JC               ; Name: 'jc'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JE               ; Name: 'je'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $4       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JG               ; Name: 'jg'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JGE              ; Name: 'jge'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $D       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JL               ; Name: 'jl'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $C       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JLE              ; Name: 'jle'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $E       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNA              ; Name: 'jna'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $6       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNAE             ; Name: 'jnae'; {$ENDIF}            Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNB              ; Name: 'jnb'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNBE             ; Name: 'jnbe'; {$ENDIF}            Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $7       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNC              ; Name: 'jnc'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNE              ; Name: 'jne'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $5       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNG              ; Name: 'jng'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $E       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNGE             ; Name: 'jnge'; {$ENDIF}            Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $C       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNL              ; Name: 'jnl'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $D       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNLE             ; Name: 'jnle'; {$ENDIF}            Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNO              ; Name: 'jno'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $1       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNP              ; Name: 'jnp'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $B       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNS              ; Name: 'jns'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $9       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNZ              ; Name: 'jnz'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $5       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JO               ; Name: 'jo'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JP               ; Name: 'jp'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $A       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JPE              ; Name: 'jpe'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $A       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JPO              ; Name: 'jpo'; {$ENDIF}             Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $B       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JS               ; Name: 'js'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $8       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JZ               ; Name: 'jz'; {$ENDIF}              Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $4       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JMP              ; Name: 'jmp'; {$ENDIF}             Group: I_JMP           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JA_SHORT         ; Name: 'ja short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $7       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JAE_SHORT        ; Name: 'jae short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JB_SHORT         ; Name: 'jb short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JBE_SHORT        ; Name: 'jbe short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $6       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JC_SHORT         ; Name: 'jc short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JE_SHORT         ; Name: 'je short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $4       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JG_SHORT         ; Name: 'jg short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JGE_SHORT        ; Name: 'jge short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $D       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JL_SHORT         ; Name: 'jl short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $C       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JLE_SHORT        ; Name: 'jle short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $E       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNA_SHORT        ; Name: 'jna short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $6       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNAE_SHORT       ; Name: 'jnae short'; {$ENDIF}      Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $2       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNB_SHORT        ; Name: 'jnb short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNBE_SHORT       ; Name: 'jnbe short'; {$ENDIF}      Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $7       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNC_SHORT        ; Name: 'jnc short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $3       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNE_SHORT        ; Name: 'jne short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $5       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNG_SHORT        ; Name: 'jng short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $E       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNGE_SHORT       ; Name: 'jnge short'; {$ENDIF}      Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $C       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNL_SHORT        ; Name: 'jnl short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $D       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNLE_SHORT       ; Name: 'jnle short'; {$ENDIF}      Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNO_SHORT        ; Name: 'jno short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $1       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNP_SHORT        ; Name: 'jnp short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $B       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNS_SHORT        ; Name: 'jns short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $9       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JNZ_SHORT        ; Name: 'jnz short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $5       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JO_SHORT         ; Name: 'jo short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JP_SHORT         ; Name: 'jp short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $A       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JPE_SHORT        ; Name: 'jpe short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $A       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JPO_SHORT        ; Name: 'jpo short'; {$ENDIF}       Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $B       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JS_SHORT         ; Name: 'js short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $8       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JZ_SHORT         ; Name: 'jz short'; {$ENDIF}        Group: I_J             ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $4       ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_JMP_SHORT        ; Name: 'jmp short'; {$ENDIF}       Group: I_JMP           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LDDQU            ; Name: 'lddqu'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_MEM           ; opCodeR: 0; opCode1: $F2000FF0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LDMXCSR          ; Name: 'ldmxcsr'; {$ENDIF}         Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 2; opCode1: $00000FAE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LEA              ; Name: 'lea'; {$ENDIF}             Group: I_LEA           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LEAVE            ; Name: 'leave'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000C9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LFENCE           ; Name: 'lfence'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000FAEE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_LOCK             ; Name: 'DAsmJit_Lock'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000F0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MASKMOVDQU       ; Name: 'maskmovdqu'; {$ENDIF}      Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $66000F57; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MASKMOVQ         ; Name: 'maskmovq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM            ; o2Flags: O_MM            ; opCodeR: 0; opCode1: $00000FF7; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MAXPD            ; Name: 'maxpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MAXPS            ; Name: 'maxps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MAXSD            ; Name: 'maxsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F5F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MAXSS            ; Name: 'maxss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MFENCE           ; Name: 'mfence'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000FAEF0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MINPD            ; Name: 'minpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MINPS            ; Name: 'minps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MINSD            ; Name: 'minsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F5D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MINSS            ; Name: 'minss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MONITOR          ; Name: 'monitor'; {$ENDIF}         Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000F01C8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOV              ; Name: 'mov'; {$ENDIF}             Group: I_MOV           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVAPD           ; Name: 'movapd'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F28; opCode2: $66000F29),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVAPS           ; Name: 'movaps'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F28; opCode2: $00000F29),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVBE            ; Name: 'movbe'; {$ENDIF}           Group: I_MOVBE         ; o1Flags:O_G16_32_64 or O_MEM; o2Flags:O_G16_32_64 or O_MEM; opCodeR: 0; opCode1: $000F38F0; opCode2: $000F38F1),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVD             ; Name: 'movd'; {$ENDIF}            Group: I_MMU_MOVD      ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVDDUP          ; Name: 'movddup'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F12; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVDQ2Q          ; Name: 'movdq2q'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_MM            ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $F2000FD6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVDQA           ; Name: 'movdqa'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F6F; opCode2: $66000F7F),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVDQU           ; Name: 'movdqu'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F6F; opCode2: $F3000F7F),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVHLPS          ; Name: 'movhlps'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_XMM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $00000F12; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVHPD           ; Name: 'movhpd'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F16; opCode2: $66000F17),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVHPS           ; Name: 'movhps'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F16; opCode2: $00000F17),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVLHPS          ; Name: 'movlhps'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_XMM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $00000F16; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVLPD           ; Name: 'movlpd'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F12; opCode2: $66000F13),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVLPS           ; Name: 'movlps'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F12; opCode2: $00000F13),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVMSKPD         ; Name: 'movmskpd'; {$ENDIF}        Group: I_MMU_MOV       ; o1Flags: O_G32_64 or O_NOREX; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $66000F50; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVMSKPS         ; Name: 'movmskps'; {$ENDIF}        Group: I_MMU_MOV       ; o1Flags: O_G32_64 or O_NOREX; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $00000F50; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTDQ          ; Name: 'movntdq'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_MEM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: 0        ; opCode2: $66000FE7),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTDQA         ; Name: 'movntdqa'; {$ENDIF}        Group: I_MMU_MOV       ; o1Flags: O_XMM           ; o2Flags: O_MEM           ; opCodeR: 0; opCode1: $660F382A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTI           ; Name: 'movnti'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_MEM           ; o2Flags: O_G32_64        ; opCodeR: 0; opCode1: 0        ; opCode2: $00000FC3),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTPD          ; Name: 'movntpd'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_MEM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: 0        ; opCode2: $66000F2B),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTPS          ; Name: 'movntps'; {$ENDIF}         Group: I_MMU_MOV       ; o1Flags: O_MEM           ; o2Flags: O_XMM           ; opCodeR: 0; opCode1: 0        ; opCode2: $00000F2B),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVNTQ           ; Name: 'movntq'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_MEM           ; o2Flags: O_MM            ; opCodeR: 0; opCode1: 0        ; opCode2: $00000FE7),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVQ             ; Name: 'movq'; {$ENDIF}            Group: I_MMU_MOVQ      ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVQ2DQ          ; Name: 'movq2dq'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_MM            ; opCodeR: 0; opCode1: $F3000FD6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSD            ; Name: 'movsd'; {$ENDIF}           Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F10; opCode2: $F2000F11),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSHDUP         ; Name: 'movshdup'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F16; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSLDUP         ; Name: 'movsldup'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F12; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSS            ; Name: 'movss'; {$ENDIF}           Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F10; opCode2: $F3000F11),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSX            ; Name: 'movsx'; {$ENDIF}           Group: I_MOVSX_MOVZX   ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FBE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVSXD           ; Name: 'movsxd'; {$ENDIF}          Group: I_MOVSXD        ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVUPD           ; Name: 'movupd'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F10; opCode2: $66000F11),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVUPS           ; Name: 'movups'; {$ENDIF}          Group: I_MMU_MOV       ; o1Flags: O_XMM_MEM       ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F10; opCode2: $00000F11),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOVZX            ; Name: 'movzx'; {$ENDIF}           Group: I_MOVSX_MOVZX   ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FB6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MOV_PTR          ; Name: 'mov'; {$ENDIF}             Group: I_MOV_PTR       ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MPSADBW          ; Name: 'mpsadbw'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A42; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MUL              ; Name: 'mul'; {$ENDIF}             Group: I_RM            ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 4; opCode1: $000000F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MULPD            ; Name: 'mulpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F59; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MULPS            ; Name: 'mulps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F59; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MULSD            ; Name: 'mulsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F59; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MULSS            ; Name: 'mulss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F59; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_MWAIT            ; Name: 'mwait'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000F01C9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_NEG              ; Name: 'neg'; {$ENDIF}             Group: I_RM            ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 3; opCode1: $000000F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_NOP              ; Name: 'nop'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000090; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_NOT              ; Name: 'not'; {$ENDIF}             Group: I_RM            ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 2; opCode1: $000000F6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_OR               ; Name: 'or'; {$ENDIF}              Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 1; opCode1: $00000008; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ORPD             ; Name: 'orpd'; {$ENDIF}            Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F56; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ORPS             ; Name: 'orps'; {$ENDIF}            Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F56; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PABSB            ; Name: 'pabsb'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F381C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PABSD            ; Name: 'pabsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F381E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PABSW            ; Name: 'pabsw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F381D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PACKSSDW         ; Name: 'packssdw'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F6B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PACKSSWB         ; Name: 'packsswb'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F63; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PACKUSDW         ; Name: 'packusdw'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F382B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PACKUSWB         ; Name: 'packuswb'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F67; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDB            ; Name: 'paddb'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FFC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDD            ; Name: 'paddd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FFE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDQ            ; Name: 'paddq'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FD4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDSB           ; Name: 'paddsb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FEC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDSW           ; Name: 'paddsw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FED; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDUSB          ; Name: 'paddusb'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDUSW          ; Name: 'paddusw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PADDW            ; Name: 'paddw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FFD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PALIGNR          ; Name: 'palignr'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3A0F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PAND             ; Name: 'pand'; {$ENDIF}            Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PANDN            ; Name: 'pandn'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDF; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PAUSE            ; Name: 'pause'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F3000090; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PAVGB            ; Name: 'pavgb'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PAVGW            ; Name: 'pavgw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE3; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PBLENDVB         ; Name: 'pblendvb'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3810; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PBLENDW          ; Name: 'pblendw'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A0E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPEQB          ; Name: 'pcmpeqb'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F74; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPEQD          ; Name: 'pcmpeqd'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F76; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPEQQ          ; Name: 'pcmpeqq'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3829; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPEQW          ; Name: 'pcmpeqw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F75; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPESTRI        ; Name: 'pcmpestri'; {$ENDIF}       Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A61; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPESTRM        ; Name: 'pcmpestrm'; {$ENDIF}       Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A60; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPGTB          ; Name: 'pcmpgtb'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F64; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPGTD          ; Name: 'pcmpgtd'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F66; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPGTQ          ; Name: 'pcmpgtq'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3837; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPGTW          ; Name: 'pcmpgtw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F65; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPISTRI        ; Name: 'pcmpistri'; {$ENDIF}       Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A63; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PCMPISTRM        ; Name: 'pcmpistrm'; {$ENDIF}       Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A62; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PEXTRB           ; Name: 'pextrb'; {$ENDIF}          Group: I_MMU_PEXTR     ; o1Flags: O_G8 or O_G32 or O_MEM; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $000F3A14; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PEXTRD           ; Name: 'pextrd'; {$ENDIF}          Group: I_MMU_PEXTR     ; o1Flags: O_G32      or O_MEM; o2Flags: O_XMM           ; opCodeR: 0; opCode1: $000F3A16; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PEXTRQ           ; Name: 'pextrq'; {$ENDIF}          Group: I_MMU_PEXTR     ; o1Flags: O_G32_64   or O_MEM; o2Flags: O_XMM           ; opCodeR: 1; opCode1: $000F3A16; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PEXTRW           ; Name: 'pextrw'; {$ENDIF}          Group: I_MMU_PEXTR     ; o1Flags: O_G32      or O_MEM; o2Flags: O_XMM  or  O_MM    ; opCodeR: 0; opCode1: $000F3A16; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PF2ID            ; Name: 'pf2id'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $1D),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PF2IW            ; Name: 'pf2iw'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $1C),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFACC            ; Name: 'pfacc'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $AE),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFADD            ; Name: 'pfadd'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $9E),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFCMPEQ          ; Name: 'pfcmpeq'; {$ENDIF}         Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $B0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFCMPGE          ; Name: 'pfcmpge'; {$ENDIF}         Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $90),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFCMPGT          ; Name: 'pfcmpgt'; {$ENDIF}         Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $A0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFMAX            ; Name: 'pfmax'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $A4),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFMIN            ; Name: 'pfmin'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $94),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFMUL            ; Name: 'pfmul'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $B4),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFNACC           ; Name: 'pfnacc'; {$ENDIF}          Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $8A),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFPNACC          ; Name: 'pfpnacc'; {$ENDIF}         Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $8E),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFRCP            ; Name: 'pfrcp'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $96),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFRCPIT1         ; Name: 'pfrcpit1'; {$ENDIF}        Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $A6),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFRCPIT2         ; Name: 'pfrcpit2'; {$ENDIF}        Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $B6),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFRSQIT1         ; Name: 'pfrsqit1'; {$ENDIF}        Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $A7),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFRSQRT          ; Name: 'pfrsqrt'; {$ENDIF}         Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $97),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFSUB            ; Name: 'pfsub'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $9A),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PFSUBR           ; Name: 'pfsubr'; {$ENDIF}          Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $AA),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHADDD           ; Name: 'phaddd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3802; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHADDSW          ; Name: 'phaddsw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3803; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHADDW           ; Name: 'phaddw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3801; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHMINPosUW       ; Name: 'phminPosuw'; {$ENDIF}      Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3841; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHSUBD           ; Name: 'phsubd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3806; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHSUBSW          ; Name: 'phsubsw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3807; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PHSUBW           ; Name: 'phsubw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3805; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PI2FD            ; Name: 'pi2fd'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $0D),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PI2FW            ; Name: 'pi2fw'; {$ENDIF}           Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $0C),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PINSRB           ; Name: 'pinsrb'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_G32 or O_MEM   ; opCodeR: 0; opCode1: $660F3A20; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PINSRD           ; Name: 'pinsrd'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_G32 or O_MEM   ; opCodeR: 0; opCode1: $660F3A22; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PINSRQ           ; Name: 'pinsrq'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_G64 or O_MEM   ; opCodeR: 0; opCode1: $660F3A22; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PINSRW           ; Name: 'pinsrw'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_MM_XMM        ; o2Flags: O_G32 or O_MEM   ; opCodeR: 0; opCode1: $00000FC4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMADDUBSW        ; Name: 'pmaddubsw'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3804; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMADDWD          ; Name: 'pmaddwd'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FF5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXSB           ; Name: 'pmaxsb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXSD           ; Name: 'pmaxsd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXSW           ; Name: 'pmaxsw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FEE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXUB           ; Name: 'pmaxub'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXUD           ; Name: 'pmaxud'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMAXUW           ; Name: 'pmaxuw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINSB           ; Name: 'pminsb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3838; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINSD           ; Name: 'pminsd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3839; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINSW           ; Name: 'pminsw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FEA; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINUB           ; Name: 'pminub'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FDA; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINUD           ; Name: 'pminud'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMINUW           ; Name: 'pminuw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F383A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVMSKB         ; Name: 'pmovmskb'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_G32_64        ; o2Flags: O_MM_XMM        ; opCodeR: 0; opCode1: $00000FD7; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXBD         ; Name: 'pmovsxbd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3821; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXBQ         ; Name: 'pmovsxbq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3822; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXBW         ; Name: 'pmovsxbw'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3820; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXDQ         ; Name: 'pmovsxdq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3825; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXWD         ; Name: 'pmovsxwd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3823; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVSXWQ         ; Name: 'pmovsxwq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3824; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXBD         ; Name: 'pmovzxbd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3831; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXBQ         ; Name: 'pmovzxbq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3832; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXBW         ; Name: 'pmovzxbw'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3830; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXDQ         ; Name: 'pmovzxdq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3835; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXWD         ; Name: 'pmovzxwd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3833; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMOVZXWQ         ; Name: 'pmovzxwq'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3834; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULDQ           ; Name: 'pmuldq'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3828; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULHRSW         ; Name: 'pmulhrsw'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F380B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULHUW          ; Name: 'pmulhuw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULHW           ; Name: 'pmulhw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULLD           ; Name: 'pmulld'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3840; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULLW           ; Name: 'pmullw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FD5; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PMULUDQ          ; Name: 'pmuludq'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FF4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POP              ; Name: 'pop'; {$ENDIF}             Group: I_POP           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000058; opCode2: $0000008F),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POPAD            ; Name: 'popad'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000061; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POPCNT           ; Name: 'popcnt'; {$ENDIF}          Group: I_R_RM          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $F3000FB8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POPFD            ; Name: 'popfd'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000009D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POPFQ            ; Name: 'popfq'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000009D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_POR              ; Name: 'por'; {$ENDIF}             Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FEB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PREFETCH         ; Name: 'prefetch'; {$ENDIF}        Group: I_MMU_PREFETCH  ; o1Flags: O_MEM           ; o2Flags: O_IMM           ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSADBW           ; Name: 'psadbw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FF6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSHUFB           ; Name: 'pshufb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3800; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSHUFD           ; Name: 'pshufd'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F70; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSHUFW           ; Name: 'pshufw'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F70; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSHUFHW          ; Name: 'pshufhw'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F70; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSHUFLW          ; Name: 'pshuflw'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F70; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSIGNB           ; Name: 'psignb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3808; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSIGND           ; Name: 'psignd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F380A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSIGNW           ; Name: 'psignw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $000F3809; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSLLD            ; Name: 'pslld'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 6; opCode1: $00000FF2; opCode2: $00000F72),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSLLDQ           ; Name: 'pslldq'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM    ; o2Flags: O_IMM           ; opCodeR: 7; opCode1: 0        ; opCode2: $66000F73),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSLLQ            ; Name: 'psllq'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 6; opCode1: $00000FF3; opCode2: $00000F73),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSLLW            ; Name: 'psllw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 6; opCode1: $00000FF1; opCode2: $00000F71),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRAD            ; Name: 'psrad'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 4; opCode1: $00000FE2; opCode2: $00000F72),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRAW            ; Name: 'psraw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 4; opCode1: $00000FE1; opCode2: $00000F71),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRLD            ; Name: 'psrld'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 2; opCode1: $00000FD2; opCode2: $00000F72),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRLDQ           ; Name: 'psrldq'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM    ; o2Flags: O_IMM           ; opCodeR: 3; opCode1: 0        ; opCode2: $66000F73),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRLQ            ; Name: 'psrlq'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 2; opCode1: $00000FD3; opCode2: $00000F73),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSRLW            ; Name: 'psrlw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM ; o2Flags: O_IMM or O_MM_XMM_MEM    ; opCodeR: 2; opCode1: $00000FD1; opCode2: $00000F71),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBB            ; Name: 'psubb'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FF8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBD            ; Name: 'psubd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FFA; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBQ            ; Name: 'psubq'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FFB; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBSB           ; Name: 'psubsb'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBSW           ; Name: 'psubsw'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FE9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBUSB          ; Name: 'psubusb'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FD8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBUSW          ; Name: 'psubusw'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FD9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSUBW            ; Name: 'psubw'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FF9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PSWAPD           ; Name: 'pswapd'; {$ENDIF}          Group: I_MMU_RM_3DNOW  ; o1Flags: O_MM            ; o2Flags: O_MM_MEM        ; opCodeR: 0; opCode1: $00000F0F; opCode2: $BB),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PTEST            ; Name: 'ptest'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3817; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKHBW        ; Name: 'punpckhbw'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F68; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKHDQ        ; Name: 'punpckhdq'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F6A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKHQDQ       ; Name: 'punpckhqdq'; {$ENDIF}      Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F6D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKHWD        ; Name: 'punpckhwd'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F69; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKLBW        ; Name: 'punpcklbw'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F60; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKLDQ        ; Name: 'punpckldq'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F62; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKLQDQ       ; Name: 'punpcklqdq'; {$ENDIF}      Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F6C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUNPCKLWD        ; Name: 'punpcklwd'; {$ENDIF}       Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000F61; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUSH             ; Name: 'push'; {$ENDIF}            Group: I_PUSH          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 6; opCode1: $00000050; opCode2: $000000FF),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUSHAD           ; Name: 'pushad'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000060; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUSHFD           ; Name: 'pushfd'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000009C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PUSHFQ           ; Name: 'pushfq'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000009C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_PXOR             ; Name: 'pxor'; {$ENDIF}            Group: I_MMU_RMI       ; o1Flags: O_MM_XMM        ; o2Flags: O_MM_XMM_MEM    ; opCodeR: 0; opCode1: $00000FEF; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RCL              ; Name: 'rcl'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 2; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RCPPS            ; Name: 'rcpps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F53; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RCPSS            ; Name: 'rcpss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F53; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RCR              ; Name: 'rcr'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 3; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RDTSC            ; Name: 'rdtsc'; {$ENDIF}           Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F31; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RDTSCP           ; Name: 'rdtscp'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000F01F9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RET              ; Name: 'ret'; {$ENDIF}             Group: I_RET           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROL              ; Name: 'rol'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROR              ; Name: 'ror'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 1; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROUNDPD          ; Name: 'roundpd'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A09; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROUNDPS          ; Name: 'roundps'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A08; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROUNDSD          ; Name: 'roundsd'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A0B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_ROUNDSS          ; Name: 'roundss'; {$ENDIF}         Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $660F3A0A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RSQRTPS          ; Name: 'rsqrtps'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F52; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_RSQRTSS          ; Name: 'rsqrtss'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F52; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SAHF             ; Name: 'sahf'; {$ENDIF}            Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $0000009E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SAL              ; Name: 'sal'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 4; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SAR              ; Name: 'sar'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 7; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SBB              ; Name: 'sbb'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 3; opCode1: $00000018; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETA             ; Name: 'seta'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F97; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETAE            ; Name: 'setae'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F93; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETB             ; Name: 'setb'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F92; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETBE            ; Name: 'setbe'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F96; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETC             ; Name: 'setc'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F92; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETE             ; Name: 'sete'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F94; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETG             ; Name: 'setg'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETGE            ; Name: 'setge'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETL             ; Name: 'setl'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETLE            ; Name: 'setle'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNA            ; Name: 'setna'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F96; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNAE           ; Name: 'setnae'; {$ENDIF}          Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F92; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNB            ; Name: 'setnb'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F93; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNBE           ; Name: 'setnbe'; {$ENDIF}          Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F97; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNC            ; Name: 'setnc'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F93; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNE            ; Name: 'setne'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F95; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNG            ; Name: 'setng'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNGE           ; Name: 'setnge'; {$ENDIF}          Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNL            ; Name: 'setnl'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9D; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNLE           ; Name: 'setnle'; {$ENDIF}          Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9F; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNO            ; Name: 'setno'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F91; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNP            ; Name: 'setnp'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNS            ; Name: 'setns'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F99; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETNZ            ; Name: 'setnz'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F95; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETO             ; Name: 'seto'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F90; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETP             ; Name: 'setp'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETPE            ; Name: 'setpe'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9A; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETPO            ; Name: 'setpo'; {$ENDIF}           Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F9B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETS             ; Name: 'sets'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F98; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SETZ             ; Name: 'setz'; {$ENDIF}            Group: I_RM_B          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F94; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SFENCE           ; Name: 'sfence'; {$ENDIF}          Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000FAEF8; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHL              ; Name: 'shl'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 4; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHLD             ; Name: 'shld'; {$ENDIF}            Group: I_SHLD_SHRD     ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FA4; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHR              ; Name: 'shr'; {$ENDIF}             Group: I_ROT           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 5; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHRD             ; Name: 'shrd'; {$ENDIF}            Group: I_SHLD_SHRD     ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FAC; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHUFPD           ; Name: 'shufpd'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000FC6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SHUFPS           ; Name: 'shufps'; {$ENDIF}          Group: I_MMU_RM_IMM8   ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000FC6; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SQRTPD           ; Name: 'sqrtpd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F51; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SQRTPS           ; Name: 'sqrtps'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F51; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SQRTSD           ; Name: 'sqrtsd'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F51; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SQRTSS           ; Name: 'sqrtss'; {$ENDIF}          Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F51; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_STC              ; Name: 'stc'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000F9; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_STD              ; Name: 'std'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $000000FD; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_STMXCSR          ; Name: 'stmxcsr'; {$ENDIF}         Group: I_M             ; o1Flags: O_MEM           ; o2Flags: 0               ; opCodeR: 3; opCode1: $00000FAE; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SUB              ; Name: 'sub'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 5; opCode1: $00000028; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SUBPD            ; Name: 'subpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F5C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SUBPS            ; Name: 'subps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F5C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SUBSD            ; Name: 'subsd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F2000F5C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_SUBSS            ; Name: 'subss'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $F3000F5C; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_TEST             ; Name: 'test'; {$ENDIF}            Group: I_TEST          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UCOMISD          ; Name: 'ucomisd'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F2E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UCOMISS          ; Name: 'ucomiss'; {$ENDIF}         Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F2E; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UD2              ; Name: 'ud2'; {$ENDIF}             Group: I_EMIT          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000F0B; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UNPCKHPD         ; Name: 'unpckhpd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F15; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UNPCKHPS         ; Name: 'unpckhps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F15; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UNPCKLPD         ; Name: 'unpcklpd'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F14; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_UNPCKLPS         ; Name: 'unpcklps'; {$ENDIF}        Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F14; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_XADD             ; Name: 'xadd'; {$ENDIF}            Group: I_RM_R          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: $00000FC0; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_XCHG             ; Name: 'xchg'; {$ENDIF}            Group: I_XCHG          ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 0; opCode1: 0        ; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_XOR              ; Name: 'xor'; {$ENDIF}             Group: I_ALU           ; o1Flags: 0               ; o2Flags: 0               ; opCodeR: 6; opCode1: $00000030; opCode2: $00000080),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_XORPD            ; Name: 'xorpd'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $66000F57; opCode2: 0),
    ({$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP} Instruction: INST_XORPS            ; Name: 'xorps'; {$ENDIF}           Group: I_MMU_RMI       ; o1Flags: O_XMM           ; o2Flags: O_XMM_MEM       ; opCodeR: 0; opCode1: $00000F57; opCode2: 0)
  );
var
  _none: TOperand;

procedure TAssembler._emitX86(Code: UInt32; o1, o2, o3: POperand);
var
  id: TInstructionDescription;
  opCode, hint, prefix: UInt32;
  opReg, index, i1, i2, md: UInt8;
  immSize: Int32;
  rel8_Size, rel32_Size, offs: SysInt;
  imm: TImmediate;
  reg: TRegister;
  op: TOperand;
  mem: TMem;
  Lbl: PLabel;
  rexw, isGpdGpq, isShortJump, Reverse, useImm8: Boolean;
label
  illegalInstruction;
begin
  if (not canEmit) then Exit;
  if (o1 = nil) then o1 := @_none;
  if (o2 = nil) then o2 := @_none;
  if (o3 = nil) then o3 := @_none;

  if (Code >= _INST_COUNT) then
  begin
    setError(ERROR_UNKNOWN_INSTRUCTION);
    Exit;
  end;

  id := x86instructions[Code - 1];

{$IFDEF ASMJIT_DEBUG_INSTRUCTION_MAP}
  Assert(id.Instruction = Code);
{$ENDIF}

  if ((FLogger <> nil) and (FLogger is TLogger)) then
  begin
    TLogger(FLogger).logInstruction(code, o1, o2, o3, FInlineCommentBuffer);
    FInlineCommentBuffer := '';
  end;

  case id.Group of
    I_EMIT:
    begin
      _emitOpCode(id.opCode1);
      Exit;
    end;

    I_ALU:
    begin
      opCode := id.opCode1;
      opReg := id.opCodeR;

      if ((o1.isMem) and (o2.isReg)) then
      begin
        _emitX86RM(opCode + UInt8(not o2.isRegType(REG_GPB)),
          o2.isRegType(REG_GPW),
          o2.isRegType(REG_GPQ),
          TRegister(o2^).code,
          TOperand(o1^),
          0);
        Exit;
      end;

      if ((o1.isReg) and (o2.isRegMem)) then
      begin
        _emitX86RM(opCode + 2 + UInt8(not o1.isRegType(REG_GPB)),
          o1.isRegType(REG_GPW),
          o1.isRegType(REG_GPQ),
          TRegister(o1^).code,
          TOperand(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegIndex(0)) and (o2.isImm)) then
      begin
        if (o1.isRegType(REG_GPW)) then
          _emitByte($66)
        else if (o1.isRegType(REG_GPQ)) then
          _emitByte($48);

        _emitByte((opReg shl 3) or ($04 + UInt8(not o1.isRegType(REG_GPB))));
        if (o1.Size <= 4) then
          _emitImmediate(TImmediate(o2^), o1.Size)
        else
          _emitImmediate(TImmediate(o2^), 4);
        Exit;
      end;

      if ((o1.isRegMem) and (o2.isImm)) then
      begin
        imm := TImmediate(o2^);
        if isInt8(imm.value) then
          immSize := 1
        else if (o1.Size <= 4) then
          immSize := o1.Size
        else immSize := 4;


        if (o1.Size <> 1) then
          if (immSize <> 1) then
            _emitX86RM(id.opCode2 + 1,
              o1.Size = 2,
              o1.Size = 8,
              opReg, TOperand(o1^),
              immSize)
          else
            _emitX86RM(id.opCode2 + 3,
              o1.Size = 2,
              o1.Size = 8,
              opReg, TOperand(o1^),
              immSize)
        else
          _emitX86RM(id.opCode2,
            o1.Size = 2,
            o1.Size = 8,
            opReg, TOperand(o1^),
            immSize);

        _emitImmediate(
          TImmediate(o2^),
          immSize);
        Exit;
      end;
    end;

    I_BSWAP:
      if (o1.isReg) then
      begin
        reg := TRegister(o1^);

{$IFDEF ASMJIT_X64}
        _emitRexR(reg.Typ = REG_GPQ, 1, reg.code);
{$ENDIF}
        _emitByte($0F);
        _emitModR(1, reg.code);
        Exit;
      end;

    I_BT:
    begin
      if ((o1.isRegMem) and (o2.isReg)) then
      begin
        op := TOperand(o1^);
        reg := TRegister(o2^);

        _emitX86RM(id.opCode1,
          reg.isRegType(REG_GPW),
          reg.isRegType(REG_GPQ),
          reg.code,
          op,
          0);
        Exit;
      end;

      if ((o1.isRegMem) and (o2.isImm)) then
      begin
        op := TOperand(o1^);
        imm := TImmediate(o2^);

        _emitX86RM(id.opCode2,
          imm.Size = 2,
          imm.Size = 8,
          id.opCodeR,
          op,
          1);
        _emitImmediate(imm, 1);
        Exit;
      end;
    end;

    I_CALL:
    begin
      if (o1.isRegMem(REG_GPN)) then
      begin
        op := TOperand(o1^);
        _emitX86RM($FF,
          False,
          False, 2, op,
          0);
        Exit;
      end;

      if (o1.isImm) then
      begin
        imm := TImmediate(o1^);
        _emitByte($E8);
        _emitJmpOrCallReloc(I_CALL, @imm.u._imm.value);
        Exit;
      end;

      if (o1.isLabel) then
      begin
        Lbl := PLabel(o1);

        if (Lbl.isBound) then
        begin
          rel32_Size := 5;
          offs := Lbl.Position - offset;
          Assert(offs <= 0);

          _emitByte($E8);
          _emitInt32(Int32(offs - rel32_Size));
        end
        else
        begin
          _emitByte($E8);
          _emitDisplacement(Lbl, -4, 4);
        end;
        Exit;
      end;
    end;

    I_CRC32:
    begin
      if ((o1.isReg) and (o2.isRegMem)) then
      begin
        reg := TRegister(o1^);
        op := TOperand(o2^);
        Assert((reg.Typ = REG_GPD) or (reg.Typ = REG_GPQ));

        _emitX86RM(id.opCode1 + UInt8(reg.Size <> 1),
          op.Size = 2,
          reg.Typ = 8, reg.code, op,
          0);
        Exit;
      end;
    end;

    I_ENTER:
      if ((o1.isImm) and (o2.isImm)) then
      begin
        _emitByte($C8);
        _emitImmediate(TImmediate(o1^), 2);
        _emitImmediate(TImmediate(o2^), 1);
      end;

    I_IMUL:
    begin
      if ((o1.isRegMem) and (o2.isNone) and (o3.isNone)) then
      begin
        op := TOperand(o1^);
        _emitX86RM($F6 + UInt8(op.Size <> 1),
          op.Size = 2,
          op.Size = 8, 5, op,
          0);
        Exit;
      end
      else if ((o1.isReg) and (not o2.isNone) and (o3.isNone)) then
      begin
        Reg := TRegister(o1^);
        Assert(not reg.isRegType(REG_GPW));

        if (o2.isRegMem) then
        begin
          op := TOperand(o2^);

          _emitX86RM($0FAF,
            reg.isRegType(REG_GPW),
            reg.isRegType(REG_GPQ), reg.code, op,
            0);
          Exit;
        end
        else if (o2.isImm) then
        begin
          imm := TImmediate(o2^);

          if (isInt8(imm.value) and (imm.relocMode = RELOC_NONE)) then
          begin
            _emitX86RM($6B,
              reg.isRegType(REG_GPW),
              reg.isRegType(REG_GPQ), reg.code, reg,
              1);
            _emitImmediate(imm, 1);
          end
          else
          begin
            if (reg.isRegType(REG_GPW)) then
              immSize := 2
            else
              immSize := 4;

            _emitX86RM($69,
              reg.isRegType(REG_GPW),
              reg.isRegType(REG_GPQ), reg.code, reg,
              immSize);
            _emitImmediate(imm, UInt32(immSize));
          end;
          Exit;
        end;
      end
      else if ((o1.isReg) and (o2.isRegMem) and (o3.isImm)) then
      begin
        reg := TRegister(o1^);    //dst
        op := TOperand(o2^);       //src
        imm := TImmediate(o3^);

        if (isInt8(imm.value) and (imm.relocMode = RELOC_NONE)) then
        begin
          _emitX86RM($6B,
            reg.isRegType(REG_GPW),
            reg.isRegType(REG_GPQ), reg.code, op,
            1);
          _emitImmediate(imm, 1);
        end
        else
        begin
          if (reg.isRegType(REG_GPW)) then
            immSize := 2
          else
            immSize := 4;
          _emitX86RM($69,
            reg.isRegType(REG_GPW),
            reg.isRegType(REG_GPQ), reg.code, op,
            immSize);
          _emitImmediate(imm, Int32(immSize));
        end;
        Exit;
      end;
    end;

    I_INC_DEC:
    begin
      if (o1.isRegMem) then
      begin
        op := TOperand(o1^);

{$IFDEF ASMJIT_X86}
        if ((op.isReg) and ((op.isRegType(REG_GPW)) or (op.isRegType(REG_GPD)))) then
        begin
          _emitX86Inl(id.opCode1,
            op.isRegType(REG_GPW),
            False, TBaseReg(op).code);
          Exit;
        end;
{$ENDIF}

        _emitX86RM(id.opCode2 + UInt8(op.Size <> 1),
          op.Size = 2,
          op.Size = 8, UInt8(id.opCodeR), op,
          0);
        Exit;
      end;
    end;

    I_J:
    begin
      if (o1.isLabel) then
      begin
        Lbl := PLabel(o1);
        Hint := 0;
        isShortJump := ((Code >= INST_J_SHORT) and (Code <= INST_JMP_SHORT));

        if (o2.isImm) then
          hint := TImmediate(o2^).value;

        if ((hint and (HINT_TAKEN or HINT_NOT_TAKEN) <> 0) and
           ((FProperties and (1 shl PROPERTY_X86_JCC_HINTS)) <> 0)) then
          if ((hint and HINT_TAKEN) <> 0) then
            _emitByte(HINT_BYTE_VALUE_TAKEN)
          else if ((hint and HINT_NOT_TAKEN) <> 0) then
            _emitByte(HINT_BYTE_VALUE_NOT_TAKEN);

        if (Lbl.isBound) then
        begin
          rel8_Size := 2;
          rel32_Size := 6;
          offs := Lbl.Position - offset;

          Assert(offs <= 0);

          if (isInt8(offs - rel8_Size)) then
          begin
            _emitByte($70 or UInt8(id.opCode1));
            _emitByte(UInt8(Int8(offs - rel8_Size)));
          end
          else
          begin
            if (isShortJump and (FLogger <> nil)) then
              FLogger.log('; WARNING: Emitting long conditional jump, but short jump instruction forced!');

            _emitByte($0F);
            _emitByte($80 or UInt8(id.opCode1));
            _emitInt32(Int32(offs - rel32_Size));
          end;
        end
        else if (isShortJump) then
        begin
          _emitByte($70 or UInt8(id.opCode1));
          _emitDisplacement(Lbl, -1, 1);
        end
        else
        begin
          _emitByte($0F);
          _emitByte($80 or UInt8(id.opCode1));
          _emitDisplacement(Lbl, -4, 4);
        end;
        Exit;
      end;

    end;

    I_JMP:
    begin
      if (o1.isRegMem) then
      begin
        op := TOperand(o1^);

        _emitX86RM($FF,
          False,
          False, 4, op,
          0);
        Exit;
      end;

      if (o1.isImm) then
      begin
        imm := TImmediate(o1^);
        _emitByte($E9);
        _emitJmpOrCallReloc(I_JMP, @imm.u._imm.value);
        Exit;
      end;

      if (o1.isLabel) then
      begin
        Lbl := PLabel(o1);
        isShortJump := (code = INST_JMP_SHORT);

        if (Lbl.isBound) then
        begin
          rel8_Size := 2;
          rel32_Size := 5;
          offs := Lbl.Position - offset;

          if (isInt8(offs - rel8_Size)) then
          begin
            _emitByte($EB);
            _emitByte(UInt8(Int8(offs - rel8_Size)));
          end
          else
          begin
            if (isShortJump and (FLogger <> nil)) then
              FLogger.log('; WARNING: Emitting long jump, but short jump instruction forced!');

            _emitByte($E9);
            _emitInt32(Int32(offs - rel32_Size));
          end;
        end
        else
        begin
          if (isShortJump) then
          begin
            _emitByte($EB);
            _emitDisplacement(Lbl, -1, 1);
          end
          else
          begin
            _emitByte($E9);
            _emitDisplacement(Lbl, -4, 4);
          end;
        end;
        Exit;
      end;
    end;

    I_LEA:
    begin
      if ((o1.isReg) and (o2.isMem)) then
      begin
        reg := TRegister(o1^); //dst
        mem := TMem(o2^); //src
        _emitX86RM($8D,
          reg.isRegType(REG_GPW),
          reg.isRegType(REG_GPQ), reg.code, mem,
          0);
        Exit;
      end;
    end;

    I_M:
    if (o1.isMem) then
    begin
      _emitX86RM(id.opCode1, False, Boolean(id.opCode2), id.opCodeR, TMem(o1^), 0);
      Exit;
    end;

    I_MOV:
    begin
      //dst := o1;
      //src := o2;

      case ((o1.op shl 4) or o2.op) of
        (OP_REG shl 4) or OP_REG,
        (OP_REG shl 4) or OP_MEM:
        begin
          if ((((o1.op shl 4) or o2.op)) = ((OP_REG shl 4) or OP_REG)) then
            Assert((o2.isRegType(REG_GPB)) or (o2.isRegType(REG_GPW)) or
                   (o2.isRegType(REG_GPD)) or (o2.isRegType(REG_GPQ)));

          Assert((o1.isRegType(REG_GPB)) or (o1.isRegType(REG_GPW)) or
                 (o1.isRegType(REG_GPD)) or (o1.isRegType(REG_GPQ)));

          _emitX86RM($0000008A + UInt8(not o1.isRegType(REG_GPB)),
            o1.isRegType(REG_GPW),
            o1.isRegType(REG_GPQ),
            TRegister(o1^).code,
            TOperand(o2^),
            0);
          Exit;
        end;

        (OP_REG shl 4) or OP_IMM:
        begin
          reg := TRegister(o1^); //dst
          imm := TImmediate(o2^); //src

          immSize := reg.Size;

{$IFDEF ASMJIT_X64}
          // Optimize instruction size by using 32-bit immediate if value can fit into it.
          if ((immSize = 8) and isInt32(imm.value) and (imm.relocMode = RELOC_NONE)) then
          begin
            _emitX86RM($C7,
              reg.isRegType(REG_GPW),
              reg.isRegType(REG_GPQ),
              0,
              reg,
              0);
            immSize := 4;
          end
          else
          begin
{$ENDIF}
            if (reg.Size = 1) then
              opCode := $B0
            else
              opCode := $B8;
            _emitX86Inl(opCode,
              reg.isRegType(REG_GPW),
              reg.isRegType(REG_GPQ),
              reg.code);
{$IFDEF ASMJIT_X64}
          end;
{$ENDIF}

          _emitImmediate(imm, UInt32(immSize));
          Exit;
        end;

        (OP_MEM shl 4) or OP_REG:
        begin
          Assert((o2.isRegType(REG_GPB)) or (o2.isRegType(REG_GPW)) or
                 (o2.isRegType(REG_GPD)) or (o2.isRegType(REG_GPQ)));

          _emitX86RM($88 + UInt8(not o2.isRegType(REG_GPB)),
            o2.isRegType(REG_GPW),
            o2.isRegType(REG_GPQ),
            TRegister(o2^).code,
            TOperand(o1^),
            0);
          Exit;
        end;

        (OP_MEM shl 4) or OP_IMM:
        begin
          if (o1.Size <= 4) then
            immSize := o1.Size
          else
            immSize := 4;

          _emitX86RM($C6 + UInt8(o1.Size <> 1),
            o1.Size = 2,
            o1.Size = 8,
            0,
            TOperand(o1^),
            immSize);
          _emitImmediate(TImmediate(o2^),
            UInt32(immSize));
          Exit;
        end;
      end;
    end;

    I_MOV_PTR:
    begin
      if (((o1.isReg) and (o2.isImm)) or ((o1.isImm) and (o2.isReg))) then
      begin
        reverse := o1.op = OP_REG;
        if (not reverse) then
          opCode := $A0
        else
          opCode := $A2;
        if Reverse then
        begin
          reg := TRegister(o2^);
          imm := TImmediate(o1^);
        end
        else
        begin
          reg := TRegister(o1^);
          imm := TImmediate(o2^);
        end;

        if (reg.index <> 0) then
          goto illegalInstruction;

        if (reg.isRegType(REG_GPW)) then
          _emitByte($66);
{$IFDEF ASMJIT_X64}
        _emitRexR(reg.Size = 8, 0, 0);
{$ENDIF} // ASMJIT_X64
        _emitByte(opCode + UInt8(reg.Size <> 1));
        _emitImmediate(imm, SizeOf(SysInt));
        Exit;
      end;
    end;

    I_MOVSX_MOVZX:
    begin
      if ((o1.isReg) and (o2.isRegMem)) then
      begin
        reg := TRegister(o1^); //dst
        op := TOperand(o2^);  //src

        if (reg.isRegType(REG_GPB)) then
          goto illegalInstruction;
        if ((op.Size <> 1) and (op.Size <> 2)) then
          goto illegalInstruction;
        if ((op.Size = 2) and (reg.isRegType(REG_GPW))) then
          goto illegalInstruction;

        _emitX86RM(id.opCode1 + UInt8(op.Size <> 1),
          reg.isRegType(REG_GPW),
          reg.isRegType(REG_GPQ),
          reg.code,
          op,
          0);
        Exit;
      end;
    end;

{$IFDEF ASMJIT_X64}
    I_MOVSXD:
      if ((o1.isReg) and (o2.isRegMem)) then
      begin
        reg := TRegister(o1^); //dst
        op := TOperand(o2^);   //src
        _emitX86RM($00000063,
          False,
          True, reg.code, op,
          0);
        Exit;
      end;
{$ENDIF}

    I_PUSH, I_POP:
    begin
      if (id.Group = I_PUSH) and (o1.isImm) then
      begin
        imm := TImmediate(o1^);

        if (isInt8(imm.value) and (imm.relocMode = RELOC_NONE)) then
        begin
          _emitByte($6A);
          _emitImmediate(imm, 1);
        end
        else
        begin
          _emitByte($68);
          _emitImmediate(imm, 4);
        end;
        Exit;
      end;

      if (o1.isReg) then
      begin
        Assert((o1.isRegType(REG_GPW)) or (o1.isRegType(REG_GPN)));
        _emitX86Inl(id.opCode1, o1.isRegType(REG_GPW), False, TRegister(o1^).code);
        Exit;
      end;

      if (o1.isMem) then
      begin
        _emitX86RM(id.opCode2, o1.Size = 2, False, id.opCodeR, TOperand(o1^), 0);
        Exit;
      end;
    end;

    I_R_RM:
      if ((o1.isReg) and (o2.isRegMem)) then
      begin
        reg := TRegister(o1^); //dst
        Assert(reg.Typ <> REG_GPB);
        op := TOperand(o2^); //src

        _emitX86RM(id.opCode1,
          reg.Typ = REG_GPW,
          reg.Typ = REG_GPQ, reg.code, op,
          0);
        Exit;
      end;

    I_RM_B:
      if (o1.isRegMem) then
      begin
        op := TOperand(o1^);
        _emitX86RM(id.opCode1, False, False, 0, op, 0);
        Exit;
      end;

    I_RM:
      if (o1.isRegMem) then
      begin
        op := TOperand(o1^);
        _emitX86RM(id.opCode1 + UInt8(op.Size <> 1),
          op.Size = 2,
          op.Size = 8, UInt8(id.opCodeR), op,
          0);
        Exit;
      end;

    I_RM_R:
      if ((o1.isRegMem) and (o2.isReg)) then
      begin
        op := TOperand(o1^); //dst
        reg := TRegister(o2^); //src
        _emitX86RM(id.opCode1 + UInt8(reg.Typ <> REG_GPB),
          reg.Typ = REG_GPW,
          reg.Typ = REG_GPQ, reg.code, op,
          0);
        Exit;
      end;

    I_RET:
      if (o1.isNone) then
      begin
        _emitByte($C3);
        Exit;
      end
      else if (o1.isImm) then
      begin
        imm := TImmediate(o1^);
        Assert(isUInt16(imm.value));

        if ((imm.value = 0) and (imm.relocMode = RELOC_NONE)) then
          _emitByte($C3)
        else
        begin
          _emitByte($C2);
          _emitImmediate(imm, 2);
        end;
        Exit;
      end;

    I_ROT:
      if ((o1.isRegMem) and ((o2.isRegCode(REG_CL)) or (o2.isImm))) then
      begin
        // generate opcode. For these operations is base $C0 or $D0.
        useImm8 := ((o2.isImm) and
                   ((TImmediate(o2^).value <> 1) or
                   (TImmediate(o2^).relocMode <> RELOC_NONE)));
        if (useImm8) then
          opCode := $C0
        else
          opCode := $D0;

        if (o1.Size <> 1) then opCode := opCode or $01;
        if (o2.op = OP_REG) then opCode := opCode or $02;

        _emitX86RM(opCode,
          o1.Size = 2,
          o1.Size = 8,
          id.opCodeR, TOperand(o1^),
          Byte(useImm8));
        if (useImm8) then
          _emitImmediate(TImmediate(o2^), 1);
        Exit;
      end;

    I_SHLD_SHRD:
      if ((o1.isRegMem) and (o2.isReg) and ((o3.isImm) or ((o3.isReg) and (o3.isRegCode(REG_CL))))) then
      begin
        op := TOperand(o1^); //dst
        reg := TRegister(o2^); //src1
        //const Operandand src2 := TOperand(o3^);

        Assert(op.Size = reg.Size);

        _emitX86RM(id.opCode1 + UInt8(TOperand(o3^).isReg),
          reg.isRegType(REG_GPW),
          reg.isRegType(REG_GPQ),
          reg.code, op,
          Byte(TOperand(o3^).isImm));
        if (TOperand(o3^).isImm) then
          _emitImmediate(TImmediate(o3^), 1);
        Exit;
      end;

    I_TEST:
    begin
      if ((o1.isRegMem) and (o2.isReg)) then
      begin
        Assert(o1.Size = o2.Size);
        _emitX86RM($84 + UInt8(o2.Size <> 1),
          o2.Size = 2, o2.Size = 8,
          TBaseReg(o2^).code,
          TOperand(o1^),
          0);
        Exit;
      end;

      if ((o1.isRegIndex(0)) and (o2.isImm)) then
      begin
        if (o1.Size <= 4) then
          immSize := o1.Size
        else
          immSize := 4;

        if (o1.Size = 2) then _emitByte($66);
{$IFDEF ASMJIT_X64}
        _emitRexRM(o1.Size = 8, 0, TOperand(o1^));
{$ENDIF}
        _emitByte($A8 + UInt8(o1.Size <> 1));
        _emitImmediate(TImmediate(o2^), UInt32(immSize));
        Exit;
      end;

      if ((o1.isRegMem) and (o2.isImm)) then
      begin
        if (o1.Size <= 4) then
          immSize := o1.Size
        else
          immSize := 4;

        if (o1.Size = 2) then _emitByte($66);
        _emitSegmentPrefix(TOperand(o1^));
{$IFDEF ASMJIT_X64}
        _emitRexRM(o1.Size = 8, 0, TOperand(o1^));
{$ENDIF}
        _emitByte($F6 + UInt8(o1.Size <> 1));
        _emitModRM(0, TOperand(o1^), immSize);
        _emitImmediate(TImmediate(o2^), UInt32(immSize));
        Exit;
      end;
    end;

    I_XCHG:
      if ((o1.isRegMem) and (o2.isReg)) then
      begin
        op := TOperand(o1^); //dst
        reg := TRegister(o2^); //src

        if (reg.isRegType(REG_GPW)) then _emitByte($66);
        _emitSegmentPrefix(op);
{$IFDEF ASMJIT_X64}
        _emitRexRM(reg.isRegType(REG_GPQ), reg.code, op);
{$ENDIF}

        if (((op.op = OP_REG) and (op.Size > 1)) and
            ((TRegister(op).code = 0) or
             (TRegister(reg).code = 0 ))) then
        begin
          index := TRegister(op).code or reg.code;
          _emitByte($90 + index);
          Exit;
        end;

        _emitByte($86 + UInt8(not reg.isRegType(REG_GPB)));
        _emitModRM(reg.code, op, 0);
        Exit;
      end;

    I_MOVBE:
    begin
      if ((o1.isReg) and (o2.isMem)) then
      begin
        _emitX86RM($000F38F0,
          o1.isRegType(REG_GPW),
          o1.isRegType(REG_GPQ),
          TRegister(o1^).code,
          TMem(o2^),
          0);
        Exit;
      end;

      if ((o1.isMem) and (o2.isReg)) then
      begin
        _emitX86RM($000F38F1,
          o2.isRegType(REG_GPW),
          o2.isRegType(REG_GPQ),
          TRegister(o2^).code,
          TMem(o1^),
          0);
        Exit;
      end;
    end;

    I_X87_FPU:
    begin
      if (o1.isRegType(REG_X87)) then
      begin
        i1 := TX87Register(o1^).index;
        i2 := 0;

        if ((code <> INST_FCOM) and (code <> INST_FCOMP)) then
        begin
          if (not o2.isRegType(REG_X87)) then goto illegalInstruction;
          i2 := TX87Register(o2^).index;
        end
        else if ((i1 <> 0) and (i2 <> 0)) then
          goto illegalInstruction;

        if (i1 = 0) then
          _emitByte((id.opCode1 and $FF000000) shr 24)
        else
          _emitByte((id.opCode1 and $00FF0000) shr 16);
        if (i1 = 0) then
          _emitByte(((id.opCode1 and $0000FF00) shr  8) + i2)
        else
          _emitByte(((id.opCode1 and $000000FF)       ) + i1);
        Exit;
      end;

      if ((o1.isMem) and ((o1.Size = 4) or (o1.Size = 8)) and (o2.isNone)) then
      begin
        Mem := TMem(o1^);

        // segment prefix
        _emitSegmentPrefix(Mem);

        if (o1.Size = 4) then
          _emitByte((id.opCode1 and $FF000000) shr 24)
        else
          _emitByte((id.opCode1 and $00FF0000) shr 16);
        _emitModM(id.opCodeR, Mem, 0);
        Exit;
      end;
    end;

    I_X87_STI:
      if (o1.isRegType(REG_X87)) then
      begin
        _emitByte(UInt8((id.opCode1 and $0000FF00) shr 8));
        _emitByte(UInt8((id.opCode1 and $000000FF) + TX87Register(o1^).index));
        Exit;
      end;

    I_X87_FSTSW:
    begin
      if ((o1.isReg) and
          (TBaseReg(o1^).Typ <= REG_GPQ) and
          (TBaseReg(o1^).index = 0)) then
      begin
        _emitOpCode(id.opCode2);
        Exit;
      end;

      if (o1.isMem) then
      begin
        _emitX86RM(id.opCode1, False, False, id.opCodeR, TMem(o1^), 0);
        Exit;
      end;
    end;

    I_X87_MEM_STI, I_X87_MEM:
    begin
      if (id.Group = I_X87_MEM_STI) and (o1.isRegType(REG_X87)) then
      begin
        _emitByte(UInt8((id.opCode2 and $FF000000) shr 24));
        _emitByte(UInt8((id.opCode2 and $00FF0000) shr 16) +
          TX87Register(o1^).index);
        Exit;
      end;

      if (not o1.isMem) then goto illegalInstruction;
      mem := TMem(o1^);

      opCode := $00;
      md := 0;

      if ((o1.Size = 2) and ((id.o1Flags and O_FM_2) <> 0)) then
      begin
        opCode := UInt8((id.opCode1 and $FF000000) shr 24);
        md     := id.opCodeR;
      end;
      if ((o1.Size = 4) and ((id.o1Flags and O_FM_4) <> 0)) then
      begin
        opCode := UInt8((id.opCode1 and $00FF0000) shr 16);
        md     := id.opCodeR;
      end;
      if ((o1.Size = 8) and ((id.o1Flags and O_FM_8) <> 0)) then
      begin
        opCode := UInt8((id.opCode1 and $0000FF00) shr  8);
        md     := UInt8((id.opCode1 and $000000FF)       );
      end;

      if (opCode > 0) then
      begin
        _emitSegmentPrefix(mem);
        _emitByte(opCode);
        _emitModM(md, mem, 0);
        Exit;
      end;
    end;

    I_MMU_MOV:
    begin
      Assert(id.o1Flags <> 0);
      Assert(id.o2Flags <> 0);

      if (((o1.isMem             ) and ((id.o1Flags and O_MEM) = 0)) or
          ((o1.isRegType(REG_MM )) and ((id.o1Flags and O_MM ) = 0)) or
          ((o1.isRegType(REG_XMM)) and ((id.o1Flags and O_XMM) = 0)) or
          ((o1.isRegType(REG_GPD)) and ((id.o1Flags and O_G32) = 0)) or
          ((o1.isRegType(REG_GPQ)) and ((id.o1Flags and O_G64) = 0)) or
          ((o2.isRegType(REG_MM )) and ((id.o2Flags and O_MM ) = 0)) or
          ((o2.isRegType(REG_XMM)) and ((id.o2Flags and O_XMM) = 0)) or
          ((o2.isRegType(REG_GPD)) and ((id.o2Flags and O_G32) = 0)) or
          ((o2.isRegType(REG_GPQ)) and ((id.o2Flags and O_G64) = 0)) or
          ((o2.isMem             ) and ((id.o2Flags and O_MEM) = 0)) ) then
        goto illegalInstruction;

      if ((o1.isMem) and (o2.isMem)) then goto illegalInstruction;

       if (((id.o1Flags or id.o2Flags) and O_NOREX) <> 0) then
         rexw := False
       else
         rexw := o1.isRegType(REG_GPQ) or o1.isRegType(REG_GPQ);

      if ((o1.isReg) and (o2.isReg)) then
      begin
        _emitMmu(id.opCode1, rexw,
          TBaseReg(o1^).code,
          TBaseReg(o2^),
          0);
        Exit;
      end;

      if ((o1.isReg) and (o2.isMem)) then
      begin
        _emitMmu(id.opCode1, rexw,
          TBaseReg(o1^).code,
          TMem(o2^),
          0);
        Exit;
      end;

      if ((o1.isMem) and (o2.isReg)) then
      begin
        _emitMmu(id.opCode2, rexw,
          TBaseReg(o2^).code,
          TMem(o1^),
          0);
        Exit;
      end;
    end;

    I_MMU_MOVD:
    begin
      if (((o1.isRegType(REG_MM)) or (o1.isRegType(REG_XMM))) and ((o2.isRegType(REG_GPD)) or (o2.isMem))) then
      begin
        if (o1.isRegType(REG_XMM)) then
          _emitMmu($66000F6E, False,
            TBaseReg(o1^).code,
            TOperand(o2^),
            0)
        else
          _emitMmu($00000F6E, False,
            TBaseReg(o1^).code,
            TOperand(o2^),
            0);
        Exit;
      end;

      if (((o1.isRegType(REG_GPD)) or (o1.isMem)) and ((o2.isRegType(REG_MM)) or (o2.isRegType(REG_XMM)))) then
      begin
        if (o2.isRegType(REG_XMM)) then
          _emitMmu($66000F7E, False,
            TBaseReg(o2^).code,
            TOperand(o1^),
            0)
        else
          _emitMmu($00000F7E, False,
            TBaseReg(o2^).code,
            TOperand(o1^),
            0);
        Exit;
      end;
    end;

    I_MMU_MOVQ:
    begin
      if ((o1.isRegType(REG_MM)) and (o2.isRegType(REG_MM))) then
      begin
        _emitMmu($00000F6F, False,
          TMMRegister(o1^).code,
          TMMRegister(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegType(REG_XMM)) and (o2.isRegType(REG_XMM))) then
      begin
        _emitMmu($F3000F7E, False,
          TXMMRegister(o1^).code,
          TXMMRegister(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegType(REG_MM)) and (o2.isRegType(REG_XMM))) then
      begin
        _emitMmu($F2000FD6, False,
          TMMRegister(o1^).code,
          TXMMRegister(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegType(REG_XMM)) and (o2.isRegType(REG_MM))) then
      begin
        _emitMmu($F3000FD6, False,
          TXMMRegister(o1^).code,
          TMMRegister(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegType(REG_MM)) and (o2.isMem)) then
      begin
        _emitMmu($00000F6F, False,
          TMMRegister(o1^).code,
          TMem(o2^),
          0);
        Exit;
      end;

      if ((o1.isRegType(REG_XMM)) and (o2.isMem)) then
      begin
        _emitMmu($F3000F7E, False,
          TXMMRegister(o1^).code,
          TMem(o2^),
          0);
        Exit;
      end;

      if ((o1.isMem) and (o2.isRegType(REG_MM))) then
      begin
        _emitMmu($00000F7F, False,
          TMMRegister(o2^).code,
          TMem(o1^),
          0);
        Exit;
      end;

      if ((o1.isMem) and (o2.isRegType(REG_XMM))) then
      begin
        _emitMmu($66000FD6, False,
          TXMMRegister(o2^).code,
          TMem(o1^),
          0);
        Exit;
      end;

{$IFDEF ASMJIT_X64}
      if (((o1.isRegType(REG_MM)) or (o1.isRegType(REG_XMM))) and ((o2.isRegType(REG_GPQ)) or (o2.isMem))) then
      begin
        if (o1.isRegType(REG_XMM)) then
          _emitMmu($66000F6E, True,
            TBaseReg(o1^).code,
            TOperand(o2^),
            0)
        else
          _emitMmu($00000F6E, True,
            TBaseReg(o1^).code,
            TOperand(o2^),
            0);
        Exit;
      end;

      if (((o1.isRegType(REG_GPQ)) or (o1.isMem)) and ((o2.isRegType(REG_MM)) or (o2.isRegType(REG_XMM)))) then
      begin
        if (o2.isRegType(REG_XMM)) then
          _emitMmu($66000F7E, True,
            TBaseReg(o2^).code,
            TOperand(o1^),
            0)
        else
          _emitMmu($00000F7E, True,
            TBaseReg(o2^).code,
            TOperand(o1^),
            0);
        Exit;
      end;
{$ENDIF}
    end;

    I_MMU_PREFETCH:
      if ((o1.isMem) and (o2.isImm)) then
      begin
        Mem := TMem(o1^);
        imm := TImmediate(o2^);

        _emitMmu($00000F18, False, UInt8(imm.value), Mem, 0);
        Exit;
      end;

    I_MMU_PEXTR:
    begin
      if ((not o1.isRegMem) and
         ((o2.isRegType(REG_XMM)) or ((code = INST_PEXTRW) and (o2.isRegType(REG_MM)))) and
          (o3.isImm)) then
        goto illegalInstruction;

      opCode := id.opCode1;
      isGpdGpq := o1.isRegType(REG_GPD) or o1.isRegType(REG_GPQ);

      if ((code = INST_PEXTRB) and ((o1.Size <> 0) and (o1.Size <> 1)) and (not isGpdGpq)) then goto illegalInstruction;
      if ((code = INST_PEXTRW) and ((o1.Size <> 0) and (o1.Size <> 2)) and (not isGpdGpq)) then goto illegalInstruction;
      if ((code = INST_PEXTRD) and ((o1.Size <> 0) and (o1.Size <> 4)) and (not isGpdGpq)) then goto illegalInstruction;
      if ((code = INST_PEXTRQ) and ((o1.Size <> 0) and (o1.Size <> 8)) and (not isGpdGpq)) then goto illegalInstruction;

      if (o2.isRegType(REG_XMM)) then opCode := opCode or $66000000;

      if (o1.isReg) then
      begin
        _emitMmu(opCode, Boolean(id.opCodeR) or o1.isRegType(REG_GPQ),
          TBaseReg(o2^).code,
          TBaseReg(o1^), 1);
        _emitImmediate(TImmediate(o3^), 1);
        Exit;
      end;

      if (o1.isMem) then
      begin
        _emitMmu(opCode, Boolean(id.opCodeR),
          TBaseReg(o2^).code,
          TMem(o1^), 1);
        _emitImmediate(TImmediate(o3^), 1);
        Exit;
      end;
    end;

    I_MMU_RMI:
    begin
      Assert(id.o1Flags <> 0);
      Assert(id.o2Flags <> 0);

      // Check parameters (X)MM|GP32_64 <- (X)MM|GP32_64|Mem|Imm
      if ((not o1.isReg) or
          ((o1.isRegType(REG_MM )) and ((id.o1Flags and O_MM ) = 0)) or
          ((o1.isRegType(REG_XMM)) and ((id.o1Flags and O_XMM) = 0)) or
          ((o1.isRegType(REG_GPD)) and ((id.o1Flags and O_G32) = 0)) or
          ((o1.isRegType(REG_GPQ)) and ((id.o1Flags and O_G64) = 0)) or
          ((o2.isRegType(REG_MM )) and ((id.o2Flags and O_MM ) = 0)) or
          ((o2.isRegType(REG_XMM)) and ((id.o2Flags and O_XMM) = 0)) or
          ((o2.isRegType(REG_GPD)) and ((id.o2Flags and O_G32) = 0)) or
          ((o2.isRegType(REG_GPQ)) and ((id.o2Flags and O_G64) = 0)) or
          ((o2.isMem             ) and ((id.o2Flags and O_MEM) = 0)) or
          ((o2.isImm             ) and ((id.o2Flags and O_IMM) = 0))) then
        goto illegalInstruction;

      if (((id.o1Flags and O_MM_XMM) = O_MM_XMM) and (o1.isRegType(REG_XMM))) or
         (((id.o2Flags and O_MM_XMM) = O_MM_XMM) and (o2.isRegType(REG_XMM))) then
        prefix := $66000000
      else
        prefix := $00000000;
      if (((id.o1Flags or id.o2Flags) and O_NOREX) <> 0) then
        rexw := False
      else
        rexw := o1.isRegType(REG_GPQ) or o1.isRegType(REG_GPQ);

      if (o2.isReg) then
      begin
        if (id.o2Flags and ((O_MM_XMM or O_G32_64)) = 0) then goto illegalInstruction;
        _emitMmu(id.opCode1 or prefix, rexw,
          TBaseReg(o1^).code,
          TBaseReg(o2^), 0);
        Exit;
      end;

      if (o2.isMem) then
      begin
        if ((id.o2Flags and O_MEM) = 0) then goto illegalInstruction;
        _emitMmu(id.opCode1 or prefix, rexw,
          TBaseReg(o1^).code,
          TMem(o2^), 0);
        Exit;
      end;

      if (o2.isImm) then
      begin
        if ((id.o2Flags and O_IMM) = 0) then goto illegalInstruction;
        _emitMmu(id.opCode2 or prefix, rexw,
          id.opCodeR,
          TBaseReg(o1^), 1);
        _emitImmediate(TImmediate(o2^), 1);
        Exit;
      end;
    end;

    I_MMU_RM_IMM8:
    begin
      Assert(id.o1Flags <> 0);
      Assert(id.o2Flags <> 0);

      if ((not o1.isReg) or
          ((o1.isRegType(REG_MM )) and ((id.o1Flags and O_MM ) = 0)) or
          ((o1.isRegType(REG_XMM)) and ((id.o1Flags and O_XMM) = 0)) or
          ((o1.isRegType(REG_GPD)) and ((id.o1Flags and O_G32) = 0)) or
          ((o1.isRegType(REG_GPQ)) and ((id.o1Flags and O_G64) = 0)) or
          ((o2.isRegType(REG_MM )) and ((id.o2Flags and O_MM ) = 0)) or
          ((o2.isRegType(REG_XMM)) and ((id.o2Flags and O_XMM) = 0)) or
          ((o2.isRegType(REG_GPD)) and ((id.o2Flags and O_G32) = 0)) or
          ((o2.isRegType(REG_GPQ)) and ((id.o2Flags and O_G64) = 0)) or
          ((o2.isMem             ) and ((id.o2Flags and O_MEM) = 0)) or
          (not o3.isImm)) then
        goto illegalInstruction;

      if (((id.o1Flags and O_MM_XMM) = O_MM_XMM) and (o1.isRegType(REG_XMM))) or
         (((id.o2Flags and O_MM_XMM) = O_MM_XMM) and (o2.isRegType(REG_XMM))) then
        prefix := $66000000
      else
        prefix := $00000000;
      if (((id.o1Flags or id.o2Flags) and O_NOREX) <> 0) then
        rexw := False
      else
        rexw := o1.isRegType(REG_GPQ) or o1.isRegType(REG_GPQ);

      if (o2.isReg) then
      begin
        if ((id.o2Flags and (O_MM_XMM or O_G32_64)) = 0) then goto illegalInstruction;
        _emitMmu(id.opCode1 or prefix, rexw,
          TBaseReg(o1^).code,
          TBaseReg(o2^), 1);
        _emitImmediate(TImmediate(o3^), 1);
        Exit;
      end;

      if (o2.isMem) then
      begin
        if ((id.o2Flags and O_MEM) = 0) then goto illegalInstruction;
        _emitMmu(id.opCode1 or prefix, rexw,
          TBaseReg(o1^).code,
          TMem(o2^), 1);
        _emitImmediate(TImmediate(o3^), 1);
        Exit;
      end;
    end;

    I_MMU_RM_3DNOW:
    begin
      if ((o1.isRegType(REG_MM)) and ((o2.isRegType(REG_MM)) or (o2.isMem))) then
      begin
        _emitMmu(id.opCode1, False,
          TBaseReg(o1^).code,
          TMem(o2^), 1);
        _emitByte(UInt8(id.opCode2));
        Exit;
      end;
    end;
  end;

illegalInstruction:
  setError(ERROR_ILLEGAL_INSTRUCTION);
  Assert(False);
end;


procedure TAssembler._embed(data: Pointer; Size: SysUInt);
var
  i, j, max: SysUInt;
  buf: string;
begin
  if (not canEmit) then Exit;
  buf := '.data ';

  if ((FLogger <> nil) and FLogger.enabled) then
  begin
    i := 0;
    while (i < Size) do
    begin
      if (Size - i < 16) then
        max := Size - i
      else
        max := 16;

      if (max <= 0) then
        Continue;

      for j := 0 to max - 1 do
        buf := buf + Format('%0.2X', [PByteArray(data)^[i+j]]);

      buf := buf + LineEnding;

      FLogger.log(buf);
      i := i + 16;
    end;
  end;

  FBuffer.emitData(data, Size);
end;

procedure TAssembler._embedLabel(Lbl: PLabel);
var
  link: PLinkData;
  rd: PRelocData;
  buf: string;
begin
  if (not canEmit) then Exit;

  if ((FLogger <> nil) and FLogger.Enabled and (FLogger is TLogger)) then
  begin
    buf := '.data '+ TLogger(FLogger).dumpLabel(Lbl^);
    buf := buf + LineEnding;
    FLogger.log(buf);
  end;

  new(rd);
  rd.Typ := RELATIVE_TO_ABSOLUTE;
  rd.Size := SizeOf(Pointer);
  rd.offset := offset;
  rd.destination := 0;

  if (Lbl.isBound) then
    rd.destination := Lbl.Position
  else
  begin
    // Chain with label
    link := _newLinkData;
    link.prev := Lbl.u._lbl.link;
    link.offset := offset;
    link.displacement := 0;
    link.relocId := FRelocData.length;

    Lbl.u._lbl.link := link;
    Lbl.u._lbl.state := LABEL_STATE_LINKED;
  end;

  FRelocData.append(rd);
  FMemZone.append(rd);
  _emitSysInt(0);
end;

procedure TAssembler.align(m: SysInt);
const
  nop1: array[0..0] of UInt8 = ($90);
  nop2: array[0..1] of UInt8 = ($66, $90);
  nop3: array[0..2] of UInt8 = ($0F, $1F, $00);
  nop4: array[0..3] of UInt8 = ($0F, $1F, $40, $00);
  nop5: array[0..4] of UInt8 = ($0F, $1F, $44, $00, $00);
  nop6: array[0..5] of UInt8 = ($66, $0F, $1F, $44, $00, $00);
  nop7: array[0..6] of UInt8 = ($0F, $1F, $80, $00, $00, $00, $00);
  nop8: array[0..7] of UInt8 = ($0F, $1F, $84, $00, $00, $00, $00, $00);
  nop9: array[0..8] of UInt8 = ($66, $0F, $1F, $84, $00, $00, $00, $00, $00);

  nop10: array[0..9]  of UInt8 = ($66, $66, $0F, $1F, $84, $00, $00, $00, $00, $00);
  nop11: array[0..10] of UInt8 = ($66, $66, $66, $0F, $1F, $84, $00, $00, $00, $00, $00);
var
  i, n: SysInt;
  ci: TCpuInfo;
  p: PUInt8;
begin
  if (not canEmit) then Exit;
  if (FLogger <> nil) then FLogger.logAlign(m);

  if (m = 0) then Exit;

  if (m > 64) then
  begin
    Assert(False);
    Exit;
  end;

  i := m - (offset mod m);
  if (i = m) then Exit;

  if ((FProperties and (1 shl PROPERTY_OPTIMIZE_ALIGN)) <> 0) then
  begin
    ci := getCpuInfo;

    if ((ci.vendorId = Vendor_INTEL) and
       (((ci.family and $0F) = 6) or
        ((ci.family and $0F) = 15))
       ) then
    begin
      repeat
        case i of
          1: begin p := @nop1[0]; n := 1; end;
          2: begin p := @nop2[0]; n := 2; end;
          3: begin p := @nop3[0]; n := 3; end;
          4: begin p := @nop4[0]; n := 4; end;
          5: begin p := @nop5[0]; n := 5; end;
          6: begin p := @nop6[0]; n := 6; end;
          7: begin p := @nop7[0]; n := 7; end;
          8: begin p := @nop8[0]; n := 8; end;
          else begin p := @nop9[0]; n := 9; end;
        end;

        i := i - n;
        repeat
          _emitByte(p^);
          Inc(p);
          Dec(n);
        until(n <= 0);
      until(i <= 0);

      Exit;
    end;

    if ((ci.vendorId = DAsmJit_CpuInfo.Vendor_AMD) and
        (ci.family >= $0F)) then
    begin
      repeat
        case i of
          1: begin p := @nop1[0] ; n :=  1; end;
          2: begin p := @nop2[0] ; n :=  2; end;
          3: begin p := @nop3[0] ; n :=  3; end;
          4: begin p := @nop4[0] ; n :=  4; end;
          5: begin p := @nop5[0] ; n :=  5; end;
          6: begin p := @nop6[0] ; n :=  6; end;
          7: begin p := @nop7[0] ; n :=  7; end;
          8: begin p := @nop8[0] ; n :=  8; end;
          9: begin p := @nop9[0] ; n :=  9; end;
          10: begin p := @nop10[0]; n := 10; end;
          else begin p := @nop11[0]; n := 11; end;
        end;

        i := i - n;
        repeat
          _emitByte(p^);
          Inc(p);
          Dec(n);
        until(n <= 0);
      until(i <= 0);

      Exit;
    end;
{$IFDEF ASMJIT_X86}
    repeat
      case i of
        3: begin _emitByte($66); Dec(i); end;
        2: begin _emitByte($66); Dec(i); end;
        1: begin _emitByte($90); Dec(i);end;
        else begin _emitByte($66); Dec(i); end;
      end;
    until (i <= 0);
{$ENDIF}
  end;

  repeat
    _emitByte($90);
    Dec(i);
  until(i <= 0);
end;

function TAssembler.newLabel: TLabel;
begin
  Result.Create;
end;

procedure TAssembler.bind(Lbl: PLabel);
begin
  Assert(not Lbl.isBound);

  if (FLogger <> nil) and (FLogger is TLogger) then TLogger(FLogger).logLabel(Lbl^);
  bindTo(Lbl, offset);
end;

procedure TAssembler.bindTo(Lbl: PLabel; Pos: SysInt);
var
  link, prev: PLinkData;
  offset: SysInt;
  patchedValue: Int32;
  Size: UInt32;
begin
  if (Lbl.isLinked) then
  begin
    link := PLinkData(Lbl.u._lbl.link);
    prev := nil;

    while (link <> nil) do
    begin
      offset := link.offset;

      if (link.relocId <> -1) then
      begin
        Inc(PRelocData(FRelocData[link.relocId]).destination, Pos);
      end
      else
      begin
        patchedValue := Int32(Pos - offset + link.displacement);
        Size := getByteAt(offset);

        Assert((Size = 1) or (Size = 4));

        if (Size = 1) then
        begin
          if (isInt8(patchedValue)) then
            setByteAt(offset, UInt8(Int8(patchedValue)))
          else
            setError(ERROR_ILLEGAL_SHORT_JUMP);
        end
        else
          setInt32At(offset, patchedValue);
      end;

      prev := link.prev;
      link := prev;
    end;

    link := PLinkData(Lbl.u._lbl.link);
    if (prev = nil) then prev := link;

    prev.prev := FUnusedLinks;
    FUnusedLinks := link;

    Lbl.u._lbl.link := nil;
  end;

  Lbl.setStatePos(LABEL_STATE_BOUND, Pos);
end;

function TAssembler.Make(memManager: TMemoryManager; allocType: UInt32): Pointer;
var
  p: Pointer;
begin
  if ((error > 0) or (codeSize = 0)) then
  begin
    Result := nil;
    Exit;
  end;

  if (memManager = nil) then memManager := TMemoryManager.global;

  p := memManager.alloc(codeSize, allocType);
  if (p = nil) then
  begin
    setError(ERROR_NO_VIRTUAL_MEMORY);
    Result := nil;
    Exit;
  end;

  RelocCode(p);
  Result := p;
end;


function TAssembler._newLinkData: PLinkdata;
var
  link: PLinkData;
begin
  link := FUnusedLinks;

  if (link <> nil) then
    FUnusedLinks := link.prev
  else
  begin
    //link := _zoneAlloc(SizeOf(TLinkData));
    //GetMem(Link, SizeOf(TLinkData));
    New(link);
    if (link = nil) then
    begin
      Result := nil;
      Exit;
    end;
    FMemZone.append(link);
  end;

  // clean link
  link.prev := nil;
  link.offset := 0;
  link.displacement := 0;
  link.relocId := -1;

  Result := link;
end;

procedure TAssembler._freeLinkData(link: PLinkData);
begin
  link.prev := FUnusedLinks;
  FUnusedLinks := link;
end;

initialization
  _none.Create;
finalization
end.
