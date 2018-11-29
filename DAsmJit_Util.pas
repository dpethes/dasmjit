unit DAsmJit_Util;

{$I DAsmJit.inc}

interface

uses
  DAsmJit;

type
  _DontInitialize = record end;
  _Initialize = record end;

var
  _Init: _Initialize;
  _DontInit: _DontInitialize;

type
  TI32FPUnion = record
    case Integer of
      0: (i: Int32);
      1: (f: Single);
  end;

  TI64FPUnion = record
    case Integer of
      0: (i: Int64);
      1: (f: Double);
  end;

  TMMData = record
    case Integer of
      0: (sb: array[0..7] of Int8);
      1: (ub: array[0..7] of UInt8);
      2: (sw: array[0..3] of Int16);
      3: (uw: array[0..3] of UInt16);
      4: (sd: array[0..1] of Int32);
      5: (ud: array[0..1] of UInt32);
      6: (sq: array[0..0] of Int64);
      7: (uq: array[0..0] of UInt64);
      8: (sf: array[0..1] of Single);
  end;

  TXMMData = record
    case Integer of
      0: (sb: array[0..15] of Int8);
      1: (ub: array[0..15] of UInt8);
      2: (sw: array[0..7] of Int16);
      3: (uw: array[0..7] of UInt16);
      4: (sd: array[0..3] of Int32);
      5: (ud: array[0..3] of UInt32);
      6: (sq: array[0..1] of Int64);
      7: (uq: array[0..1] of UInt64);
      8: (sf: array[0..3] of Single);
      9: (df: array[0..1] of Double);
  end;

  TBuffer = class
  protected
    FData: PUInt8;
    FCur: PUInt8;
    FMax: PUInt8;
    FCapacity: SysInt;
    FGrowThreshold: SysInt;
  public
    constructor Create(growThreshold: SysInt = 16);
    destructor Destroy; override;

    function offset: SysInt;
    function ensureSpace: Boolean;
    function toOffset(o: SysInt): SysInt;
    function realloc(t: SysInt): Boolean;
    function grow: Boolean;
    procedure clear;
    procedure DoFree;
    function take: PUInt8;
    procedure emitByte(x: UInt8);
    procedure emitWord(x: UInt16);
    procedure emitDWord(x: UInt32);
    procedure emitQWord(x: UInt64);
    procedure emitSysInt(x: SysInt);
    procedure emitSysUInt(x: SysUInt);
    procedure emitData(ptr: Pointer; len: SysUInt);

    function getByteAt(pos: SysInt): UInt8;
    function getWordAt(pos: SysInt): UInt16;
    function getDWordAt(pos: SysInt): UInt32;
    function getQWordAt(pos: SysInt): UInt64;
    procedure setByteAt(pos: SysInt; x: UInt8);
    procedure setWordAt(pos: SysInt; x: UInt16);
    procedure setDWordAt(pos: SysInt; x: UInt32);
    procedure setQWordAt(pos: SysInt; x: UInt64);

    property Data: PUInt8 read FData;
    property Cur: PUInt8 read FCur;
    property Maximum: PUInt8 read FMax;
    property Capacity: SysInt read FCapacity;
    property GrowThreshold: SysInt read FGrowThreshold;
  end;

  TPodVector = class
  protected
    FData: Pointer;
    FLength: SysUInt;
    FCapacity: SysUInt;

    function GetIndex(Index: SysUInt): Pointer;
    procedure SetIndex(Index: SysUInt; Val: Pointer);
    function _grow: Boolean;
    function _realloc(t: SysUInt): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure clear;
    procedure DoFree;
    function prepend(Item: Pointer): Boolean;
    function insert(index: SysUInt; Item: Pointer): Boolean;
    function append(Item: Pointer): Boolean;
    function indexOf(Val: Pointer): SysUInt;
    procedure removeAt(i: SysUInt);
    procedure swap(Other: TPodVector);

    property Data: Pointer read FData;
    property Length: SysUInt read FLength;
    property Capacity: SysUInt read FCapacity;
    property Items[Index: SysUInt]: Pointer read GetIndex write SetIndex; default;
  end;

  (*PChunk = ^TChunk;
  TChunk = record
    Prev: PChunk;
    Pos: SysUInt;
    Size: SysUInt;
    Data: array[0..0] of UInt8;
    function Remain: SysUInt;
  end;

  TZone = class
  protected
    FChunks: PChunk;
    FTotal: SysUInt;
    FChunkSize: SysUInt;
  public
    constructor Create(ChunkSize: SysUInt);
    destructor Destroy; override;

    function alloc(size: SysUInt): Pointer;
    procedure clear;
    procedure freeAll;

    property Total: SysUInt read FTotal;
    property ChunkSize: SysUInt read FChunkSize;
  end;*)

procedure MMData_set_sb(var r: TMMData; x0, x1, x2, x3, x4, x5, x6, x7: Int8);
procedure MMData_set_ub(var r: TMMData; x0, x1, x2, x3, x4, x5, x6, x7: UInt8);
procedure MMData_set_sw(var r: TMMData; x0, x1, x2, x3: Int16);
procedure MMData_set_uw(var r: TMMData; x0, x1, x2, x3: UInt16);
procedure MMData_set_sd(var r: TMMData; x0, x1: Int32);
procedure MMData_set_ud(var r: TMMData; x0, x1: UInt32);
procedure MMData_set_sq(var r: TMMData; x0: Int64);
procedure MMData_set_uq(var r: TMMData; x0: UInt64);
procedure MMData_set_sf(var r: TMMData; x0, x1: Single);

procedure XMMData_set_sb(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15: Int8);
procedure XMMData_set_ub(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15: UInt8);
procedure XMMData_set_sw(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7: Int16);
procedure XMMData_set_uw(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7: UInt16);
procedure XMMData_set_sd(var r: TXMMData; x0, x1, x2, x3: Int32);
procedure XMMData_set_ud(var r: TXMMData; x0, x1, x2, x3: UInt32);
procedure XMMData_set_sq(var r: TXMMData; x0, x1: Int64);
procedure XMMData_set_uq(var r: TXMMData; x0, x1: UInt64);
procedure XMMData_set_sf(var r: TXMMData; x0, x1, x2, x3: Single);
procedure XMMData_set_df(var r: TXMMData; x0, x1: Single);

function isInt8(x: SysInt): Boolean; inline;
function isUInt8(x: SysInt): Boolean; inline;
function isInt16(x: SysInt): Boolean; inline;
function isUInt16(x: SysInt): Boolean; inline;
function isInt32(x: SysInt): Boolean; inline;
function isUInt32(x: SysInt): Boolean; inline;

function int32AsFloat(i: Int32): Single; inline;
function floatAsInt32(f: Single): Int32; inline;
function int64AsDouble(i: Int64): Double; inline;
function doubleAsInt32(f: Double): Int64; inline;

implementation

function isInt8(x: SysInt): Boolean; inline;
begin
  Result := (x >= -128) and (x <= 127);
end;

function isUInt8(x: SysInt): Boolean; inline;
begin
  Result := (x >= 0) and (x <= 255);
end;

function isInt16(x: SysInt): Boolean; inline;
begin
  Result := (x >= -32768) and (x <= 32767);
end;

function isUInt16(x: SysInt): Boolean; inline;
begin
  Result := (x >= 0) and (x <= 65535);
end;

function isInt32(x: SysInt): Boolean; inline;
begin
{$IFDEF ASMJIT_X86}
  Result := True;
{$ELSE}
  Result := (x >= Int64(-2147483648)) and (x <= Int64(2147483647));
{$ENDIF}
end;

function isUInt32(x: SysInt): Boolean; inline;
begin
{$IFDEF ASMJIT_X86}
  Result := (x >= 0);
{$ELSE}
  Result := (x >= 0) and (x <= Int64(4294967295));
{$ENDIF}
end;

function int32AsFloat(i: Int32): Single; inline;
var
  u: TI32FPUnion;
begin
  u.i := i;
  Result := u.f;
end;

function floatAsInt32(f: Single): Int32; inline;
var
  u: TI32FPUnion;
begin
  u.f := f;
  Result := u.i;
end;

function int64AsDouble(i: Int64): Double; inline;
var
  u: TI64FPUnion;
begin
  u.i := i;
  Result := u.f;
end;

function doubleAsInt32(f: Double): Int64; inline;
var
  u: TI64FPUnion;
begin
  u.f := f;
  Result := u.i;
end;

procedure MMData_set_sb(var r: TMMData; x0, x1, x2, x3, x4, x5, x6, x7: Int8);
begin
  with r do begin
  sb[0] := x0; sb[1] := x1; sb[2] := x2; sb[3] := x3; sb[4] := x4; sb[5] := x5; sb[6] := x6; sb[7] := x7;
  end;
end;

procedure MMData_set_ub(var r: TMMData; x0, x1, x2, x3, x4, x5, x6, x7: UInt8);
begin
  with r do begin
  ub[0] := x0; ub[1] := x1; ub[2] := x2; ub[3] := x3; ub[4] := x4; ub[5] := x5; ub[6] := x6; ub[7] := x7;
  end;
end;

procedure MMData_set_sw(var r: TMMData; x0, x1, x2, x3: Int16);
begin
  with r do begin
  sw[0] := x0; sw[1] := x1; sw[2] := x2; sw[3] := x3;
  end;
end;

procedure MMData_set_uw(var r: TMMData; x0, x1, x2, x3: UInt16);
begin
  with r do begin
  uw[0] := x0; uw[1] := x1; uw[2] := x2; uw[3] := x3;
  end;
end;

procedure MMData_set_sd(var r: TMMData; x0, x1: Int32);
begin
  with r do begin
  sd[0] := x0; sd[1] := x1;
  end;
end;

procedure MMData_set_ud(var r: TMMData; x0, x1: UInt32);
begin
  with r do begin
  ud[0] := x0; ud[1] := x1;
  end;
end;

procedure MMData_set_sq(var r: TMMData; x0: Int64);
begin
  with r do begin
  sq[0] := x0;
  end;
end;

procedure MMData_set_uq(var r: TMMData; x0: UInt64);
begin
  with r do begin
  uq[0] := x0;
  end;
end;

procedure MMData_set_sf(var r: TMMData; x0, x1: Single);
begin
  with r do begin
  sf[0] := x0; sf[1] := x1;
  end;
end;

procedure XMMData_set_sb(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15: Int8);
begin
  with r do begin
  sb[0] := x0; sb[1] := x1; sb[2] := x2; sb[3] := x3; sb[4] := x4; sb[5] := x5; sb[6] := x6; sb[7] := x7;
  sb[8] := x8; sb[9] := x9; sb[10] := x10; sb[11] := x11; sb[12] := x12; sb[13] := x13; sb[14] := x14; sb[15] := x15;
  end;
end;

procedure XMMData_set_ub(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15: UInt8);
begin
  with r do begin
  ub[0] := x0; ub[1] := x1; ub[2] := x2;   ub[3] := x3;   ub[4] := x4;   ub[5] := x5;   ub[6] := x6;   ub[7] := x7;
  ub[8] := x8; ub[9] := x9; ub[10] := x10; ub[11] := x11; ub[12] := x12; ub[13] := x13; ub[14] := x14; ub[15] := x15;
  end;
end;

procedure XMMData_set_sw(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7: Int16);
begin
  with r do begin
  sw[0] := x0; sw[1] := x1; sw[2] := x2; sw[3] := x3; sw[4] := x4; sw[5] := x5; sw[6] := x6; sw[7] := x7;
  end;
end;

procedure XMMData_set_uw(var r: TXMMData; x0, x1, x2, x3, x4, x5, x6, x7: UInt16);
begin
  with r do begin
  uw[0] := x0; uw[1] := x1; uw[2] := x2; uw[3] := x3; uw[4] := x4; uw[5] := x5; uw[6] := x6; uw[7] := x7;
  end;
end;

procedure XMMData_set_sd(var r: TXMMData; x0, x1, x2, x3: Int32);
begin
  with r do begin
  sd[0] := x0; sd[1] := x1; sd[2] := x2; sd[3] := x3;
  end;
end;

procedure XMMData_set_ud(var r: TXMMData; x0, x1, x2, x3: UInt32);
begin
  with r do begin
  ud[0] := x0; ud[1] := x1; ud[2] := x2; ud[3] := x3;
  end;
end;

procedure XMMData_set_sq(var r: TXMMData; x0, x1: Int64);
begin
  with r do begin
  sq[0] := x0; sq[1] := x1;
  end;
end;

procedure XMMData_set_uq(var r: TXMMData; x0, x1: UInt64);
begin
  with r do begin
  uq[0] := x0; uq[1] := x1;
  end;
end;

procedure XMMData_set_sf(var r: TXMMData; x0, x1, x2, x3: Single);
begin
  with r do begin
  sf[0] := x0; sf[1] := x1; sf[2] := x2; sf[3] := x3;
  end;
end;

procedure XMMData_set_df(var r: TXMMData; x0, x1: Single);
begin
  with r do begin
  df[0] := x0; df[1] := x1;
  end;
end;

constructor TBuffer.Create(GrowThreshold: SysInt = 16);
begin
  inherited Create;

  FGrowThreshold := GrowThreshold;
  FData := nil;
  FCur := nil;
  FMax := nil;
  FCapacity := 0;
end;

destructor TBuffer.Destroy;
begin
  if (FData <> nil) then
    FreeMem(FData);

  inherited;
end;

{function TBuffer.data: PUInt8;
begin
  Result := FData;
end;

function TBuffer.cur: PUInt8;
begin
  Result := FCur;
end;

function TBuffer.Maximum: PUInt8;
begin
  Result := FMax;
end;

function TBuffer.capacity: SysInt;
begin
  Result := FCapacity;
end;

function TBuffer.GrowThreshold: SysInt;
begin
  Result := FGrowThreshold;
end;}

function TBuffer.offset: SysInt;
begin
  Result := SysInt(PtrInt(FCur) - PtrInt(FData));
end;

function TBuffer.ensureSpace: Boolean;
begin
  if (PtrInt(FCur) >= PtrInt(FMax))  then
    Result := Grow
  else
    Result := True;
end;

function TBuffer.toOffset(o: SysInt): SysInt;
var
  Prev: SysInt;
begin
  Assert(o < FCapacity);

  prev := SysInt(PtrInt(FCur) - PtrInt(FData));
  FCur := PUInt8(PtrInt(FData) + o);
  Result := Prev;
end;

procedure TBuffer.emitByte(x: UInt8);
begin
  FCur^ := x;
  Inc(FCur);
end;

procedure TBuffer.emitWord(x: UInt16);
begin
  PUInt16(FCur)^ := x;
  Inc(FCur, 2);
end;

procedure TBuffer.emitDWord(x: UInt32);
begin
  PUInt32(FCur)^ := x;
  Inc(FCur, 4);
end;

procedure TBuffer.emitQWord(x: UInt64);
begin
  PUInt64(FCur)^ := x;
  Inc(FCur, 8);
end;

procedure TBuffer.emitSysInt(x: SysInt);
begin
  PSysInt(FCur)^ := x;
  Inc(FCur, SizeOf(SysInt));
end;

procedure TBuffer.emitSysUInt(x: SysUInt);
begin
  PSysUInt(FCur)^ := x;
  Inc(FCur, SizeOf(SysUInt));
end;

function TBuffer.getByteAt(pos: SysInt): UInt8;
begin
  Result := PUInt8(PtrInt(FData) + pos)^;
end;

function TBuffer.getWordAt(pos: SysInt): UInt16;
begin
  Result := PUInt16(PtrInt(FData) + pos)^;
end;

function TBuffer.getDWordAt(pos: SysInt): UInt32;
begin
  Result := PUInt32(PtrInt(FData) + pos)^;
end;

function TBuffer.getQWordAt(pos: SysInt): UInt64;
begin
  Result := PUInt64(PtrInt(FData) + pos)^;
end;

procedure TBuffer.setByteAt(pos: SysInt; x: UInt8);
begin
  PUInt8(PtrInt(FData) + pos)^ := x;
end;

procedure TBuffer.setWordAt(pos: SysInt; x: UInt16);
begin
  PUInt16(PtrInt(FData) + pos)^ := x;
end;

procedure TBuffer.setDWordAt(pos: SysInt; x: UInt32);
begin
  PUInt32(PtrInt(FData) + pos)^ := x;
end;

procedure TBuffer.setQWordAt(pos: SysInt; x: UInt64);
begin
  PUInt64(PtrInt(FData) + pos)^ := x;
end;

procedure TBuffer.emitData(Ptr: Pointer; Len: SysUInt);
var
  max: SysInt;
begin
  max := capacity - offset;
  if (SysUInt(max) < Len) and (not realloc(SysUInt(offset) + Len)) then
    Exit;

  Move(Ptr^, FCur^, Len);
  Inc(FCur, Len);
end;

function TBuffer.realloc(t: SysInt): Boolean;
var
  len: SysInt;
begin
  if (capacity < t) then
  begin
    len := offset();

    if (FData <> nil) then
      ReallocMem(FData, t)
    else
      GetMem(FData, t);
    if (FData = nil) then
    begin
      Result := False;
      Exit;
    end;

    FCur := PUInt8(PtrInt(FData) + len);
    FMax := PUInt8(PtrInt(FData) + t);
    if (t >= FGrowThreshold) then
      FMax := PUInt8(PtrInt(FMax) - FGrowThreshold)
    else
      FMax := PUInt8(PtrInt(FMax) - t);

    FCapacity := t;
  end;

  Result := True;
end;

function TBuffer.grow: Boolean;
var
  t: SysInt;
begin
  t := FCapacity;

  if (t < 512) then
    t := 1024
  else if (t > 65536) then
    t := t + 65536
  else
    t := t shl 1;

  Result := realloc(t);
end;

procedure TBuffer.clear;
begin
  FCur := FData;
end;

procedure TBuffer.DoFree;
begin
  if (FData = nil) then Exit;
  FreeMem(FData);

  FData := nil;
  FCur := nil;
  FMax := nil;
  FCapacity := 0;
end;

function TBuffer.take: PUInt8;
var
  data: PUInt8;
begin
  data := FData;

  FData := nil;
  FCur := nil;
  FMax := nil;
  FCapacity := 0;

  Result := data;
end;

constructor TPodVector.Create;
begin
  inherited Create;

  FData := nil;
  FLength := 0;
  FCapacity := 0;
end;

destructor TPodVector.Destroy;
begin
  if (FData <> nil) then
    FreeMem(FData);

  inherited Destroy;
end;

function TPodVector.GetIndex(Index: SysUInt): Pointer;
begin
  Assert(Index < FLength);
  Result := PPointer(PtrUInt(FData) + (Index * SizeOf(Pointer)))^;
end;

procedure TPodVector.SetIndex(Index: SysUInt; Val: Pointer);
begin
  Assert(Index < FLength);
  PPointer(PtrUInt(FData) + (Index * SizeOf(Pointer)))^ := Val;
end;

{function TPodVector.data: Pointer;
begin
  Result := FData;
end;

function TPodVector.length: SysUInt;
begin
  Result := FLength;
end;

function TPodVector.capacity: SysUInt;
begin
  Result := FCapacity;
end;}

procedure TPodVector.clear;
begin
  FLength := 0;
end;

procedure TPodVector.DoFree;
begin
  if (FData <> nil) then
  begin
    FreeMem(FData);
    FData := nil;
    FLength := 0;
    FCapacity := 0;
  end;
end;

function TPodVector.prepend(Item: Pointer): Boolean;
var
  dst: Pointer;
begin
  if ((FLength = FCapacity) and (not _Grow)) then
  begin
    Result := False;
    Exit;
  end;

  dst := Pointer(PtrInt(FData) + SizeOf(Pointer));
  Move(FData^, dst^, SizeOf(Pointer) * FLength);
  PPointer(FData)^ := Item;

  Inc(FLength);
  Result := True;
end;

function TPodVector.insert(Index: SysUInt; Item: Pointer): Boolean;
var
  src, dst: Pointer;
begin
  Assert(index <= FLength);
  if ((FLength = FCapacity) and (not _grow)) then
  begin
    Result := False;
    Exit;
  end;

  src := Pointer(PtrUInt(FData) + (index * SizeOf(Pointer)));
  dst := Pointer(PtrUInt(FData) + ((index + 1) * SizeOf(Pointer)));
  Move(src^, dst^, (FLength - index) * SizeOf(Pointer));
  PPointer(src)^ := Item;

  Inc(FLength);
  Result := True;
end;

function TPodVector.append(Item: Pointer): Boolean;
var
  dst: Pointer;
begin
  if ((FLength = FCapacity) and (not _grow)) then
  begin
    Result := False;
    Exit;
  end;

  dst := Pointer(PtrUInt(FData) + (FLength * SizeOf(Pointer)));
  PPointer(dst)^ := Item;

  Inc(FLength);
  Result := True;
end;

function TPodVector.indexOf(Val: Pointer): SysUInt;
var
  i: SysUInt;
begin
  if (FLength > 0) then
  begin
    for i := 0 to FLength - 1 do
      if (Val = GetIndex(i)) then
      begin
        Result := i;
        Exit;
      end;
  end;
  Result := SysUInt(-1);
end;

procedure TPodVector.removeAt(i: SysUInt);
var
  src, dst: Pointer;
begin
  Assert(i < FLength);

  src := Pointer(PtrUInt(FData) + ((i + 1) * SizeOf(Pointer)));
  dst := Pointer(PtrUInt(FData) + (i * SizeOf(Pointer)));
  Dec(FLength);
  Move(src^, dst^, (FLength - i) * SizeOf(Pointer));
end;

procedure TPodVector.swap(Other: TPodVector);
var
  _tmp_data: Pointer;
  _tmp_length, _tmp_capacity: SysUInt;
begin
  _tmp_data := FData;
  _tmp_length := FLength;
  _tmp_capacity := FCapacity;

  FData := other.FData;
  FLength := other.FLength;
  FCapacity := other.FCapacity;

  other.FData := _tmp_data;
  other.FLength := _tmp_length;
  other.FCapacity := _tmp_capacity;
end;

function TPodVector._grow: Boolean;
begin
  if FCapacity < 16 then
    Result := _realloc(16)
  else
    Result := _realloc(FCapacity shl 1);
end;

function TPodVector._realloc(t: SysUInt): Boolean;
begin
  Assert(t >= FLength);

  if (FData <> nil) then
    ReallocMem(FData, t * SizeOf(Pointer))
  else
    GetMem(FData, t * SizeOf(Pointer));

  if (FData = nil) then
  begin
    Result := False;
    Exit;
  end;

  FCapacity := t;
  Result := True;
end;

(*function TChunk.remain: SysUInt;
begin
  Result := Size - Pos;
end;

constructor TZone.Create(ChunkSize: SysUInt);
begin
  inherited Create;

  FChunks := nil;
  FTotal := 0;
  FChunkSize := ChunkSize;
end;

destructor TZone.Destroy;
begin
  FreeAll;

  inherited;
end;

{function TZone.Total: SysUInt;
begin
  Result := FTotal;
end;

function TZone.chunkSize: SysUInt;
begin
  Result := FChunkSize;
end;}

function TZone.alloc(Size: SysUInt): Pointer;
var
  cur: PChunk;
  chSize: SysUInt;
  p: PUInt8;
begin
  Size := (Size + SizeOf(SysInt) - 1) and (not (SizeOf(SysInt) - 1));
  cur := FChunks;

  if ((cur = nil) or (cur.remain < Size)) then
  begin
    chSize := FChunkSize;
    if (chSize < size) then
      chSize := Size;

    GetMem(Cur, SizeOf(TChunk) - SizeOf(Pointer) + chSize);
    if (cur = nil) then
    begin
      Result := nil;
      Exit;
    end;

    FillChar(Cur^, SizeOf(TChunk) - SizeOf(Pointer) + chSize, 0);

    cur.prev := FChunks;
    cur.pos := 0;
    cur.size := FChunkSize;
    FChunks := cur;
  end;

  {$R-}
  p := PUInt8(@cur.data[cur.pos]);
  {$R+}
  cur.pos := cur.pos + size;
  FTotal := FTotal + size;
  Result := p;
end;

procedure TZone.clear;
var
  cur, prev: PChunk;
begin
  cur := FChunks;
  if (cur = nil) then Exit;

  repeat
    prev := cur.prev;
    if (prev <> nil) then
      FreeMem(cur);
    cur := prev;
  until(cur = nil);

  FChunks := prev;
  FChunks.pos := 0;
  FTotal := 0;
end;

procedure TZone.freeAll;
var
  cur, prev: PChunk;
begin
  cur := FChunks;
  if (cur = nil) then Exit;

  repeat
    prev := cur.prev;
    FreeMem(cur);
    Cur := prev;
  until(cur = nil);

  FChunks := nil;
  FTotal := 0;
end;*)

end.
