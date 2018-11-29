unit DAsmJit_VirtualMemory;

{$I DAsmJit.inc}

interface

uses
  {$IFDEF ASMJIT_WINDOWS}Windows,{$ENDIF} DAsmJit;

type
  TVirtualMemory = class
    class function Alloc(Length: SysUInt; Allocated: PSysUInt; canExecute: Boolean): Pointer;
    class procedure FreeAddress(Addr: Pointer; Length: SysUInt);
    class function Alignment: SysUInt;
    class function PageSize: SysUInt;
  end;

implementation

type
  TVirtualMemoryLocal = class
  public
    Alignment: SysUInt;
    pageSize: SysUInt;

    constructor Create;
  end;

var
  vm: TVirtualMemoryLocal;

function isAligned(Base, Alignment: SysUInt): Boolean;
begin
  Result := (Base mod Alignment) = 0;
end;

function roundUp(Base, pageSize: SysUInt): SysUInt;
var
  over: SysUInt;
begin
  over := Base mod pageSize;
  if (over > 0) then
    Result := Base + (pageSize - over)
  else
    Result := Base;
end;

function roundUpToPowerOf2(Base: SysUInt): SysUInt;
begin
  Dec(Base);

  Base := Base or (Base shr 1);
  Base := Base or (Base shr 2);
  Base := Base or (Base shr 4);
  Base := Base or (Base shr 8);
  Base := Base or (Base shr 16);

{$IFDEF ASMJIT_X64}
  Base := Base or (Base shr 32);
{$ENDIF}

  Result := Base + 1;
end;

constructor TVirtualMemoryLocal.Create;
{$IFDEF ASMJIT_WINDOWS}
var
  Info: TSystemInfo;
{$ENDIF}
begin
  inherited;

  {$IFDEF ASMJIT_WINDOWS}
  GetSystemInfo(Info);
  Alignment := Info.dwAllocationGranularity;
  pageSize := roundUpToPowerOf2(Info.dwPageSize);
  {$ELSE}
  Alignment := getpagesize;
  pageSize := Alignment;
  {$ENDIF}
end;

class function TVirtualMemory.Alloc(Length: SysUInt; Allocated: PSysUInt; canExecute: Boolean): Pointer;
var
  mSize: SysUInt;
  Protect: Integer;
  mBase: Pointer;
{$IFDEF ASMJIT_WINDOWS}
  mProt: Pointer;
{$ENDIF ASMJIT_WINDOWS}
begin
  mSize := RoundUp(Length, vm.pageSize);

{$IFDEF ASMJIT_WINDOWS}
  if canExecute then
    Protect := PAGE_EXECUTE_READWRITE
  else
    Protect := PAGE_READWRITE;
  mBase := VirtualAlloc(nil, mSize, MEM_COMMIT or MEM_RESERVE, Protect);
  mProt := nil;
  VirtualProtect(mBase, mSize, PAGE_EXECUTE, mProt);
{$ELSE}
  Protect := PROT_READ or PROT_WRITE;
  if canExecute then
    Protect := Protect or PROT_EXEC;
  mBase := mmap(nil, mSize, Protection, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
{$ENDIF}

{$IFDEF ASMJIT_WINDOWS}
  if (mBase = nil) then
{$ELSE}
  if (mBase = MAP_FAILED) then
{$ENDIF}
  begin
    Result := nil;
    Exit;
  end;

{$IFDEF ASMJIT_WINDOWS}
  Assert(isAligned(SysUInt(mBase), vm.Alignment));
{$ENDIF}

  if (Allocated <> nil) then
    Allocated^ := mSize;
  Result := mBase;
end;

class procedure TVirtualMemory.FreeAddress(Addr: Pointer; Length: SysUInt);
begin
{$IFDEF ASMJIT_WINDOWS}
  VirtualFree(Addr, 0, MEM_RELEASE);
{$ELSE}
  munmap(Addr, Length);
{$ENDIF}
end;

class function TVirtualMemory.Alignment: SysUInt;
begin
  Result := vm.Alignment;
end;

class function TVirtualMemory.PageSize: SysUInt;
begin
  Result := vm.pageSize
end;

initialization
  vm := TVirtualMemoryLocal.Create;
finalization
  vm.Free;

end.
