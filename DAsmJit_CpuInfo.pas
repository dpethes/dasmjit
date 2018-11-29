unit DAsmJit_CpuInfo;

{$I DAsmJit.inc}

interface

uses
  {$IFDEF ASMJIT_WINDOWS}Windows,{$ENDIF} DAsmJit;

type
  PCpuId = ^ TCpuId;
  TCpuId = record
    case UInt32 of
      0: (i: array[0..3] of UInt32);
      1: (eax: UInt32;
          ebx: UInt32;
          ecx: UInt32;
          edx: UInt32;)
  end;

const
  //TVendorID_E = (
    Vendor_Unknown = 0;
    Vendor_INTEL = 1;
    Vendor_AMD = 2;
    Vendor_VIA = 3;
  //);

type
  TX86ExtendedInfo = record
    ProcessorType: UInt32;
    BrandIndex: UInt32;
    clFlushCacheLineSize: UInt32;
    LogicalProcessors: UInt32;
    apicPhysicalId: UInt32;
  end;

  PCpuInfo = ^TCpuInfo;
  TCpuInfo = record
    Vendor: AnsiString;
    VendorId: UInt32;
    Family: UInt32;
    Model: UInt32;
    Stepping: UInt32;
    NumberOfProcessors: UInt32;
    Features: UInt32;
    Bugs: UInt32;
    x86ExtendedInfo: TX86ExtendedInfo;
  end;

const
  //TFeature_E = (
    Feature_RDTSC = SysUInt(1) shl 0;
    Feature_RDTSCP = SysUInt(1) shl 1;
    Feature_CMOV = SysUInt(1) shl 2;
    Feature_CMPXCHG8B = SysUInt(1) shl 3;
    Feature_CMPXCHG16B = SysUInt(1) shl 4;
    Feature_CLFLUSH = SysUInt(1) shl 5;
    Feature_PREFETCH = SysUInt(1) shl 6;
    Feature_LAHF_SAHF = SysUInt(1) shl 7;
    Feature_FXSR = SysUInt(1) shl 8;
    Feature_FFXSR = SysUInt(1) shl 9;
    Feature_MMX = SysUInt(1) shl 10;
    Feature_MMXExt = SysUInt(1) shl 11;
    Feature_3dNow = SysUInt(1) shl 12;
    Feature_3dNowExt = SysUInt(1) shl 13;
    Feature_SSE = SysUInt(1) shl 14;
    Feature_MSSE = SysUInt(1) shl 15;
    Feature_SSE2 = SysUInt(1) shl 16;
    Feature_SSE3 = SysUInt(1) shl 17;
    Feature_SSSE3 = SysUInt(1) shl 18;
    Feature_SSE4_A = SysUInt(1) shl 19;
    Feature_SSE4_1 = SysUInt(1) shl 20;
    Feature_SSE4_2 = SysUInt(1) shl 21;
    Feature_SSE5 = SysUInt(1) shl 22;
    Feature_MotitorMWait = SysUInt(1) shl 23;
    Feature_POPCNT = SysUInt(1) shl 24;
    Feature_LZCNT  = SysUInt(1) shl 25;
    Feature_MultiThreading = SysUInt(1) shl 29;
    Feature_ExecuteDisableBit = SysUInt(1) shl 30;
    Feature_64Bit = SysUInt(1) shl 31;
  //);

//const
  Bug_AmdLockMB = SysUInt(1) shl 0;

var
  getCpuInfo: TCpuInfo;

procedure cpuId(varIn: NativeUInt; varOut: PCpuId);
procedure detectCpuInfo(i: PCpuInfo);

implementation

type
  TVendorInfo = record
    id: UInt32;
    Text: AnsiString;
  end;

const
  VendorInfo: array[0..3] of TVendorInfo = (
    (id: Vendor_INTEL; Text: 'GenuineIntel'),
    (id: Vendor_AMD;   Text: 'AMDisbetter!'),
    (id: Vendor_AMD;   Text: 'AuthenticAMD'),
    (id: Vendor_VIA;   Text: 'VIA'#0'VIA'#0'VIA'#0)
  );

function detectNumberOfProcessors: UInt32;
var
{$IFDEF ASMJIT_WINDOWS}
  Info: TSystemInfo;
{$ELSE}
  res: SysUInt;
{$ENDIF}
begin
{$IFDEF ASMJIT_WINDOWS}
  GetSystemInfo(Info);
  Result := Info.dwNumberOfProcessors;
{$ELSE}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  if (Result = -1) then Result := 1;
{$ENDIF}
end;

procedure cpuId(varIn: NativeUInt; varOut: PCpuId); assembler; {$asmmode intel}
asm
  {$IFDEF ASMJIT_X64}

  //x64
  push rax
  push rbx
  push rcx
  push rdx
  push rdi
  mov     rax, varIn
  mov     rdi, varOut
  cpuid
  mov     dword64 ptr[rdi +   0], rax
  mov     dword64 ptr[rdi +   4], rbx
  mov     dword64 ptr[rdi +   8], rcx
  mov     dword64 ptr[rdi +  12], rdx
  pop rdi
  pop rdx
  pop rcx
  pop rbx
  pop rax

  {$ELSE}

  //x86
  push eax
  push ebx
  push ecx
  push edx
  push edi
  mov     eax, varIn
  mov     edi, varOut
  cpuid
  mov     dword ptr[edi +   0], eax
  mov     dword ptr[edi +   4], ebx
  mov     dword ptr[edi +   8], ecx
  mov     dword ptr[edi +  12], edx
  pop edi
  pop edx
  pop ecx
  pop ebx
  pop eax

  {$ENDIF}
end;

procedure detectCpuInfo(i: PCpuInfo);

  function Min(a, b: SysUInt): SysUInt;
  begin
    if (b < a) then
      Result := b
    else
      Result := a;
  end;

var
  a{, exIds}: SysUInt;
  idOut: TCpuId;
  v: PAnsiChar;
begin
  FillChar(i^, SizeOf(TCpuInfo), 0);
  i.Vendor := 'Unknown';
  i.NumberOfProcessors := detectNumberOfProcessors;

  cpuId(0, @idOut);

  GetMem(v, 12 * SizeOf(AnsiChar));
  FillChar(v^, 12 * SizeOf(AnsiChar), 0);
  Move(idOut.ebx, v^, 4);
  Inc(v, 4);
  Move(idOut.edx, v^, 4);
  Inc(v, 4);
  Move(idOut.ecx, v^, 4);
  Dec(v, 8);
  i.Vendor := v;
  FreeMem(v);

  for a := 0 to 2 do
    if (i.Vendor = VendorInfo[a].Text) then
    begin
      i.vendorId := VendorInfo[a].id;
      Break;
    end;

  cpuId(1, @idOut);

  i.family   := (idOut.eax shr 8) and $0F;
  i.model    := (idOut.eax shr 4) and $0F;
  i.stepping := (idOut.eax      ) and $0F;

  if (i.family = $0F) then
  begin
    Inc(i.family, ((idOut.eax shr 20) and $FF));
    Inc(i.model,  ((idOut.eax shr 16) and $0F) shl 4);
  end;

  i.x86ExtendedInfo.processorType         := ((idOut.eax shr 12) and $03);
  i.x86ExtendedInfo.brandIndex            := ((idOut.ebx       ) and $FF);
  i.x86ExtendedInfo.clFlushCacheLineSize  := ((idOut.ebx shr  8) and $FF) * 8;
  i.x86ExtendedInfo.logicalProcessors     := ((idOut.ebx shr 16) and $FF);
  i.x86ExtendedInfo.apicPhysicalId        := ((idOut.ebx shr 24) and $FF);

  if ((idOut.ecx and SysUInt($00000001)) > 0) then i.features := i.Features or Feature_SSE3;
  if ((idOut.ecx and SysUInt($00000008)) > 0) then i.features := i.features or Feature_MotitorMWait;
  if ((idOut.ecx and SysUInt($00000200)) > 0) then i.features := i.features or Feature_SSSE3;
  if ((idOut.ecx and SysUInt($00002000)) > 0) then i.features := i.features or Feature_CMPXCHG16B;
  if ((idOut.ecx and SysUInt($00080000)) > 0) then i.features := i.features or Feature_SSE4_1;
  if ((idOut.ecx and SysUInt($00100000)) > 0) then i.features := i.features or Feature_SSE4_2;
  if ((idOut.ecx and SysUInt($00800000)) > 0) then i.features := i.features or Feature_POPCNT;

  if ((idOut.edx and SysUInt($00000010)) > 0) then i.features := i.features or Feature_RDTSC;
  if ((idOut.edx and SysUInt($00000100)) > 0) then i.features := i.features or Feature_CMPXCHG8B;
  if ((idOut.edx and SysUInt($00008000)) > 0) then i.features := i.features or Feature_CMOV;
  if ((idOut.edx and SysUInt($00800000)) > 0) then i.features := i.features or Feature_MMX;
  if ((idOut.edx and SysUInt($01000000)) > 0) then i.features := i.features or Feature_FXSR;
  if ((idOut.edx and SysUInt($02000000)) > 0) then i.features := i.features or Feature_SSE or Feature_MMXExt;
  if ((idOut.edx and SysUInt($04000000)) > 0) then i.features := i.features or Feature_SSE or Feature_SSE2;
  if ((idOut.edx and SysUInt($10000000)) > 0) then i.features := i.features or Feature_MultiThreading;

  if ((i.vendorId = Vendor_AMD) and ((idOut.edx and SysUInt($10000000)) > 0)) then
    if (i.numberOfProcessors = 1) then i.numberOfProcessors := 2;

  if ((i.vendorId = Vendor_AMD) and (i.family = 15) and (i.model >= 32) and (i.model <= 63)) then
    i.bugs := i.bugs or Bug_AmdLockMB;

  cpuid($80000000, @idOut);
  //exIds := Min(idOut.eax - 1, $80000001);

  //for a := UInt32($80000001) to exIds do
  begin
    cpuid($80000001, @idOut);

    //case a of
    //  $80000001:
      begin
        if ((idOut.ecx and SysUInt($00000001)) > 0) then i.features := i.Features or Feature_LAHF_SAHF;
        if ((idOut.ecx and SysUInt($00000020)) > 0) then i.features := i.features or Feature_LZCNT;
        if ((idOut.ecx and SysUInt($00000040)) > 0) then i.features := i.features or Feature_SSE4_A;
        if ((idOut.ecx and SysUInt($00000080)) > 0) then i.features := i.features or Feature_MSSE;
        if ((idOut.ecx and SysUInt($00000100)) > 0) then i.features := i.features or Feature_PREFETCH;
        if ((idOut.ecx and SysUInt($00000800)) > 0) then i.features := i.features or Feature_SSE5;

        if ((idOut.edx and SysUInt($00100000)) > 0) then i.features := i.features or Feature_ExecuteDisableBit;
        if ((idOut.edx and SysUInt($00200000)) > 0) then i.features := i.features or Feature_FFXSR;
        if ((idOut.edx and SysUInt($00400000)) > 0) then i.features := i.features or Feature_MMXExt;
        if ((idOut.edx and SysUInt($08000000)) > 0) then i.features := i.features or Feature_RDTSCP;
        if ((idOut.edx and SysUInt($20000000)) > 0) then i.features := i.features or Feature_64Bit;
        if ((idOut.edx and SysUInt($40000000)) > 0) then i.features := i.features or Feature_3dNowExt or Feature_MMXExt;
        if ((idOut.edx and SysUInt($80000000)) > 0) then i.features := i.features or Feature_3dNow;
      end;
  //  end;
  end;
end;

initialization
  detectCpuInfo(@getCpuInfo);
finalization
end.
