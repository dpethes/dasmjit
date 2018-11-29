unit DAsmJit_Logger;

{$I DAsmJit.inc}

interface

uses
  DAsmJit, Classes, DAsmJit_Serializer, DAsmJit_Defs;

type
  TLogger = class(TCustomLogger)
  protected
    //FEnabled: Boolean;
    FHaveStream: Boolean;
  public
    constructor Create; virtual;

    procedure log(buf: string); override;
    procedure logInstruction(code: UInt32; o1, o2, o3: POperand; inlineComment: string = ''); virtual;
    procedure logAlign(m: SysInt); override;
    procedure logLabel(Lbl: TLabel); virtual;
    procedure logFormat(fmt: string; args: array of const); override;

    class function dumpInstruction(code: UInt32): string;
    class function dumpOperand(op: POperand): string;
    class function dumpRegister(typ, index: UInt8): string;
    class function dumpLabel(Lbl: TLabel): string;

    //property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TStreamLogger = class(TLogger)
  protected
    FStream: TStream;

    procedure SetStream(Stream: TStream);
  public
    constructor Create(Stream: TStream = nil); reintroduce;

    procedure log(buf: string); override;
    property Stream: TStream read FStream write SetStream;
  end;

  TLoggerCallback = procedure(buf: string) of object;
  TCallbackLogger = class(TLogger)
  protected
    cb: TLoggerCallback;
  public
    constructor Create(CallBack: TLoggerCallback = nil); reintroduce;

    procedure log(buf: string); override;
    property Callback: TLoggerCallback read cb write cb;
  end;

implementation

uses
  SysUtils;

const
 instructionName: array[0..577] of pchar = (
  'adc',
  'add',
  'addpd',
  'addps',
  'addsd',
  'addss',
  'addsubpd',
  'addsubps',
  'amd_prefetch',
  'amd_prefetchw',
  'and',
  'andnpd',
  'andnps',
  'andpd',
  'andps',
  'blendpd',
  'blendps',
  'blendvpd',
  'blendvps',
  'bsf',
  'bsr',
  'bswap',
  'bt',
  'btc',
  'btr',
  'bts',
  'call',
  'cbw',
  'cdqe',
  'clc',
  'cld',
  'clflush',
  'cmc',
  'cmova',
  'cmovae',
  'cmovb',
  'cmovbe',
  'cmovc',
  'cmove',
  'cmovg',
  'cmovge',
  'cmovl',
  'cmovle',
  'cmovna',
  'cmovnae',
  'cmovnb',
  'cmovnbe',
  'cmovnc',
  'cmovne',
  'cmovng',
  'cmovnge',
  'cmovnl',
  'cmovnle',
  'cmovno',
  'cmovnp',
  'cmovns',
  'cmovnz',
  'cmovo',
  'cmovp',
  'cmovpe',
  'cmovpo',
  'cmovs',
  'cmovz',
  'cmp',
  'cmppd',
  'cmpps',
  'cmpsd',
  'cmpss',
  'cmpxchg',
  'cmpxchg16b',
  'cmpxchg8b',
  'comisd',
  'comiss',
  'cpuid',
  'crc32',
  'cvtdq2pd',
  'cvtdq2ps',
  'cvtpd2dq',
  'cvtpd2pi',
  'cvtpd2ps',
  'cvtpi2pd',
  'cvtpi2ps',
  'cvtps2dq',
  'cvtps2pd',
  'cvtps2pi',
  'cvtsd2si',
  'cvtsd2ss',
  'cvtsi2sd',
  'cvtsi2ss',
  'cvtss2sd',
  'cvtss2si',
  'cvttpd2dq',
  'cvttpd2pi',
  'cvttps2dq',
  'cvttps2pi',
  'cvttsd2si',
  'cvttss2si',
  'cwde',
  'daa',
  'das',
  'dec',
  'div',
  'divpd',
  'divps',
  'divsd',
  'divss',
  'dppd',
  'dpps',
  'emms',
  'enter',
  'extractps',
  'f2xm1',
  'fabs',
  'fadd',
  'faddp',
  'fbld',
  'fbstp',
  'fchs',
  'fclex',
  'fcmovb',
  'fcmovbe',
  'fcmove',
  'fcmovnb',
  'fcmovnbe',
  'fcmovne',
  'fcmovnu',
  'fcmovu',
  'fcom',
  'fcomi',
  'fcomip',
  'fcomp',
  'fcompp',
  'fcos',
  'fdecstp',
  'fdiv',
  'fdivp',
  'fdivr',
  'fdivrp',
  'femms',
  'ffree',
  'fiadd',
  'ficom',
  'ficomp',
  'fidiv',
  'fidivr',
  'fild',
  'fimul',
  'fincstp',
  'finit',
  'fist',
  'fistp',
  'fisttp',
  'fisub',
  'fisubr',
  'fld',
  'fld1',
  'fldcw',
  'fldenv',
  'fldl2e',
  'fldl2t',
  'fldlg2',
  'fldln2',
  'fldpi',
  'fldz',
  'fmul',
  'fmulp',
  'fnclex',
  'fninit',
  'fnop',
  'fnsave',
  'fnstcw',
  'fnstenv',
  'fnstsw',
  'fpatan',
  'fprem',
  'fprem1',
  'fptan',
  'frndint',
  'frstor',
  'fsave',
  'fscale',
  'fsin',
  'fsincos',
  'fsqrt',
  'fst',
  'fstcw',
  'fstenv',
  'fstp',
  'fstsw',
  'fsub',
  'fsubp',
  'fsubr',
  'fsubrp',
  'ftst',
  'fucom',
  'fucomi',
  'fucomip',
  'fucomp',
  'fucompp',
  'fwait',
  'fxam',
  'fxch',
  'fxrstor',
  'fxsave',
  'fxtract',
  'fyl2x',
  'fyl2xp1',
  'haddpd',
  'haddps',
  'hsubpd',
  'hsubps',
  'idiv',
  'imul',
  'inc',
  'int3',
  'ja',
  'jae',
  'jb',
  'jbe',
  'jc',
  'je',
  'jg',
  'jge',
  'jl',
  'jle',
  'jna',
  'jnae',
  'jnb',
  'jnbe',
  'jnc',
  'jne',
  'jng',
  'jnge',
  'jnl',
  'jnle',
  'jno',
  'jnp',
  'jns',
  'jnz',
  'jo',
  'jp',
  'jpe',
  'jpo',
  'js',
  'jz',
  'jmp',
  'ja short',
  'jae short',
  'jb short',
  'jbe short',
  'jc short',
  'je short',
  'jg short',
  'jge short',
  'jl short',
  'jle short',
  'jna short',
  'jnae short',
  'jnb short',
  'jnbe short',
  'jnc short',
  'jne short',
  'jng short',
  'jnge short',
  'jnl short',
  'jnle short',
  'jno short',
  'jnp short',
  'jns short',
  'jnz short',
  'jo short',
  'jp short',
  'jpe short',
  'jpo short',
  'js short',
  'jz short',
  'jmp short',
  'lddqu',
  'ldmxcsr',
  'lea',
  'leave',
  'lfence',
  'DAsmJit_Lock',
  'maskmovdqu',
  'maskmovq',
  'maxpd',
  'maxps',
  'maxsd',
  'maxss',
  'mfence',
  'minpd',
  'minps',
  'minsd',
  'minss',
  'monitor',
  'mov',
  'movapd',
  'movaps',
  'movbe',
  'movd',
  'movddup',
  'movdq2q',
  'movdqa',
  'movdqu',
  'movhlps',
  'movhpd',
  'movhps',
  'movlhps',
  'movlpd',
  'movlps',
  'movmskpd',
  'movmskps',
  'movntdq',
  'movntdqa',
  'movnti',
  'movntpd',
  'movntps',
  'movntq',
  'movq',
  'movq2dq',
  'movsd',
  'movshdup',
  'movsldup',
  'movss',
  'movsx',
  'movsxd',
  'movupd',
  'movups',
  'movzx',
  'mov',
  'mpsadbw',
  'mul',
  'mulpd',
  'mulps',
  'mulsd',
  'mulss',
  'mwait',
  'neg',
  'nop',
  'not',
  'or',
  'orpd',
  'orps',
  'pabsb',
  'pabsd',
  'pabsw',
  'packssdw',
  'packsswb',
  'packusdw',
  'packuswb',
  'paddb',
  'paddd',
  'paddq',
  'paddsb',
  'paddsw',
  'paddusb',
  'paddusw',
  'paddw',
  'palignr',
  'pand',
  'pandn',
  'pause',
  'pavgb',
  'pavgw',
  'pblendvb',
  'pblendw',
  'pcmpeqb',
  'pcmpeqd',
  'pcmpeqq',
  'pcmpeqw',
  'pcmpestri',
  'pcmpestrm',
  'pcmpgtb',
  'pcmpgtd',
  'pcmpgtq',
  'pcmpgtw',
  'pcmpistri',
  'pcmpistrm',
  'pextrb',
  'pextrd',
  'pextrq',
  'pextrw',
  'pf2id',
  'pf2iw',
  'pfacc',
  'pfadd',
  'pfcmpeq',
  'pfcmpge',
  'pfcmpgt',
  'pfmax',
  'pfmin',
  'pfmul',
  'pfnacc',
  'pfpnacc',
  'pfrcp',
  'pfrcpit1',
  'pfrcpit2',
  'pfrsqit1',
  'pfrsqrt',
  'pfsub',
  'pfsubr',
  'phaddd',
  'phaddsw',
  'phaddw',
  'phminposuw',
  'phsubd',
  'phsubsw',
  'phsubw',
  'pi2fd',
  'pi2fw',
  'pinsrb',
  'pinsrd',
  'pinsrq',
  'pinsrw',
  'pmaddubsw',
  'pmaddwd',
  'pmaxsb',
  'pmaxsd',
  'pmaxsw',
  'pmaxub',
  'pmaxud',
  'pmaxuw',
  'pminsb',
  'pminsd',
  'pminsw',
  'pminub',
  'pminud',
  'pminuw',
  'pmovmskb',
  'pmovsxbd',
  'pmovsxbq',
  'pmovsxbw',
  'pmovsxdq',
  'pmovsxwd',
  'pmovsxwq',
  'pmovzxbd',
  'pmovzxbq',
  'pmovzxbw',
  'pmovzxdq',
  'pmovzxwd',
  'pmovzxwq',
  'pmuldq',
  'pmulhrsw',
  'pmulhuw',
  'pmulhw',
  'pmulld',
  'pmullw',
  'pmuludq',
  'pop',
  'popad',
  'popcnt',
  'popfd',
  'popfq',
  'por',
  'prefetch',
  'psadbw',
  'pshufb',
  'pshufd',
  'pshufw',
  'pshufhw',
  'pshuflw',
  'psignb',
  'psignd',
  'psignw',
  'pslld',
  'pslldq',
  'psllq',
  'psllw',
  'psrad',
  'psraw',
  'psrld',
  'psrldq',
  'psrlq',
  'psrlw',
  'psubb',
  'psubd',
  'psubq',
  'psubsb',
  'psubsw',
  'psubusb',
  'psubusw',
  'psubw',
  'pswapd',
  'ptest',
  'punpckhbw',
  'punpckhdq',
  'punpckhqdq',
  'punpckhwd',
  'punpcklbw',
  'punpckldq',
  'punpcklqdq',
  'punpcklwd',
  'push',
  'pushad',
  'pushfd',
  'pushfq',
  'pxor',
  'rcl',
  'rcpps',
  'rcpss',
  'rcr',
  'rdtsc',
  'rdtscp',
  'ret',
  'rol',
  'ror',
  'roundpd',
  'roundps',
  'roundsd',
  'roundss',
  'rsqrtps',
  'rsqrtss',
  'sahf',
  'sal',
  'sar',
  'sbb',
  'seta',
  'setae',
  'setb',
  'setbe',
  'setc',
  'sete',
  'setg',
  'setge',
  'setl',
  'setle',
  'setna',
  'setnae',
  'setnb',
  'setnbe',
  'setnc',
  'setne',
  'setng',
  'setnge',
  'setnl',
  'setnle',
  'setno',
  'setnp',
  'setns',
  'setnz',
  'seto',
  'setp',
  'setpe',
  'setpo',
  'sets',
  'setz',
  'sfence',
  'shl',
  'shld',
  'shr',
  'shrd',
  'shufpd',
  'shufps',
  'sqrtpd',
  'sqrtps',
  'sqrtsd',
  'sqrtss',
  'stc',
  'std',
  'stmxcsr',
  'sub',
  'subpd',
  'subps',
  'subsd',
  'subss',
  'test',
  'ucomisd',
  'ucomiss',
  'ud2',
  'unpckhpd',
  'unpckhps',
  'unpcklpd',
  'unpcklps',
  'xadd',
  'xchg',
  'xor',
  'xorpd',
  'xorps');

 operandSize: array[0..16] of string = (
  '',
  'byte ptr ',
  'word ptr ',
  '',
  'dword ptr ',
  '',
  '',
  '',
  'qword ptr ',
  '',
  'tword ptr ',
  '',
  '',
  '',
  '',
  '',
  'dqword ptr ');

 segmentName: array[0..6] of string = (
  '   ',
  'cs:',
  'ss:',
  'ds:',
  'es:',
  'fs:',
  'gs:');

function myutoa(i: SysUInt; base: SysUInt = 10): string;
const
  Letters = '0123456789ABCDEF';
var
  b: SysInt;
begin
  Result := '';
  repeat
    b := i mod Base;
    Result := letters[Int8(b + 1)] + Result;
    i := i div Base;
  until (i <= 0);
end;

function myitoa(i: SysInt; Base: SysUInt = 10): string;
begin
  Result := '';
  if (i < 0) then
  begin
    Result := Result + '-';
    i := -i;
  end;

  Result := Result + myutoa(SysUInt(i), Base);
end;

constructor TLogger.Create;
begin
  inherited;

  FEnabled := True;
  FHaveStream := True;
end;

procedure TLogger.logInstruction(Code: UInt32; o1, o2, o3: POperand; inlineComment: string);
var
  buf: string;
  currentLength, commentLength, alignBy: SysUInt;
begin
  if ((not FEnabled) or (not FHaveStream)) then Exit;

  Buf := dumpInstruction(Code);

  if (o1 <> nil) and (not o1.isNone) then begin Buf := Buf + ' '; Buf := Buf + dumpOperand(o1); end;
  if (o2 <> nil) and (not o2.isNone) then begin Buf := Buf + ', ';Buf := Buf + dumpOperand(o2); end;
  if (o3 <> nil) and (not o3.isNone) then begin Buf := Buf + ', ';Buf := Buf + dumpOperand(o3); end;

  if (inlineComment <> '') then
  begin
    currentLength := Length(buf);
    commentLength := Length(inlineComment);
    if (commentLength > 255) then
      commentLength := 255;
    alignBy := 40;

    if (currentLength >= alignBy) then
      alignBy := 0
    else
      alignBy := alignBy - currentLength;

    Buf := Buf + StringOfChar(' ', alignBy) + '; ' + Copy(inlineComment, 1, commentLength);
  end;

  Buf := Buf + LineEnding;
  log(buf);
end;

procedure TLogger.logAlign(m: SysInt);
begin
  if ((not FEnabled) or (not FHaveStream)) then Exit;

  logFormat('.align %d' + LineEnding, [Int32(m)]);
end;

procedure TLogger.logLabel(Lbl: TLabel);
begin
  if ((not FEnabled) or (not FHaveStream)) then Exit;

  log(dumpLabel(Lbl) + ':' + LineEnding);
end;

procedure TLogger.logFormat(fmt: string; Args: array of const);
begin
  if ((not FEnabled) or (not FHaveStream)) then Exit;
  log(Format(fmt, Args));
end;

procedure TLogger.log(buf: string);
begin
end;

class function TLogger.dumpInstruction(Code: UInt32): string;
begin
  Assert(Code < _INST_COUNT);
  Result := instructionName[code - 1];
end;

class function TLogger.dumpOperand(op: POperand): string;
const
  onetwofoureight = '1248';
var
  reg: TBaseReg;
  isAbsolute: Boolean;
  mem: TMem;
begin
  Result := '';
  if (op.isReg) then
  begin
    reg := TBaseReg(op^);
    Result := dumpRegister(reg.typ, reg.index);
    Exit;
  end
  else if (op.isMem) then
  begin
    isAbsolute := False;
    mem := TMem(op^);

    if (op.size <= 16) then
      Result := operandSize[op.size];

    Result := Result + segmentName[mem.segmentPrefix] + '[';

    if (mem.hasBase) then
      Result := Result + dumpRegister(REG_GPN, mem.base)
    else if (mem.hasLabel) then
      Result := Result + dumpLabel(mem.u._mem.Lbl^)
    else
    begin
      isAbsolute := True;
      Result := Result + myutoa(SysUInt(mem.u._mem.target), 16);
    end;

    if (mem.hasIndex) then
    begin
      Result := Result + ' + ' + dumpRegister(REG_GPN, mem.index);

      if (mem.shift > 0) then
        Result := Result + ' * ' + onetwofoureight[(mem.shift and 3) + 1];
    end;

    if ((mem.displacement > 0) and (not isAbsolute)) then
    begin
      if (mem.displacement < 0) then
        Result := Result + '-'
      else
        Result := Result + '+';
      Result := Result + myitoa(mem.displacement);
    end;

    Result := Result + ']';
    Exit;
  end
  else if (op.isImm) then
    Result := myitoa(SysInt(TImmediate(op^).value))
  else if (op.isLabel) then
    Result := dumpLabel(TLabel(op^))
  else
    Result := 'none';
end;

class function TLogger.dumpRegister(typ, index: UInt8): string;
const
  regs1: array[0..7] of string = ('al', 'cl', 'dl', 'bl', 'ah', 'ch', 'dh', 'bh');
  regs2: array[0..7] of string = ('ax', 'cx', 'dx', 'bx', 'sp', 'bp', 'si', 'di');
begin
  Result := '';
  case typ of
    REG_GPB:
      if (index < 8) then
        Result := regs1[index]
      else
        Result := Format('r%ub', [UInt32(index)]);
    REG_GPW:
      if (index < 8) then
        Result := regs2[index]
      else
        Result := Format('r%uw', [UInt32(index)]);
    REG_GPD:
      if (index < 8) then
        Result := Format('e%s', [regs2[index]])
      else
        Result := Format('r%ud', [UInt32(index)]);
    REG_GPQ:
      if (index < 8) then
        Result := Format('r%s', [regs2[index]])
      else
        Result := Format('r%u', [UInt32(index)]);
    REG_X87:
      Result := Format('st%u', [UInt32(index)]);
    REG_MM:
      Result := Format('mmu%u', [UInt32(index)]);
    REG_XMM:
      Result := Format('xmm%u', [UInt32(index)]);
  end;
end;

class function TLogger.dumpLabel(Lbl: TLabel): string;
begin
  Result := 'L';

  if (Lbl.labelId > 0) then
    Result := Result + myutoa(Lbl.labelId)
  else
    Result := Result + 'x';
end;

constructor TStreamLogger.Create(Stream: TStream = nil);
begin
  inherited Create;

  SetStream(Stream);
end;

procedure TStreamLogger.log(buf: string);
begin
  if ((not FEnabled) or (not FHaveStream)) then Exit;

  FStream.Write(Buf, Length(Buf));
end;

procedure TStreamLogger.SetStream(Stream: TStream);
begin
  FStream := Stream;
  FHaveStream := (Stream <> nil);
end;

constructor TCallBackLogger.Create(CallBack: TLoggerCallback = nil);
begin
  inherited Create;

  cb := CallBack;
end;

procedure TCallBackLogger.log(buf: string);
begin
  if ((not FEnabled) or (@cb = nil)) then
    Exit;

  cb(buf);
end;

end.
