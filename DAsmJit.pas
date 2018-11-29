unit DAsmJit;

{$I DAsmJit.inc}

interface

type
  //Defined in Delphi
  PInt8 = ^Int8;
  Int8 = ShortInt;

  PUInt8 = ^UInt8;
  UInt8 = Byte;

  PInt16 = ^Int16;
  Int16 = SmallInt;

  PUInt16 = ^UInt16;
  UInt16 = Word;

  PInt32 = ^Int32;
  Int32 = LongInt;

  PUInt32 = ^UInt32;
  UInt32 = LongWord;

  //PInt64 = ^Int64;
  //Int64 = Int64;

  PUInt64 = ^UInt64;
  //UInt64 = UInt64;

{$IFDEF ASMJIT_X86}
  SysInt = Int32;
  SysUInt = UInt32;
{$ELSE}
  SysInt = Int64;
  SysUInt = UInt64;
{$ENDIF}

  PSysInt = ^SysInt;
  PSysUInt = ^SysUInt;

{$IFNDEF FPC}
  PtrInt = Integer;
  PtrUInt = Cardinal;

const
  LineEnding = #13#10;
{$ENDIF}

implementation

end.
