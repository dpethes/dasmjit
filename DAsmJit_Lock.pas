unit DAsmJit_Lock;

{$I DAsmJit.inc}

interface

{$IFDEF ASMJIT_WINDOWS}
uses
  Windows;
{$ENDIF}

type
{$IFDEF ASMJIT_WINDOWS}
  TLockHandle = TRTLCriticalSection;
{$ELSE}
  TLockHandle = pthread_mutex_t;
{$ENDIF}

  TLock = class
  protected
    FHandle: TLockHandle;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DAsmJit_Lock;
    procedure UnLock;

    property Handle: TLockHandle read FHandle;
  end;

  TAutoLock = class
  protected
    FTarget: TLock;
    FOwnTarget: Boolean;
  public
    constructor Create(Target: TLock = nil);
    destructor Destroy; override;
  end;

implementation

constructor TLock.Create;
begin
  inherited;

  {$IFDEF ASMJIT_WINDOWS}
  InitializeCriticalSection(FHandle);
  {$ENDIF}
end;

destructor TLock.Destroy;
begin
  {$IFDEF ASMJIT_WINDOWS}
  DeleteCriticalSection(FHandle);
  {$ENDIF}

  inherited;
end;

procedure TLock.DAsmJit_Lock;
begin
  {$IFDEF ASMJIT_WINDOWS}
  EnterCriticalSection(FHandle);
  {$ENDIF}
end;

procedure TLock.UnLock;
begin
  {$IFDEF ASMJIT_WINDOWS}
  LeaveCriticalSection(FHandle);
  {$ENDIF}
end;

constructor TAutoLock.Create(Target: TLock = nil);
begin
  inherited Create;

  if (Target = nil) then
  begin
    FTarget := TLock.Create;
    FOwnTarget := True;
  end
  else
  begin
    FTarget := Target;
    FOwnTarget := False;
  end;

  FTarget.DAsmJit_Lock;
end;

destructor TAutoLock.Destroy;
begin
  FTarget.UnLock;

  if (FOwnTarget) then
    FTarget.Free;

  inherited;
end;

end.
