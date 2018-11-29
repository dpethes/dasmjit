unit DAsmJit_MemoryManager;

{$I DAsmJit.inc}

interface

uses
  DAsmJit, DAsmJit_Lock, DAsmJit_VirtualMemory;

const
  //MEMORY_ALLOC_TYPE_E = (
    MEMORY_ALLOC_FREEABLE = 0;
    MEMORY_ALLOC_PERNAMENT = 1;
  //);

  //TNODE_COLOR_E = (
    NODE_BLACK = 0;
    NODE_RED = 1;
  //);

  BITS_PER_ENTITY = SizeOf(SysUInt) * 8;

type
  TMemoryManager = class
  public
    function Alloc(Size: SysUInt; Typ: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer; virtual; abstract;
    function FreeAddress(Address: Pointer): Boolean; virtual; abstract;
    function Used: SysUInt; virtual; abstract;
    function Allocated: SysUInt; virtual; abstract;
    class function Global: TMemoryManager;
  end;

  TDefaultMemoryManager = class(TMemoryManager)
  protected
    Fd: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    function Alloc(Size: SysUInt; Typ: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer; override;
    function FreeAddress(Address: Pointer): Boolean; override;
    function Used: SysUInt; override;
    function Allocated: SysUInt; override;
  end;

  PM_Node = ^TM_Node;
  TM_Node = record
    prev: PM_Node;
    next: PM_Node;
    nlLeft: PM_Node;
    nlRight: PM_Node;
    nlColor: UInt32;
    mem: PUInt8;
    size: SysUInt;
    blocks: SysUInt;
    density: SysUInt;
    used: SysUInt;
    largestBlock: SysUInt;
    baUsed: PSysUInt;
    baCont: PSysUInt;
  end;

  PM_PernamentNode = ^TM_PernamentNode;
  TM_PernamentNode = record
    mem: PUInt8;
    size: SysUInt;
    used: SysUInt;
    prev: PM_PernamentNode;
  end;

function M_Node_Remain(r: TM_Node): SysUInt;
function M_PernamentNode_Available(r: TM_PernamentNode): SysUInt;

implementation

type
  TMemoryManagerPrivate = class
  protected
    FLock: TLock;
    FNewChunkSize: SysUInt;
    FNewChunkDensity: SysUInt;
    FAllocated: SysUInt;
    FUsed: SysUInt;
    FFirst: PM_Node;
    FLast: PM_Node;
    FOptimal: PM_Node;
    FRoot: PM_Node;
    FPernament: PM_PernamentNode;
  public
    constructor Create;

    class function CreateNode(Size, Density: SysUInt): PM_Node;
    function AllocPernament(vSize: SysUInt): Pointer;
    function AllocFreeable(vSize: SysUInt): Pointer;
    function FreeAddress(Address: Pointer): Boolean;
    class function nlIsRed(n: PM_Node): Boolean;
    class function nlRotateLeft(n: PM_Node): PM_Node;
    class function nlRotateRight(n: PM_Node): PM_Node;
    class procedure nlFlipColor(n: PM_Node);
    class function nlMoveRedLeft(h: PM_Node): PM_Node;
    class function nlMoveRedRight(h: PM_Node): PM_Node;
    class function nlFixUp(h: PM_Node): PM_Node;
    procedure nlInsertNode(n: PM_Node);
    function nlInsertNode_(h, n: PM_Node): PM_Node;
    procedure nlRemoveNode(n: PM_Node);
    function nlRemoveNode_(h, n: PM_Node): PM_Node;
    function nlRemoveMin(h: PM_Node): PM_Node;
    function nlFindPtr(Mem: PUInt8): PM_Node;
  end;

procedure _SetBit(buf: PSysUInt; Index: SysUInt);
var
  i, j: SysUInt;
begin
  i := index div (SizeOf(SysUInt) * 8);
  j := index mod (SizeOf(SysUInt) * 8);

  Inc(Buf, i);
  Buf^ := Buf^ or SysUInt(1) shl j;
end;

procedure _ClearBit(buf: PSysUInt; index: SysUInt);
var
  i, j: SysUInt;
begin
  i := index div (SizeOf(SysUInt) * 8);
  j := index mod (SizeOf(SysUInt) * 8);

  Inc(Buf, i);
  Buf^ := Buf^ and (not (SysUInt(1) shl j));
end;

procedure _SetBits(buf: PSysUInt; index, len: SysUInt);
var
  i, j, c: SysUInt;
begin
  if (len = 0) then Exit;

  i := index div (SizeOf(SysUInt) * 8);
  j := index mod (SizeOf(SysUInt) * 8);
  c := (SizeOf(SysUInt) * 8) - j;

  Inc(Buf, i);

  if (c > len) then
  begin
    buf^ := buf^ or ((SysUInt(-1)) shr ((SizeOf(SysUInt) * 8) - len)) shl j;
    Exit;
  end
  else
  begin
    buf^ := buf^ or ((SysUInt(-1)) shr ((SizeOf(SysUInt) * 8) - c)) shl j;
    Inc(Buf);
    Dec(len, c);
  end;

  while (len >= (SizeOf(SysUInt) * 8)) do
  begin
    buf^ := SysUInt(-1);
    Inc(buf);
    Dec(len, SizeOf(SysUInt) * 8);
  end;

  if (len > 0) then
    buf^ := buf^ or ((SysUInt(-1)) shr ((SizeOf(SysUInt) * 8) - len));
end;

procedure _ClearBits(buf: PSysUInt; index, len: SysUInt);
var
  i, j, c: SysUInt;
begin
  if (len = 0) then Exit;

  i := index div (SizeOf(SysUInt) * 8);
  j := index mod (SizeOf(SysUInt) * 8);
  c := (SizeOf(SysUInt) * 8) - j;

  Inc(Buf, i);

  if (c > len) then
  begin
    buf^ := buf^ and (not (((SysUInt(-1)) shr ((SizeOf(SysUInt) * 8) - len)) shl j));
    Exit;
  end
  else
  begin
    buf^ := buf^ and (not (((SysUInt(-1)) shr ((SizeOf(SysUInt) * 8) - c)) shl j));
    Inc(Buf);
    Dec(len, c);
  end;

  while (len >= (SizeOf(SysUInt) * 8)) do
  begin
    buf^ := SysUInt(-1);
    Inc(buf);
    Dec(len, SizeOf(SysUInt) * 8);
  end;

  if (len > 0) then
    buf^ := buf^ and ((SysUInt(-1)) shr len);
end;

function M_Node_Remain(r: TM_Node): SysUInt;
begin
  Result := r.Size - r.Used;
end;

function M_PernamentNode_Available(r: TM_PernamentNode): SysUInt;
begin
  Result := r.Size - r.Used;
end;

constructor TMemoryManagerPrivate.Create;
begin
  inherited;

  FNewChunkSize := 65536;
  FNewChunkDensity := 64;
  FAllocated := 0;
  FUsed := 0;
  FRoot := nil;
  FFirst := nil;
  FOptimal := nil;
  FPernament := nil;
end;

class function TMemoryManagerPrivate.createNode(Size, Density: SysUInt): PM_Node;
var
  vSize, Blocks, baSize, memSize: SysUInt;
  vMem: PUInt8;
  Node: PM_Node;
begin
  vMem := PUInt8(TVirtualMemory.alloc(Size, @vSize, True));

  if (vmem = nil) then
  begin
    Result := nil;
    Exit;
  end;

  blocks := (vSize div Density);
  basize := (((blocks + 7) shr 3) + SizeOf(SysUInt) - 1) and (not SysUInt(SizeOf(SysUInt)-1));
  memSize := SizeOf(TM_Node) + (baSize * 2);

  GetMem(Node, memSize);

  if (Node = nil) then
  begin
    TVirtualMemory.FreeAddress(vMem, vSize);
    Result := nil;
    Exit;
  end;

  FillChar(Node^, memSize, 0);

  Node.nlColor := NODE_RED;
  Node.mem := vMem;

  Node.Size := vSize;
  Node.Blocks := Blocks;
  Node.Density := Density;
  Node.LargestBlock := vSize;
  Node.baUsed := PSysUInt( PtrUInt(PUInt8(Node)) + SizeOf(TM_Node) );
  Node.baCont := PSysUInt( PtrUInt(PUInt8(Node.baUsed)) + basize );

  Result := Node;
end;

function TMemoryManagerPrivate.allocPernament(vSize: SysUInt): Pointer;
const
  pernamentAlignment: SysUInt = 32;
  pernamentNodeSize: SysUInt = 32768;
var
  over, alignedSize, nodeSize: SysUInt;
  Locked: TAutoLock;
  node: PM_PernamentNode;
  Res: PUInt8;
begin
  over := vsize mod pernamentAlignment;
  if (over > 0) then over := pernamentAlignment - over;
  alignedSize := vsize + over;

  locked := TAutoLock.Create(FLock);
  try
    node := FPernament;

    while ((node <> nil) and (alignedSize > M_PernamentNode_Available(node^))) do node := node.prev;

    if (node = nil) then
    begin
      nodeSize := pernamentNodeSize;
      if (vsize > nodeSize) then nodeSize := vsize;

      //GetMem(Node, SizeOf(TM_PernamentNode));
	    New(node);
      if (node = nil) then
      begin
        Result := nil;
        Exit;
      end;

      node.mem := PUInt8(TVirtualMemory.alloc(nodeSize, @node.size, True));
      if (node.mem = nil) then
      begin
        FreeMem(node);
        Result := nil;
        Exit;
      end;

      node.used := 0;
      node.prev := FPernament;
      FPernament := node;
    end;

    Res := PUInt8(PtrUInt(node.mem) + node.used);
    node.used := node.used + alignedSize;
    FUsed := FUsed + alignedSize;

    Result := Pointer(Res);
  finally
    locked.Free;
  end;
end;

function TMemoryManagerPrivate.allocFreeable(vSize: SysUInt): Pointer;
var
  i, Need, minVSize, uBits, Bit, Blocks, Cont, maxCont, j, Max, chunkSize, u: SysUInt;
  Locked: TAutoLock;
  Node, Next: PM_Node;
  up: PSysUInt;
  Res: PUInt8;
label
  found;
begin
  vSize := (vSize + 31) and (not SysUInt(31));
  if (vsize = 0) then
  begin
    Result := nil;
    Exit;
  end;

  locked := TAutoLock.Create(FLock);
  try
    Node := FOptimal;

    minVSize := FNewChunkSize;

    while (node <> nil) do
    begin
      if ((M_Node_Remain(node^) < vsize) or
         ((node.largestBlock < vsize) and (node.largestBlock <> 0))) then
      begin
        Next := node.next;
        if ((M_Node_Remain(node^) < minVSize) and (node = FOptimal) and (Next <> nil)) then FOptimal := Next;
        Node := Next;
        Continue;
      end;

      up := node.baUsed;
      blocks := node.blocks;
      cont := 0;
      maxCont := 0;

      need := (vsize + node.density - 1) div node.density;
      i := 0;

      while (i < blocks) do
      begin
        ubits := up^;
        Inc(up);

        if (ubits = SysUInt(-1)) then
        begin
          if (cont > maxCont) then maxCont := cont;
          cont := 0;

          Inc(i, BITS_PER_ENTITY);
          Continue;
        end;

        max := BITS_PER_ENTITY;
        if (i + max > blocks) then max := blocks - i;


        bit := 1;
        for j := 1 to max do
        begin
          if ((ubits and bit) = 0) then
          begin
            Inc(cont);
            if (cont = need) then
            begin
              Inc(i, j);
              Dec(i, cont);
              goto found;
            end;
            Continue;
          end;

          if (cont > maxCont) then maxCont := cont;
          cont := 0;
          bit := bit shl 1;
        end;

        Inc(i, BITS_PER_ENTITY);
      end;

      node.largestBlock := maxCont * node.density;
      node := node.next;
    end;

    chunkSize := FNewChunkSize;
    if (chunkSize < vsize) then
      chunkSize := vsize;

    node := createNode(chunkSize, FNewChunkDensity);
    if (node = nil) then
    begin
      Result := nil;
      Exit;
    end;

    node.prev := FLast;

    if (FFirst = nil) then
    begin
      FFirst := node;
      FLast := node;
      FOptimal := node;
    end
    else
    begin
      node.prev := FLast;
      FLast.next := node;
      FLast := node;
    end;

    nlInsertNode(node);

    i := 0;
    need := (vsize + node.density - 1) div node.density;

    FAllocated := FAllocated + node.size;

  found:
    _SetBits(node.baUsed, i, need);
    _SetBits(node.baCont, i, need-1);

    u := need * node.density;
    Inc(node.used, u);
    node.largestBlock := 0;
    Inc(FUsed, u);

    Res := PUInt8(PtrUInt(node.mem) + (i * node.density));
    Assert((PtrUInt(Res) >= PtrUInt(node.mem)) and (PtrUInt(Res) < (PtrUInt(node.mem) + node.size)));
    Result := Res;
  finally
    locked.Free;
  end;
end;

function TMemoryManagerPrivate.FreeAddress(Address: Pointer): Boolean;
var
  locked: TAutoLock;
  node, cur, next, prev: PM_Node;
  offset, bitPos, i, j, uBits, cBits, bit, cont: SysUInt;
  up, cp: PSysUInt;
  Stop: Boolean;
begin
  if (address = nil) then
  begin
    Result := True;
    Exit;
  end;

  locked := TAutoLock.Create(FLock);
  try
    node := nlFindPtr(PUInt8(Address));
    if (node = nil) then
    begin
      Result := False;
      Exit;
    end;

    offset := SysUInt(PtrUInt(address) - PtrUInt(node.mem));
    bitpos := offset div node.density;
    i := (bitpos div BITS_PER_ENTITY);
    j := (bitpos mod BITS_PER_ENTITY);

    up := PSysUInt(PtrUInt(node.baUsed) + i);
    cp := PSysUInt(PtrUInt(node.baCont) + i);
    ubits := up^;
    cbits := cp^;
    bit := SysUInt(1) shl j;
    cont := 0;

    while True do
    begin
      Stop := (cbits and bit) = 0;
      ubits := ubits and not bit;
      cbits := cbits and not bit;

      Inc(j);
      bit := bit shl 1;
      Inc(cont);

      if (Stop or (j = BITS_PER_ENTITY)) then
      begin
        up^ := ubits;
        cp^ := cbits;

        if (Stop) then
          Break;

        Inc(up);
        Inc(cp);
        ubits := up^;
        cbits := cp^;

        j := 0;
        bit := 1;
      end;
    end;

    if (node.used = node.size) then
    begin
      cur := FOptimal;

      repeat
        cur := cur.prev;
        if (cur = node) then
        begin
          FOptimal := node;
          Break;
        end;
      until (cur = nil);
    end;

    cont := cont * node.density;
    if (node.largestBlock < cont) then
      node.largestBlock := cont;
    Dec(node.used, cont);
    Dec(FUsed, cont);

    if (node.used = 0) then
    begin
      Dec(FAllocated, node.size);
      nlRemoveNode(node);
      TVirtualMemory.FreeAddress(node.mem, node.size);

      next := node.next;
      prev := node.prev;

      if (prev <> nil) then
        prev.next := next
      else
        FFirst := next;
      if (next <> nil) then
        next.prev := prev
      else
        FLast := prev;
      if (FOptimal = node) then
        if (prev <> nil) then
          FOptimal := prev
        else
          FOptimal := next;

      FreeMem(node);
    end;

    Result := True;
  finally
    locked.Free;
  end;
end;

class function TMemoryManagerPrivate.nlIsRed(n: PM_Node): Boolean;
begin
  Result := (n <> nil) and (n.nlColor = NODE_RED);
end;

class function TMemoryManagerPrivate.nlRotateLeft(n: PM_Node): PM_Node;
var
  x: PM_Node;
begin
  x := n.nlRight;
  n.nlRight := x.nlLeft;
  x.nlLeft := n;
  x.nlColor := x.nlLeft.nlColor;
  x.nlLeft.nlColor := NODE_RED;
  Result := x;
end;

class function TMemoryManagerPrivate.nlRotateRight(n: PM_Node): PM_Node;
var
  x: PM_Node;
begin
  x := n.nlLeft;
  n.nlLeft := x.nlRight;
  x.nlRight := n;
  x.nlColor := x.nlRight.nlColor;
  x.nlRight.nlColor := NODE_RED;
  Result := x;
end;

class procedure TMemoryManagerPrivate.nlFlipColor(n: PM_Node);
begin
  n.nlColor := not n.nlColor;
  n.nlLeft.nlColor := not n.nlLeft.nlColor;
  n.nlRight.nlColor := not n.nlRight.nlColor;
end;

class function TMemoryManagerPrivate.nlMoveRedLeft(h: PM_Node): PM_Node;
begin
  nlFlipColor(h);
  if (nlIsRed(h.nlRight.nlLeft)) then
  begin
    h.nlRight := nlRotateRight(h.nlRight);
    h := nlRotateLeft(h);
    nlFlipColor(h);
  end;
  Result := h;
end;

class function TMemoryManagerPrivate.nlMoveRedRight(h: PM_Node): PM_Node;
begin
  nlFlipColor(h);
  if (nlIsRed(h.nlLeft.nlLeft)) then
  begin
    h := nlRotateRight(h);
    nlFlipColor(h);
  end;
  Result := h;
end;

class function TMemoryManagerPrivate.nlFixUp(h: PM_Node): PM_Node;
begin
  if (nlIsRed(h.nlRight)) then
    h := nlRotateLeft(h);
  if (nlIsRed(h.nlLeft) and nlIsRed(h.nlLeft.nlLeft)) then
    h := nlRotateRight(h);
  if (nlIsRed(h.nlLeft) and nlIsRed(h.nlRight)) then
    nlFlipColor(h);

  Result := h;
end;

procedure TMemoryManagerPrivate.nlInsertNode(n: PM_Node);
begin
  FRoot := nlInsertNode_(FRoot, n);
end;

function TMemoryManagerPrivate.nlInsertNode_(h, n: PM_Node): PM_Node;
begin
  if (h = nil) then
  begin
    Result := n;
    Exit;
  end;

  if (nlIsRed(h.nlLeft) and nlIsRed(h.nlRight)) then nlFlipColor(h);

  if (PtrUInt(n.mem) < PtrUInt(h.mem)) then
    h.nlLeft := nlInsertNode_(h.nlLeft, n)
  else
    h.nlRight := nlInsertNode_(h.nlRight, n);

  if (nlIsRed(h.nlRight) and not nlIsRed(h.nlLeft)) then h := nlRotateLeft(h);
  if (nlIsRed(h.nlLeft) and nlIsRed(h.nlLeft.nlLeft)) then h := nlRotateRight(h);

  Result := h;
end;

procedure TMemoryManagerPrivate.nlRemoveNode(n: PM_Node);
begin
  FRoot := nlRemoveNode_(FRoot, n);
  if (FRoot <> nil) then FRoot.nlColor := NODE_BLACK;

  Assert(nlFindPtr(n.mem) = nil);
end;

function findParent(root, n: PM_Node): PM_Node;
var
  Parent, Cur: PM_Node;
  Mem, curMem, curEnd: PUInt8;
begin
  parent := nil;
  cur := root;
  mem := n.mem;

  while (cur <> nil) do
  begin
    curMem := cur.mem;
    if (PtrUInt(mem) < PtrUInt(curMem)) then
    begin
      parent := cur;
      cur := cur.nlLeft;
      Continue;
    end
    else
    begin
      curEnd := PUInt8(PtrUInt(curMem) + cur.size);
      if (PtrUInt(mem) >= PtrUInt(curEnd)) then
      begin
        parent := cur;
        cur := cur.nlRight;
        Continue;
      end;
      Result := Parent;
      Exit;
    end;
  end;

  Result := nil;
end;

function TMemoryManagerPrivate.nlRemoveNode_(h, n: PM_Node): PM_Node;
var
  _l, _r: PM_Node;
begin
  if (PtrUInt(n.mem) < PtrUInt(h.mem)) then
  begin
    if ((not nlIsRed(h.nlLeft)) and (not nlIsRed(h.nlLeft.nlLeft))) then
      h := nlMoveRedLeft(h);
    h.nlLeft := nlRemoveNode_(h.nlLeft, n);
  end
  else
  begin
    if (nlIsRed(h.nlLeft)) then
      h := nlRotateRight(h);
    if ((h = n) and (h.nlRight = nil)) then
    begin
      Result := nil;
      Exit;
    end;
    if ((not nlIsRed(h.nlRight)) and (not nlIsRed(h.nlRight.nlLeft))) then
      h := nlMoveRedRight(h);
    if (h = n) then
    begin
      h := n.nlRight;
      while (h.nlLeft <> nil) do h := h.nlLeft;

      _l := n.nlLeft;
      _r := nlRemoveMin(n.nlRight);

      h.nlLeft := _l;
      h.nlRight := _r;
      h.nlColor := n.nlColor;
    end
    else
      h.nlRight := nlRemoveNode_(h.nlRight, n);
  end;

  Result := nlFixUp(h);
end;

function TMemoryManagerPrivate.nlRemoveMin(h: PM_Node): PM_Node;
begin
  if (h.nlLeft = nil) then
  begin
    Result := nil;
    Exit;
  end;
  if ((not nlIsRed(h.nlLeft)) and (not nlIsRed(h.nlLeft.nlLeft))) then
    h := nlMoveRedLeft(h);
  h.nlLeft := nlRemoveMin(h.nlLeft);
  Result := nlFixUp(h);
end;

function TMemoryManagerPrivate.nlFindPtr(mem: PUInt8): PM_Node;
var
  curMem, curEnd: PUInt8;
  cur: PM_Node;
begin
  cur := FRoot;

  while (cur <> nil) do
  begin
    curMem := cur.mem;
    if (PtrUInt(mem) < PtrUInt(curMem)) then
    begin
      cur := cur.nlLeft;
      Continue;
    end
    else
    begin
      curEnd := PUInt8(PtrUInt(curMem) + cur.size);
      if (PtrUInt(mem) >= PtrUInt(curEnd)) then
      begin
        cur := cur.nlRight;
        Continue;
      end;
      Result := cur;
      Exit;
    end;
  end;

  Result := nil;
end;

{$J+}
class function TMemoryManager.Global: TMemoryManager;
const
  memmgr: Pointer = nil;
begin
  if (memmgr = nil) then
    memmgr := TDefaultMemoryManager.Create;
  Result := memmgr;
end;

constructor TDefaultMemoryManager.Create;
begin
  inherited;

  Fd := TMemoryManagerPrivate.Create;
end;

destructor TDefaultMemoryManager.Destroy;
begin
  TMemoryManagerPrivate(Fd).Free;
  inherited;
end;

function TDefaultMemoryManager.Alloc(Size: SysUInt; Typ: UInt32 = MEMORY_ALLOC_FREEABLE): Pointer;
begin
  if (Typ = MEMORY_ALLOC_PERNAMENT) then
    Result := TMemoryManagerPrivate(Fd).AllocPernament(Size)
  else
    Result := TMemoryManagerPrivate(Fd).AllocFreeable(Size);
end;

function TDefaultMemoryManager.FreeAddress(Address: Pointer): Boolean;
begin
  Result := TMemoryManagerPrivate(Fd).FreeAddress(Address);
end;

function TDefaultMemoryManager.Used: SysUInt;
begin
  Result := TMemoryManagerPrivate(Fd).FUsed;
end;

function TDefaultMemoryManager.Allocated: SysUInt;
begin
  Result := TMemoryManagerPrivate(Fd).FAllocated;
end;

initialization
finalization
  TMemoryManager.Global.Free;

end.
