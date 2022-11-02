unit uxn;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
    uxnErrors: array of String = ('Working-stack underflow',
                                 'Return-stack underflow',
                                 'Working-stack overflow',
                                 'Return-stack overflow',
                                 'Working-stack division by zero',
                                 'Return-stack division by zero',
                                 'Execution timeout');

type
  TStack = record
    ptr: integer;
    dat: array [0..255] of byte;
  end;

  TDevice = record
    u: Pointer;  // TODO: Change to direct reference now that type blocks are unified?
    dat: array [0..16] of byte;
    dei: Pointer;
    deo: Pointer;
  end;

  TFuncDevice = function(d: TDevice; port: byte): byte;

  TUxnCPU = record
    ram: array [0..65535] of byte;
    wst, rst: TStack;
    dev: array [0..15] of TDevice;
  end;

procedure DEVPEEK16(d: TDevice; out o: word; x: word);
procedure DEVPOKE16(d: TDevice; x, y: word);
function GETVECTOR(d: TDevice): integer;

procedure PUSH8(u: TUxnCPU; var s: Pointer; x: integer; var errcode: integer);
procedure PUSH16(u: TUxnCPU; var s: Pointer; x: integer; var errcode: integer);
procedure PUSH(u: TUxnCPU; var s: Pointer; x, bs: integer; var errcode: integer);
procedure POP8(u: TUxnCPU; var s, sp: Pointer; out o: integer; var errcode: integer);
procedure POP16(u: TUxnCPU; var s, sp: Pointer; out o: integer; var errcode: integer);
procedure POP(u: TUxnCPU; var s, sp: Pointer; out o: integer; bs: integer; var errcode: integer);
procedure POKE(var u: TUxnCPU; x, y, bs: integer);
procedure PEEK16(u: TUxnCPU; out o: integer; x: integer);
procedure PEEK(u: TUxnCPU; out o: integer; x, bs: integer);
procedure DEVR(out o: integer; d: TDevice; x, bs: integer);
procedure DEVW8(var d: TDevice; x, y: integer);
procedure DEVW(var d: TDevice; x, y, bs: integer);
procedure WARP(var pc: integer; x, bs: integer);

function uxn_boot(VAR u: TUxnCPU): boolean;
function uxn_eval(VAR u: TUxnCPU; pc: integer): boolean;
function uxn_interrupt(): boolean;
function uxn_halt(VAR u: TUxnCPU; error, addr: integer): boolean;
function uxn_port(VAR u: TUxnCPU; id: integer; deifn, deofn: TFuncDevice): TDevice;

implementation

const
  PAGE_PROGRAM = $0100;

procedure DEVPEEK16(d: TDevice; out o: word; x: word);
begin
  o := d.dat[x] shl 8 + d.dat[x+1];
end;

procedure DEVPOKE16(d: TDevice; x, y: word);
begin
  d.dat[x] := y shr 8;
  d.dat[x+1] := y and $ff;
end;

function GETVECTOR(d: TDevice): integer;
begin
  Exit(d.dat[0] shl 8 or d.dat[1]);
end;

procedure PUSH8(u: TUxnCPU; var s: Pointer; x: integer; var errcode: integer);
var
  p: ^TStack absolute s;
begin
  if p^.ptr = $ff then
    errcode := 2 //goto err;
  else begin
    p^.dat[p^.ptr] := x and $ff;
    inc(p^.ptr);
  end;
end;

procedure PUSH16(u: TUxnCPU; var s: Pointer; x: integer; var errcode: integer);
var
  j: integer;
  p: ^TStack absolute s;
begin
  j := p^.ptr;
  if j >= $fe then
    errcode := 2 //goto err;
  else begin
    p^.dat[j] := x shr 8 and $ff;
    p^.dat[j+1] := x and $ff;
    p^.ptr := j + 2;
  end;
end;

procedure PUSH(u: TUxnCPU; var s: Pointer; x, bs: integer; var errcode: integer);
begin
  if bs <> 0 then PUSH16(u, s, x, errcode) else PUSH8(u, s, x, errcode);
end;

procedure POP8(u: TUxnCPU; var s, sp: Pointer; out o: integer; var errcode: integer);
var
  j: ^byte absolute sp;
  p: ^TStack absolute s;
begin
  // todo: error checking
  dec(j^);
  o := p^.dat[j^];
end;

procedure POP16(u: TUxnCPU; var s, sp: Pointer; out o: integer; var errcode: integer);
var
  j: ^byte absolute sp;
  p: ^TStack absolute s;
begin
  // todo: error checking
  dec(j^);
  o := p^.dat[j^];
  dec(j^);
  o := o + p^.dat[j^] shl 8;
end;

procedure POP(u: TUxnCPU; var s, sp: Pointer; out o: integer; bs: integer; var errcode: integer);
begin
  if bs <> 0 then POP16(u, s, sp, o, errcode) else POP8(u, s, sp, o, errcode);
end;

procedure POKE(var u: TUxnCPU; x, y, bs: integer);
begin
  if bs <> 0 then begin
      u.ram[x] := y shr 8;
      u.ram[x+1] := y and $ff;
    end
  else
    u.ram[x] := y;
end;

procedure PEEK16(u: TUxnCPU; out o: integer; x: integer);
begin
  o := u.ram[x] shl 8 + u.ram[x+1];
end;

procedure PEEK(u: TUxnCPU; out o: integer; x, bs: integer);
begin
  if bs <> 0 then PEEK16(u, o, x) else o := u.ram[x];
end;

procedure DEVR(out o: integer; d: TDevice; x, bs: integer);
begin
  o := TFuncDevice(d.dei)(d, x and $f);
  if bs <> 0 then
    o := (o shl 8) + TFuncDevice(d.dei)(d, (x + 1) and $f);
end;

procedure DEVW8(var d: TDevice; x, y: integer);
begin
  d.dat[x and $f] := y;
  TFuncDevice(d.deo)(d, x and $f);
end;

procedure DEVW(var d: TDevice; x, y, bs: integer);
begin
  if bs <> 0 then begin
    DEVW8(d, x, y shr 8);
    DEVW8(d, x+1, y);
  end
  else
    DEVW8(d, x, y);
end;

procedure WARP(var pc: integer; x, bs: integer);
begin
  if bs <> 0 then pc := x else pc := pc + ShortInt(x);
end;

function uxn_boot(VAR u: TUxnCPU): boolean;
begin
  FillDWord(u.ram,SizeOf(u.ram) shr 2, 0);
  u.wst.ptr := 0; // I can't tell where uxncli initializes the stack pointers
  u.rst.ptr := 0;
end;

function uxn_eval(VAR u: TUxnCPU; pc: integer): boolean;
const
  LIMIT_MAX = $40000;

var
  a, b, c, j, k, bs, instr, errcode, limit: integer;
  kptr: byte;
  sp: pByte;
  src, dst: ^TStack;
  dev: ^TDevice;

  i: integer;

begin
  limit := LIMIT_MAX;
  if pc = 0 or u.dev[0].dat[$f] then Exit(true);
  while true do begin

    // Print stacks
    {
    if u.wst.ptr > 0 then
    begin
      Write('wst: ');
      for i := 0 to u.wst.ptr-1 do begin
        Write(IntToHex(u.wst.dat[i], 2));
        Write(' ');
      end;
      WriteLn('');
    end;
    //Write('wst: ');
    //for i=0 to u.wst.ptr do Write(u.wst[i]);
    }

    instr := u.ram[pc];
    inc(pc);

    if instr = 0 then Exit(true); // Exit on BRK

    { Handle "frozen" process }
    dec(limit);
    if limit = 0 then begin
      if not uxn_interrupt() then begin
        errcode := 6;
        // goto timeout;
      end;
      limit := LIMIT_MAX;
    end;

    { Return Mode }
    if instr and $40 <> 0 then begin
        src := @u.rst;
        dst := @u.wst;
      end
    else begin
        src := @u.wst;
        dst := @u.rst;
    end;

    { Keep Mode }
    if instr and $80 <> 0 then begin
        kptr := src^.ptr;
        sp := @kptr; // point SP to standalone integer, so we pull from the right stack index, but don't affect the actual stack pointer
      end
    else
      sp := @(src^.ptr);

    { Short Mode }
    if instr and $20 <> 0 then bs := 1 else bs := 0;

    { Without C-style defines which unpack into native code, including
      arguments, these are all coded by hand, which is far less compact. }

    case instr and $1f of
    { Stack }
    $00: { LIT } begin PEEK(u, a, pc, bs); PUSH(u, src, a, bs, errcode); pc := pc + 1 + bs; end;
    $01: { INC } begin POP(u, src, sp, a, bs, errcode); PUSH(u, src, a + 1, bs, errcode); end;
    $02: { POP } begin POP(u, src, sp, a, bs, errcode); end;
    $03: { NIP } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); PUSH(u, src, a, bs, errcode); end;
    $04: { SWP } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); PUSH(u, src, a, bs, errcode); PUSH(u, src, b, bs, errcode); end;
    $05: { ROT } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); POP(u, src, sp, c, bs, errcode); PUSH(u, src, b, bs, errcode); PUSH(u, src, a, bs, errcode); PUSH(u, src, c, bs, errcode); end;
    $06: { DUP } begin POP(u, src, sp, a, bs, errcode); PUSH(u, src, a, bs, errcode); PUSH(u, src, a, bs, errcode); end;
    $07: { OVR } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); PUSH(u, src, b, bs, errcode); PUSH(u, src, a, bs, errcode); PUSH(u, src, b, bs, errcode); end;

    { Logic }
    $08: { EQU } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); if b = a then c := 1 else c := 0; PUSH8(u, src, c, errcode); end;
    $09: { NEQ } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); if b <> a then c := 1 else c := 0; PUSH8(u, src, c, errcode); end;
    $0a: { GTH } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); if b > a then c := 1 else c := 0; PUSH8(u, src, c, errcode); end;
    $0b: { LTH } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); if b < a then c := 1 else c := 0; PUSH8(u, src, c, errcode); end;
    $0c: { JMP } begin POP(u, src, sp, a, bs, errcode); WARP(pc, a, bs); end;
    $0d: { JCN } begin POP(u, src, sp, a, bs, errcode); POP8(u, src, sp, b, errcode); if b <> 0 then WARP(pc, a, bs); end;
    $0e: { JSR } begin
      POP(u, src, sp, a, bs, errcode);
      PUSH16(u, dst, pc, errcode);
      WARP(pc, a, bs); end;
    $0f: { STH } begin POP(u, src, sp, a, bs, errcode); PUSH(u, dst, a, bs, errcode); end;

    { Memory }
    $10: { LDZ } begin POP8(u, src, sp, a, errcode); PEEK(u, b, a, bs); PUSH(u, src, b, bs, errcode); end;
    $11: { STZ } begin POP8(u, src, sp, a, errcode); POP(u, src, sp, b, bs, errcode); POKE(u, a, b, bs); end;
    $12: { LDR } begin POP8(u, src, sp, a, errcode); PEEK(u, b, pc + ShortInt(a), bs); PUSH(u, src, b, bs, errcode); end;
    $13: { STR } begin POP8(u, src, sp, a, errcode); POP(u, src, sp, b, bs, errcode); c := pc + ShortInt(a); POKE(u, c, b, bs); end;
    $14: { LDA } begin POP16(u, src, sp, a, errcode); PEEK(u, b, a, bs); PUSH(u, src, b, bs, errcode); end;
    $15: { STA } begin POP16(u, src, sp, a, errcode); POP(u, src, sp, b, bs, errcode); POKE(u, a, b, bs); end;
    $16: { DEI } begin POP8(u, src, sp, a, errcode); DEVR(b, u.dev[a shr 4], a, bs); PUSH(u, src, b, bs, errcode); end;
    $17: { DEO } begin POP8(u, src, sp, a, errcode); POP(u, src, sp, b, bs, errcode); DEVW(u.dev[a shr 4], a, b, bs); end;

    { Arithmetic }
    $18: { ADD } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b + a; PUSH(u, src, c, bs, errcode); end;
    $19: { SUB } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b - a; PUSH(u, src, c, bs, errcode); end;
    $1a: { MUL } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b * a; PUSH(u, src, c, bs, errcode); end;
    $1b: { DIV } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b DIV a; PUSH(u, src, c, bs, errcode); end;
    $1c: { AND } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b and a; PUSH(u, src, c, bs, errcode); end;
    $1d: { ORA } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b or a; PUSH(u, src, c, bs, errcode); end;
    $1e: { EOR } begin POP(u, src, sp, a, bs, errcode); POP(u, src, sp, b, bs, errcode); c := b xor a; PUSH(u, src, c, bs, errcode); end;
    $1f: { SFT } begin POP8(u, src, sp, a, errcode); POP(u, src, sp, b, bs, errcode); c := b shr (a and $f) shl ((a and $f0) shr 4); PUSH(u, src, c, bs, errcode); end;

    end;

  end
end;

function uxn_interrupt(): boolean;
begin

end;

function uxn_halt(VAR u: TUxnCPU; error, addr: integer): boolean;
begin
  WriteLn(Format('Halted: %s#%s, at %s', [uxnErrors[error], IntToHex(u.ram[addr]), IntToHex(addr)]));
  uxn_halt := false;
end;

function uxn_port(VAR u: TUxnCPU; id: integer; deifn, deofn: TFuncDevice): TDevice;
var
  d: ^TDevice;
begin
  d := @u.dev[id];
  d^.u := @u;
  d^.dei := deifn;
  d^.deo := deofn;
  uxn_port := d^;
end;

end.

