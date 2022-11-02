unit uxnSystem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uxn;

function system_dei(d: TDevice; port: byte): byte;
function system_deo(d: TDevice; port: byte): byte;

implementation

procedure system_print(s: TStack; name: string);
var
  i: integer;
begin
  Write(Format('<%s>', [name]));
  for i := 0 to s.ptr-1 do Write(IntToHex(s.dat[i], 2));
  if s.ptr = 0 then Write(' empty');
  WriteLn('');
end;

procedure system_inspect(u: TUxnCPU);
begin
  system_print(u.wst, 'wst');
  system_print(u.rst, 'rst');
end;

function system_dei(d: TDevice; port: byte): byte;
var
  p: ^TUxnCPU;
begin
  p := d.u;
  case port of
  $2: Exit(p^.wst.ptr);
  $3: Exit(p^.rst.ptr);
  else Exit(d.dat[port]);
  end;
end;

function system_deo(d: TDevice; port: byte): byte;
var
  p: ^TUxnCPU;
begin
  p := d.u;
  case port of
  $2: p^.wst.ptr := d.dat[port];
  $3: p^.rst.ptr := d.dat[port];
  $e: system_inspect(p^);
  //else system_deo_special(d, port);
  end;
end;

end.

