unit uxnsystememu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uxn, uxnScreen;

function system_dei(var d: TDevice; port: byte): byte;
function system_deo(var d: TDevice; port: byte): byte;

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

function system_dei(var d: TDevice; port: byte): byte;
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

function system_deo(var d: TDevice; port: byte): byte;
var
  p: ^TUxnCPU;
begin
  p := d.u;
  case port of
  $2: p^.wst.ptr := d.dat[port];
  $3: p^.rst.ptr := d.dat[port];
  $e: system_inspect(p^);
  // TODO: Find a way to use the same uxnSystem for CLI/EMU cases, since
  //  the only difference is this else (system_deo_special). The issue I
  //  have is how to have both depend on the uxnSystem unit, but inject a
  //  distinct special handler.
  else if (port > $7) and (port < $e) then screen_palette(d);
  end;
end;

end.

