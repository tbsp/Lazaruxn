program Lazaruxncli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  uxn, uxnFile, uxnSystemCLI;

type

  { uLazaruxncli }

  uLazaruxncli = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ uLazaruxncli }

function ErrorMessage(msg, errorStr: String): Integer;
begin
  WriteLn(Format('%s: %s', [msg, errorStr]));
  ErrorMessage := 0;
end;

function console_deo(d: TDevice; port: byte): byte;
begin
  Write(chr(d.dat[port]));
end;

function nil_dei(d: TDevice; port: byte): byte;
begin
  nil_dei := d.dat[port];
end;

function nil_deo(d: TDevice; port: byte): byte;
begin
  WriteLn('nil_deo');
end;

procedure console_input(u: TUxnCPU; c: char);
begin
  u.dev[1].dat[$2] := byte(c);;
  uxn_eval(u, GETVECTOR(u.dev[1]));
end;

procedure run_uxn(u: TUxnCPU);
var
  c: char;
begin
  while u.dev[0].dat[$f] = 0 do begin
    read(c);
    if not EOF then console_input(u, c);
  end;
end;

function start(var u: TUxnCPU): boolean;
begin
  if not uxn_boot(u) then Halt(ErrorMessage('Boot', 'Failed'));
  { system   } uxn_port(u, $0, @system_dei, @system_deo);
  { console  } uxn_port(u, $1, @nil_dei, @console_deo);
  { empty    } uxn_port(u, $2, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $3, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $4, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $5, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $6, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $7, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $8, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $9, @nil_dei, @nil_deo);
  { file0    } uxn_port(u, $a, @file_dei, @file_deo);
  { file1    } uxn_port(u, $b, @file_dei, @file_deo);
  { datetime } uxn_port(u, $c, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $d, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $e, @nil_dei, @nil_deo);
  { empty    } uxn_port(u, $f, @nil_dei, @nil_deo);
  start := true;
end;

procedure uLazaruxncli.DoRun;
var
  i, j: integer;
  ErrorMsg: String;
  u: TUxnCPU;
begin
  if ParamCount < 1 then Halt(ErrorMessage('Usage', 'luxncli game.rom'));

  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not start(u) then Halt(ErrorMessage('Start', 'Failed'));
  if not load_rom(u, ParamStr(1)) then Halt(ErrorMessage('Load', 'Failed'));

  WriteLn(Format('Loaded: %s', [ParamStr(1)]));

  { Run to first BRK }
  if not uxn_eval(u, $0100) then Halt(ErrorMessage('Init', 'Failed'));

  { Pass additional command line args to console input }
  for i := 2 to ParamCount-1 do begin
    for j := 0 to Length(ParamStr(i)) do
      console_input(u, ParamStr(i)[j]);
    console_input(u, chr($a));
  end;

  { Run loop waiting for additional input }
  run_uxn(u);

  { TODO:
    - Change all appropriate integers to word or byte, depending on 16 or 8 bit use.
  }

  // stop program loop
  Terminate;
end;

constructor uLazaruxncli.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor uLazaruxncli.Destroy;
begin
  inherited Destroy;
end;

procedure uLazaruxncli.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: uLazaruxncli;
begin
  Application:=uLazaruxncli.Create(nil);
  Application.Title:='Lazaruxncli';
  Application.Run;
  Application.Free;
end.

