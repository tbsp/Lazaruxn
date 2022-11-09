unit luxnemu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uxn, uxnSystemEmu, uxnFile, uxnScreen;

type

  { TForm1 }

  TForm1 = class(TForm)
    TimerScreen: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TimerScreenTimer(Sender: TObject);
  private

  public

  end;

const
  DEFAULT_WIDTH = 64 * 8;
  DEFAULT_HEIGHT = 40 * 8;
  //PAD = 4;
  //TIMEOUT_MS = 334;
  //BENCH = 0

var
  Form1: TForm1;
  zoom_factor: byte;

  devScreen: PDevice;

procedure uxn_init(u: TUxnCPU);
procedure run_uxn(u: TUxnCPU);

implementation

{$R *.lfm}

function ErrorMessage(msg, errorStr: String): Integer;
begin
  //WriteLn(Format('%s: %s', [msg, errorStr]));
  Application.MessageBox(PChar(errorStr), PChar(msg));
  //ShowMessage(errorStr);
  ErrorMessage := 0;
end;


function handle_events(u: TUxnCPU): boolean;
begin

end;

function console_deo(var d: TDevice; port: byte): byte;
begin
  Write(chr(d.dat[port]));
  Exit(0);
end;

function nil_dei(var d: TDevice; port: byte): byte;
begin
  Exit(d.dat[port]);
end;

function nil_deo(var d: TDevice; port: byte): byte;
begin
  WriteLn(d.dat[port]);
  Exit(0);
end;

procedure redraw();
begin
  //set_size();
  screen_redraw(uxn_screen, uxn_screen.img);
end;

function start(var u: TUxnCPU): boolean;
begin
  if not uxn_boot(u) then Halt(ErrorMessage('Boot', 'Failed'));
  { system   } uxn_port(u, $0, @system_dei, @system_deo);
  { console  } uxn_port(u, $1, @nil_dei, @console_deo);
  { screen   } devScreen := uxn_port(u, $2, @screen_dei, @screen_deo);
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

procedure uxn_init(u: TUxnCPU);
begin
  if ParamCount < 1 then Halt(ErrorMessage('Usage', 'luxnemu game.rom'));

  {
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
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
  }

  if not start(u) then Halt(ErrorMessage('Start', 'Failed'));
  if not load_rom(u, ParamStr(1)) then Halt(ErrorMessage('Load', 'Failed'));

  //WriteLn(Format('Loaded: %s', [ParamStr(1)]));

  { Run to first BRK }
  if not uxn_eval(u, $0100) then Halt(ErrorMessage('Init', 'Failed'));
end;


procedure run_uxn(u: TUxnCPU);
//var
  //c: char;
begin
  while u.dev[0].dat[$f] = 0 do begin
    if not handle_events(u) then Exit();
    uxn_eval(u, GETVECTOR(devScreen^));
    if (uxn_screen.fg.changed <> 0) or (uxn_screen.bg.changed <> 0) then redraw;
  end;
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j: integer;
  ErrorMsg: String;
begin
  zoom_factor := 1;

  //screen_resize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
  //Form1.Width := DEFAULT_WIDTH;
  //Form1.Height := DEFAULT_HEIGHT;

  //set_zoom();

  { Run loop waiting for additional vectors }
  //run_uxn(u);

  TimerScreen.Enabled := true;

end;

procedure TForm1.FormPaint(Sender: TObject);
var
  b: tBitmap;
  x, y: Integer;

begin
  Form1.Width := uxn_screen.width;
  Form1.Height := uxn_screen.height;
  {
  for x := 0 to uxn_screen.width-1 do begin
    for y := 0 to uxn_screen.height-1 do begin
      uxn_screen.img.Colors[x, y] := TColorToFPColor(RGBToColor(x, y, 32));
    end;
  end;
  }

  {
  // Background
  for x := 0 to Width-1 do begin
    for y := 0 to Height-1 do begin
      uxn_screen.img.Colors[x, y] := TColorToFPColor(RGBToColor(x, y, 32));
    end;
  end;
  }
     {
  // Foreground
  for x := 32 to 64 do begin
    for y := 64 to 128 do begin
      uxn_screen.fg.img.Colors[x, y] := TColorToFPColor(RGBToColor(64, x, y));
    end;
  end;    }

  screen_redraw(uxn_screen, uxn_screen.img);

  b := TBitmap.Create;

  // Draw unified image
  b.LoadFromIntfImage(uxn_screen.img);
  Canvas.Draw(0, 0, b);

end;

procedure TForm1.TimerScreenTimer(Sender: TObject);
var
  b: tBitmap;
  x, y: Integer;
begin
  // TODO: Add Screen Vector to pending vector queue
  //TForm1.Invalidate;

  Form1.Width := uxn_screen.width;
  Form1.Height := uxn_screen.height;

  screen_redraw(uxn_screen, uxn_screen.img);

  b := TBitmap.Create;

  // Draw unified image
  b.LoadFromIntfImage(uxn_screen.img);
  Canvas.Draw(0, 0, b);
end;


end.

