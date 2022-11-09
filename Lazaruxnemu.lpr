program Lazaruxnemu;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, luxnemu, uxnScreen, uxn
  { you can add units after this };


{$R *.res}

var
  u: TUxnCPU;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  uxn_init(u);

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

