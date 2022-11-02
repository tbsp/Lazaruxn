unit uxnFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, uxn;

const
  POLYFILEY = 2;
  DEV_FILE0 = $a;

type
  FileState = (sIDLE, sFILE_READ, sFILE_WRITE, sDIR_READ);
  TUxnFile = record
    //datFile: File of Byte;
    stream: TFilestream;
    dir: string;
    current_filename: string;
    state: FileState;
  end;
  PUxnFile = ^TUxnFile;

type
  TIntegerArray = array of integer;
  TByteArray = array [0..65535] of byte;

var
  uxn_file: array [0..POLYFILEY-1] of TUxnFile;

function file_dei(d: TDevice; port: byte): byte;
function file_deo(d: TDevice; port: byte): byte;
function load_rom(var u: TUxnCPU; filename: string): boolean;

implementation

procedure reset_file(VAR c: Pointer);
var
  fp: ^TUxnFile absolute c;
begin
  //CloseFile(c.datFile);
  fp^.state := sIDLE;
end;

function get_entry(destPtr: Pointer; len: word; pathname, basestring: string; fail_nonzero: integer): word;
begin
end;

function file_read_dir(VAR c, destPtr: Pointer; len: integer): integer;
var
  fp: ^TUxnFile absolute c;
  pathname: string;
begin
  //if fp^.de = nil then fp^.de := read_dir(fp^.dir);
end;

function file_init(VAR c: Pointer; filename: string; max_len: integer): integer;
var
  fp: ^TUxnFile absolute c;
begin
  reset_file(fp);
  if FileExists(filename) then
    begin
      //AssignFile(fp^.datFile, filename);
      fp^.current_filename := filename;
      Exit(1);
    end
  else
    Exit(0);
end;

function file_read(VAR c, destPtr: Pointer; len: integer): word;
var
  fp: ^TUxnFile absolute c;
  dp: ^TByteArray absolute destPtr;
begin
  if (fp^.state <> sFILE_READ) and (fp^.state <> sDIR_READ) then begin
    //Reset(fp^.datFile);
    //fp^.stream.free;

    // dir

    // file
    try
      fp^.stream := TFileStream.Create(fp^.current_filename, fmOpenRead or fmShareDenyWrite);
    except
      on E:Exception do
        WriteLn(E.Message);
    end;
    fp^.state := sFILE_READ;

  end;
  if len > fp^.stream.Size then len := fp^.stream.Size;
  if fp^.state = sFILE_READ then Exit(fp^.stream.Read(dp^, len));
  Exit(0);
end;

function file_write(VAR c, srcPtr; len: word; flags: byte): word;
begin

end;

function file_stat(VAR c, destPtr: Pointer; len: word): word;
var
  fp: ^TUxnFile absolute c;
  i: integer;
  basename: ^string;
begin
  i := RPos('/', fp^.current_filename);
  if i <> 0 then
    basename := @fp^.current_filename[i+1]
  else
    basename := @fp^.current_filename;

  //Exit(get_entry(dest, len, fp^.current_filename, basename, 0);
end;

function file_delete(VAR c: Pointer): word;
var
  fp: ^TUxnFile absolute c;
begin
  //Exit(DeleteFile(fp^.current_filename));
end;

function file_instance(d: Pointer): PUxnFile;
var
  p: ^TDevice absolute d;
begin
//  Exit(uxn_file[p - @p.u.dev[DEV_FILE0]]);
  Exit(@uxn_file[ShortInt(p)]);
end;

function file_dei(d: TDevice; port: byte): byte;
var
  c: PUxnFile;
  dp: ^TByteArray;
begin
  c := file_instance(@d);
  case port of
  $c..$d: begin dp := @d.dat[port]; DEVPOKE16(d, $2, file_read(c, dp, 1)); end;
  end;
  Exit(d.dat[port]);
end;

function file_deo(d: TDevice; port: byte): byte;
var
  addr, len, res: word;
  c: PUxnFile;
begin
  //file_deo := d.dat[port];

  c := file_instance(@d);
  case port of
  $5: begin
    DEVPEEK16(d, addr, $4);
    DEVPEEK16(d, len, $a);
    if len > $10000 - addr then len := $10000 - addr;
    //res := file_stat(c, d.u.ram[addr], len);
    DEVPOKE16(d, $2, res);
    end;
  $6: begin
    res := file_delete(c);
    DEVPOKE16(d, $2, res);
    end;
  $9: begin
    DEVPEEK16(d, addr, $8);
    //res := file_init(c, @d.u.ram[addr], $10000 - addr);
    DEVPOKE16(d, $2, res);
    end;
  $d: begin
    DEVPEEK16(d, addr, $c);
    DEVPEEK16(d, len, $a);
    if len > $10000 - addr then len := $10000 - addr;
    //res := file_read(c, @d.u.ram[addr], len);
    DEVPOKE16(d, $2, res);
    end;
  $f: begin
    DEVPEEK16(d, addr, $e);
    DEVPEEK16(d, len, $a);
    if len > $10000 - addr then len := $10000 - addr;
    //res := file_wite(c, @d.u.ram[addr], len, d.dat[$7]);
    DEVPOKE16(d, $2, res);
    end;
  end;
end;

{ Boot }

function load_rom(var u: TUxnCPU; filename: string): boolean;
var
  p: PUxnFile;
  res: integer;
  dp: ^TByteArray;
begin
  p := @uxn_file;
  res := file_init(p, filename, Length(filename) + 1);
  dp := @u.ram[$0100];
  if res <> 0 then res := file_read(p, dp, $10000-$0100);
  if res <> 0 then reset_file(p);
  load_rom := res <> 0;
end;

end.

