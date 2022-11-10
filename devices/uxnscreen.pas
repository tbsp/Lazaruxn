unit uxnScreen;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, intfgraphics, Graphics, FPImage,
  uxn;

type
  TPixels = array of byte;

type
  TLayer = record
    pixels: TPixels;
    changed: byte;
  end;

  TUxnScreen = record
    img: tLazIntfImage;
    palette: array [0..3] of TFPColor;
    width, height: word;
    fg, bg: TLayer;
    mono: byte;
  end;

const
  FIXED_SIZE: boolean = false;
  blending: array [0..4, 0..15] of byte = (
    (0, 0, 0, 0, 1, 0, 1, 1, 2, 2, 0, 2, 3, 3, 3, 0),
    (0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3),
    (1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 3, 1),
    (2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2, 2, 3, 1, 2),
    (1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0));

var
  uxn_screen: TUxnScreen;
  palette_mono: array [0..1] of TFPColor;

procedure screen_write(layer: TLayer; x, y: word; color: byte);
procedure screen_blit(layer: TLayer; x, y: word; sprite: Pointer; color, flipx, flipy, twobpp: byte);
procedure screen_palette(d: TDevice);
procedure screen_resize(width, height: word);
procedure screen_clear(layer: TLayer);
procedure screen_redraw(s: TUxnScreen; img: tLazIntfImage);
function screen_dei(var d: TDevice; port: byte): byte;
function screen_deo(var d: TDevice; port: byte): byte;

implementation

procedure screen_resize(width, height: word);
begin
  //if (width > 0) and (height > 0) then begin
    // 0-3 value FG/BG for screen device output
    SetLength(uxn_screen.bg.pixels, width * height);
    SetLength(uxn_screen.fg.pixels, width * height);

    // Paletted merge of FG+BG for display
    uxn_screen.img := TLazIntfImage.Create(0, 0);
    uxn_screen.img.DataDescription := GetDescriptionFromDevice(0);
    uxn_screen.img.SetSize(width, height);

    uxn_screen.width := width;
    uxn_screen.height := height;

    screen_clear(uxn_screen.bg);
    screen_clear(uxn_screen.fg);
  //end;

end;

procedure screen_write(layer: TLayer; x, y: word; color: byte);
var
  px: integer;
begin
  if (x < uxn_screen.width) and (y < uxn_screen.height) then begin
    px := x + y * uxn_screen.width;
    if color <> layer.pixels[px] then begin
      layer.pixels[px] := color;
      layer.changed := 1;
    end;
  end;
end;

procedure screen_blit(layer: TLayer; x, y: word; sprite: Pointer; color, flipx, flipy, twobpp: byte);
var
  lx, ly, v, h, opaque: integer;
  c: word;
  ch: byte;
  p: ^TUXNMemoryArray absolute sprite;
begin
  opaque := blending[4][color];

  for v := 0 to 7 do begin
    if twobpp <> 0 then
      c := p^[v] or p^[v + 8] shl 8
    else
      c := p^[v];

    for h := 7 downto 0 do begin
      ch := (c and 1) or ((c shr 7) and 2);
      if (opaque or c) <> 0 then begin
        if flipx <> 0 then lx := x + 7 - h else lx := x + h;
        if flipy <> 0 then ly := y + 7 - v else ly := y + v;
        //lx := x + h;
        //ly := y + v;
        screen_write(layer, lx, ly, blending[ch][color]);
      end;
      c := c shr 1;
    end;

  end;

end;

procedure screen_palette(d: TDevice);
var
  i, r, g, b: byte;
  shift: byte;
begin
  shift := 4;
  for i := 0 to 3 do begin
    r := (d.dat[$8 + (i shr 1)] shr shift) and $f;
    g := (d.dat[$a + (i shr 1)] shr shift) and $f;
    b := (d.dat[$c + (i shr 1)] shr shift) and $f;
    uxn_screen.palette[i] := TColorToFPColor(RGBToColor(r or (r shl 4), g or (g shl 4), b or (b shl 4)));
    shift := shift xor 4;
  end;

end;

procedure screen_clear(layer: TLayer);
var
  i: integer;
begin
  //for i := low(layer.pixels) to high(layer.pixels) do
    //layer.pixels[i] := 0;

  layer.changed := 1;
end;

procedure screen_redraw(s: TUxnScreen; img: tLazIntfImage);
var
  palette: array [0..15] of TFPColor;
  i, x, y, px: integer;
begin
  for i := low(palette) to high(palette) do begin
    if i shr 2 <> 0 then
      palette[i] := uxn_screen.palette[i shr 2]
    else
      palette[i] := uxn_screen.palette[i and 3];
  end;

  if uxn_screen.mono <> 0 then begin
    for x := 0 to uxn_screen.width-1 do
      for y := 0 to uxn_screen.height-1 do begin
        px := x * uxn_screen.width + y;
        if uxn_screen.fg.pixels[px] <> 0 then
          //uxn_screen.img.Colors[x, y] := palette_mono[uxn_screen.fg.pixels[px]]
        else
          //uxn_screen.img.Colors[x, y] := palette_mono[uxn_screen.bg.pixels[px] and $1];
        end;
    end
  else begin
    for x := 0 to uxn_screen.width-1 do
      for y := 0 to uxn_screen.height-1 do begin
        px := x + y * uxn_screen.width;
        uxn_screen.img.Colors[x, y] := palette[uxn_screen.fg.pixels[px] shl 2 or uxn_screen.bg.pixels[px]];
      end;
  end;
end;

function clamp(val, min, max: integer): integer;
begin
  if val >= min then begin
    if val <= max then Exit(val) else Exit(max); end
  else
    Exit(min);
end;

function screen_dei(var d: TDevice; port: byte): byte;
begin
  case port of
  $2: Exit(uxn_screen.width shr 8);
  $3: Exit(uxn_screen.width);
  $4: Exit(uxn_screen.height shr 8);
  $5: Exit(uxn_screen.height);
  else Exit(d.dat[port]);
  end;
end;

function screen_deo(var d: TDevice; port: byte): byte;
var
  a, x, y, dx, dy, addr: word;
  i, n, twobpp: byte;
  layer: TLayer;
  p: ^TUxnCPU;
begin
  case port of
  $3: if not FIXED_SIZE then begin
    DEVPEEK16(d, a, $2);
    screen_resize(clamp(a, 1, 1024), uxn_screen.height);
  end;
  $5: if not FIXED_SIZE then begin
    DEVPEEK16(d, a, $4);
    screen_resize(uxn_screen.width, clamp(a, 1, 1024));
  end;
  $e: begin
    DEVPEEK16(d, x, $8);
    DEVPEEK16(d, y, $a);

    if (d.dat[$f] and $40) <> 0 then
      layer := uxn_screen.fg
    else
      layer := uxn_screen.bg;
    screen_write(layer, x, y, d.dat[$e] and $3);

    if (d.dat[$6] and $1) <> 0 then DEVPOKE16(d, $8, x + 1); { auto x+1 }
    if (d.dat[$6] and $2) <> 0 then DEVPOKE16(d, $a, y + 1); { auto y+1 }
  end;
  $f: begin
    p := d.u;

    twobpp := (d.dat[$f] and $80);
    if (d.dat[$f] and $40) <> 0 then
      layer := uxn_screen.fg
    else
      layer := uxn_screen.bg;

    DEVPEEK16(d, x, $8);
    DEVPEEK16(d, y, $a);
    DEVPEEK16(d, addr, $c);

    n := d.dat[$6] shr 4;
    dx := (d.dat[$6] and $1) shl 3;
    dy := (d.dat[$6] and $2) shl 2;

    if addr > ($10000 - ((n + 1) shl (3 + twobpp))) then Exit(0);
    // extra n-1, and missing VAR on DEO functions, preventing changes from DEVPOKE16 from persisting
    for i := 0 to n do begin
      screen_blit(layer, x + dy * i, y + dx * i, @p^.ram[addr], d.dat[$f] and $f, d.dat[$f] and $10, d.dat[$f] and $20, twobpp);
      addr := addr + (d.dat[$6] and $4) shl (1 + twobpp);
    end;

    DEVPOKE16(d, $c, addr);   { auto addr+length }
    DEVPOKE16(d, $8, x + dx); { auto x+8 }
    DEVPOKE16(d, $a, y + dy); { auto y+8 }
  end;

  end;

end;

initialization
  palette_mono[0] := TColorToFPColor(clBlack);
  palette_mono[1] := TColorToFPColor(clWhite);

end.


