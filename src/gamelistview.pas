unit gamelistview;

{$mode objfpc}{$H+}

// TGameLibraryView — lista "rica" de jogos, desenhada por nós (owner-draw sobre
// TCustomListBox). Cada linha é um card: checkbox + capa (thumbnail) + nome +
// chips de tag (gênero/ano/dev) + marca "no SD". Só desenha as linhas visíveis,
// então escala para centenas/milhares de jogos sem custo.
//
// Criado em runtime (não precisa registro no Lazarus). Expõe Count, ItemIndex,
// Checked[], Clear, AddGame e o evento OnSelectionChange (herdado).

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, LCLType, Types;

type
  TGameRowData = record
    Name: String;
    Genre: String;
    Year: String;
    Developer: String;
    CoverPath: String;
    Cards: String;       // rótulo(s) dos cartões onde o jogo já foi copiado
    OnSDCard: Boolean;
    Checked: Boolean;
  end;

  { TGameLibraryView }

  TGameLibraryView = class(TCustomListBox)
  private
    FRows: array of TGameRowData;
    FThumbCache: TStringList; // path -> TBitmap (thumbnail escalado), cacheado
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; AValue: Boolean);
    function GetThumb(const APath: String): TBitmap;
    procedure DrawChip(C: TCanvas; var X: Integer; Y: Integer; const S: String;
      Bg, Fg: TColor);
  protected
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearGames;
    procedure ClearThumbCache;
    procedure AddGame(const AName, AGenre, AYear, ADeveloper, ACoverPath: String;
      AOnSDCard: Boolean; const ACards: String = '');
    function CheckedCount: Integer;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemIndex;
    property Count;
    property OnSelectionChange;
    property Align;
    property Visible;
  end;

implementation

const
  ROW_H   = 60;   // altura do card
  PAD     = 6;
  CHK_SZ  = 16;   // checkbox
  THUMB_W = 40;   // capa
  THUMB_H = 48;

constructor TGameLibraryView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  ItemHeight := ROW_H;
  FThumbCache := TStringList.Create;
  FThumbCache.OwnsObjects := False; // liberamos os bitmaps à mão
end;

destructor TGameLibraryView.Destroy;
var i: Integer;
begin
  for i := 0 to FThumbCache.Count - 1 do
    FThumbCache.Objects[i].Free;
  FThumbCache.Free;
  inherited Destroy;
end;

procedure TGameLibraryView.ClearGames;
begin
  SetLength(FRows, 0);
  Items.Clear; // o cache de thumbnails sobrevive (capas não mudam)
end;

// Descarta os thumbnails cacheados (usar após baixar capas novas em lote, para
// que as capas recém-chegadas sejam recarregadas em vez de servir o placeholder).
procedure TGameLibraryView.ClearThumbCache;
var i: Integer;
begin
  for i:=0 to FThumbCache.Count - 1 do
    FThumbCache.Objects[i].Free;
  FThumbCache.Clear;
end;

procedure TGameLibraryView.AddGame(const AName, AGenre, AYear, ADeveloper,
  ACoverPath: String; AOnSDCard: Boolean; const ACards: String = '');
var n: Integer;
begin
  n := Length(FRows);
  SetLength(FRows, n + 1);
  FRows[n].Name := AName;
  FRows[n].Genre := AGenre;
  FRows[n].Year := AYear;
  FRows[n].Developer := ADeveloper;
  FRows[n].CoverPath := ACoverPath;
  FRows[n].Cards := ACards;
  FRows[n].OnSDCard := AOnSDCard;
  FRows[n].Checked := False;
  Items.Add(AName); // mantém Count/seleção do TCustomListBox em sincronia
end;

function TGameLibraryView.GetChecked(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(FRows)) and FRows[Index].Checked;
end;

procedure TGameLibraryView.SetChecked(Index: Integer; AValue: Boolean);
begin
  if (Index >= 0) and (Index < Length(FRows)) then
  begin
    FRows[Index].Checked := AValue;
    Invalidate;
  end;
end;

function TGameLibraryView.CheckedCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(FRows) do
    if FRows[i].Checked then Inc(Result);
end;

// Carrega a capa, escala para um thumbnail e cacheia por caminho. Retorna nil se
// a imagem não carregar (o desenho então mostra um placeholder).
function TGameLibraryView.GetThumb(const APath: String): TBitmap;
var idx: Integer; pic: TPicture; bmp: TBitmap;
begin
  Result := nil;
  if APath = '' then Exit;
  idx := FThumbCache.IndexOf(APath);
  if idx >= 0 then Exit(TBitmap(FThumbCache.Objects[idx]));
  if not FileExists(APath) then Exit;
  pic := TPicture.Create;
  bmp := nil;
  try
    try
      pic.LoadFromFile(APath);
      bmp := TBitmap.Create;
      bmp.SetSize(THUMB_W, THUMB_H);
      bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect(0, 0, THUMB_W, THUMB_H);
      bmp.Canvas.StretchDraw(Rect(0, 0, THUMB_W, THUMB_H), pic.Graphic);
      FThumbCache.AddObject(APath, bmp);
      Result := bmp;
    except
      FreeAndNil(bmp);
      Result := nil;
    end;
  finally
    pic.Free;
  end;
end;

// Desenha um "chip" (retângulo arredondado preenchido) com texto e avança X.
procedure TGameLibraryView.DrawChip(C: TCanvas; var X: Integer; Y: Integer;
  const S: String; Bg, Fg: TColor);
var w, h: Integer;
begin
  if S = '' then Exit;
  h := C.TextHeight('Ag') + 2;
  w := C.TextWidth(S) + 10;
  C.Brush.Color := Bg;
  C.Pen.Color := Bg;
  C.RoundRect(X, Y, X + w, Y + h, 6, 6);
  C.Brush.Style := bsClear;
  C.Font.Color := Fg;
  C.TextOut(X + 5, Y + 1, S);
  C.Brush.Style := bsSolid;
  Inc(X, w + 5);
end;

procedure TGameLibraryView.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  C: TCanvas;
  r: TGameRowData;
  cx, tx, ty: Integer;
  thumb: TBitmap;
  sel: Boolean;
  destR: TRect;
begin
  if (Index < 0) or (Index > High(FRows)) then Exit;
  r := FRows[Index];
  C := Canvas;
  sel := odSelected in State;

  // Fundo (faixa alternada + destaque de seleção)
  if sel then C.Brush.Color := clHighlight
  else if Odd(Index) then C.Brush.Color := RGBToColor(245, 245, 248)
  else C.Brush.Color := clWindow;
  C.FillRect(ARect);

  // Checkbox
  cx := ARect.Left + PAD;
  ty := ARect.Top + (ROW_H - CHK_SZ) div 2;
  C.Brush.Color := clWindow;
  C.Pen.Color := clGray;
  C.Rectangle(cx, ty, cx + CHK_SZ, ty + CHK_SZ);
  if r.Checked then
  begin
    C.Pen.Color := RGBToColor(0, 140, 0);
    C.Pen.Width := 2;
    C.Line(cx + 3, ty + 8, cx + 7, ty + 12);
    C.Line(cx + 7, ty + 12, cx + 13, ty + 4);
    C.Pen.Width := 1;
  end;

  // Capa (thumbnail) ou placeholder
  destR := Rect(ARect.Left + PAD + CHK_SZ + PAD, ARect.Top + (ROW_H - THUMB_H) div 2,
    ARect.Left + PAD + CHK_SZ + PAD + THUMB_W, ARect.Top + (ROW_H - THUMB_H) div 2 + THUMB_H);
  thumb := GetThumb(r.CoverPath);
  if thumb <> nil then
    C.StretchDraw(destR, thumb)
  else
  begin
    C.Brush.Color := RGBToColor(220, 220, 224);
    C.FillRect(destR);
  end;

  tx := destR.Right + PAD + 2;

  // Nome (negrito)
  if sel then C.Font.Color := clHighlightText else C.Font.Color := clWindowText;
  C.Font.Style := [fsBold];
  C.Brush.Style := bsClear;
  ty := ARect.Top + 8;
  C.TextOut(tx, ty, r.Name);

  // Marca "no SD" à direita
  if r.OnSDCard then
  begin
    C.Font.Style := [fsBold];
    C.Font.Color := RGBToColor(0, 140, 0);
    C.TextOut(ARect.Right - C.TextWidth('✓ no SD') - PAD, ty, '✓ no SD');
  end;

  // Chips de tag (gênero/ano) + developer
  C.Font.Style := [];
  cx := tx;
  ty := ARect.Top + 8 + C.TextHeight('Ag') + 6;
  DrawChip(C, cx, ty, r.Genre, RGBToColor(70, 110, 200), clWhite);
  DrawChip(C, cx, ty, r.Year, RGBToColor(120, 120, 130), clWhite);
  if r.Developer <> '' then
  begin
    if sel then C.Font.Color := clHighlightText
    else C.Font.Color := RGBToColor(90, 90, 95);
    C.TextOut(cx, ty + 1, r.Developer);
    Inc(cx, C.TextWidth(r.Developer) + 8);
  end;

  // Chip dos cartões onde o jogo já foi copiado (cor teal).
  C.Font.Style := [];
  DrawChip(C, cx, ty, r.Cards, RGBToColor(20, 140, 130), clWhite);

  C.Brush.Style := bsSolid;
  C.Font.Style := [];
end;

procedure TGameLibraryView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var idx: Integer;
begin
  // Clique na coluna do checkbox alterna a marca; resto seleciona normalmente.
  if (Button = mbLeft) and (X >= PAD) and (X <= PAD + CHK_SZ + 2) then
  begin
    idx := ItemAtPos(Point(X, Y), True);
    if (idx >= 0) and (idx <= High(FRows)) then
    begin
      FRows[idx].Checked := not FRows[idx].Checked;
      Invalidate;
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

end.
