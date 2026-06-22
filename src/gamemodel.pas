unit gamemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type

  { TGDEmuGame — metadata for a single Dreamcast game (local or on the SD card). }
  TGDEmuGame = class
    Id: String;
    Name: String;
    LegalName: String;
    InternalName: String;
    SlugName: String;
    Path: String;
    Extension: String;
    Index: integer;
    Date: String;
    Version: String;
    Region: String;
    VGA: String;
    Disc: String;
    DiscName: String;
    CatalogID: String;
    // Preenchidos ao cruzar a biblioteca local com o cartão:
    DiscSize: Int64;     // tamanho do maior arquivo do disco (desempata IP.BIN igual)
    OnSDCard: Boolean;   // este jogo local já existe no SD Card
    SDCardIndex: integer; // pasta (slot) onde ele está no SD, quando OnSDCard
  end;

// Tamanho (bytes) do maior arquivo de dados do disco numa pasta de jogo. Estável
// entre cópias (os tracks são copiados byte-a-byte) e distingue homebrews que
// compartilham o mesmo IP.BIN (ex.: vários OpenBOR).
function LargestDiscFileSize(const GamePath: String): Int64;

// Display name for a game folder: contents of name.txt if present, else the
// folder name.
function GetGameName(GamePath: String): String;
// URL/cache-friendly slug derived from a game name (lowercase, dashes).
function GetGameSlugName(name: String): String;
// Game name stripped of characters illegal on disc filesystems.
function GetGameLegalName(name: String): String;

implementation

function GetGameName(GamePath: String): String;
var nameTextFile: TStringList;
begin
  nameTextFile:=TStringList.Create;
  if SysUtils.FileExists(ConcatPaths([GamePath,'name.txt'])) then
  begin
    nameTextFile.LoadFromFile(ConcatPaths([GamePath,'name.txt']));
    Result:=nameTextFile.Text;
  end
  else
    Result:=SysUtils.ExtractFileName(GamePath);
  nameTextFile.Destroy;
end;

function GetGameSlugName(name: String): String;
var slugName: String;
begin
  slugName:=ReplaceRegExpr('[^.a-zA-Z0-9-\ ]+', name, '', False);
  slugName:=LowerCase(Trim(StringReplace(slugName, ' ', '-', [rfReplaceAll])));
  Result:=slugName;
end;

function GetGameLegalName(name: String): String;
var legalName: String;
begin
  legalName:=ReplaceRegExpr('[^.a-zA-Z0-9-\ ]+', name, '', False);
  Result:=Trim(legalName);
end;

function LargestDiscFileSize(const GamePath: String): Int64;
var sr: TSearchRec; ext: String;
begin
  Result:=0;
  if SysUtils.FindFirst(ConcatPaths([GamePath, AllFilesMask]), faAnyFile, sr) = 0 then
  begin
    try
      repeat
        if (sr.Attr and faDirectory) = 0 then
        begin
          ext:=LowerCase(ExtractFileExt(sr.Name));
          if (ext = '.bin') or (ext = '.raw') or (ext = '.cdi') or
             (ext = '.iso') or (ext = '.img') then
            if sr.Size > Result then
              Result:=sr.Size;
        end;
      until SysUtils.FindNext(sr) <> 0;
    finally
      SysUtils.FindClose(sr);
    end;
  end;
end;

end.
