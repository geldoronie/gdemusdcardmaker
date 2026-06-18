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
  end;

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

end.
