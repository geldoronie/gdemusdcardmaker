unit localgamesdirectorieswindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TLocalGamesDirectoriesDialog }

  TLocalGamesDirectoriesDialog = class(TForm)
    AddDirectoryBitBtn: TBitBtn;
    AddDirectoryBitBtn1: TBitBtn;
    ApplyBitBtn: TBitBtn;
    CancelBitBtn: TBitBtn;
    DirectoriesListBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    GamesFolderSelectDirectoryDialog: TSelectDirectoryDialog;
    procedure AddDirectoryBitBtn1Click(Sender: TObject);
    procedure AddDirectoryBitBtnClick(Sender: TObject);
    procedure ApplyBitBtnClick(Sender: TObject);
    procedure CancelBitBtnClick(Sender: TObject);
  private

  public

  end;

var
  LocalGamesDirectoriesDialog: TLocalGamesDirectoriesDialog;

implementation

{$R *.lfm}

{ TLocalGamesDirectoriesDialog }

procedure TLocalGamesDirectoriesDialog.AddDirectoryBitBtnClick(Sender: TObject);
var i: integer;
begin
  if GamesFolderSelectDirectoryDialog.Execute then
  begin
    for i:=0 to GamesFolderSelectDirectoryDialog.Files.Count -1 do
    begin
      DirectoriesListBox.AddItem(GamesFolderSelectDirectoryDialog.Files[i],nil);
    end;
  end;
end;

procedure TLocalGamesDirectoriesDialog.ApplyBitBtnClick(Sender: TObject);
begin
  //Close;
end;

procedure TLocalGamesDirectoriesDialog.CancelBitBtnClick(Sender: TObject);
begin
  //Close;
end;

procedure TLocalGamesDirectoriesDialog.AddDirectoryBitBtn1Click(Sender: TObject
  );
begin
  DirectoriesListBox.DeleteSelected;
end;
end.

