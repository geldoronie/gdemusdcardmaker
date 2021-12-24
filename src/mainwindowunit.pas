unit mainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ExtCtrls,
  Menus, Buttons, StdCtrls, ComCtrls, localgamesdirectorieswindows, gdemuunit,
  progresswindowunity, LCLType, aboutwindowunit, openborcreatorwindowunit;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    AddLocalGameDirectoriesMenuItem: TMenuItem;
    DownloadCoverMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    CreditsMenuItem: TMenuItem;
    DiskUseProgressBar: TProgressBar;
    ToolsMenuItem: TMenuItem;
    OpenBORDiscCreatorMenuItem: TMenuItem;
    UpdateSDCardGamesListMenuItem: TMenuItem;
    RemoveGamesFromSDCardMenuItem: TMenuItem;
    CopySelectedGamesToSDCardMenuItem: TMenuItem;
    N2: TMenuItem;
    ViewMenuItem: TMenuItem;
    UpdateGamesListSDCardBitBtn: TBitBtn;
    CopySelectedBitBtn: TBitBtn;
    RemoveGameFromSDCardBitBtn: TBitBtn;
    LocalGameMD5Label: TLabel;
    SDCardGameDiscTypeLabel: TLabel;
    LocalGameDiscTypeLabel: TLabel;
    LocalGameNameLabel: TLabel;
    SDCardGamePathLabel: TLabel;
    SDCardGameNameLabel: TLabel;
    LocalGameCoverImage: TImage;
    SDCardCoverImage: TImage;
    LoadSDCardBitBtn: TBitBtn;
    OpenLocalGamesDirectoriesDialogBitBtn: TBitBtn;
    LocalGamesList: TCheckListBox;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    N1: TMenuItem;
    OpenMenuItem: TMenuItem;
    LocalGamesToolsPanel: TPanel;
    SDCardGameInfoPanel: TPanel;
    LocalGameInfoPanel: TPanel;
    LocalGamePathLabel: TLabel;
    SDCardGameIndexLabel: TLabel;
    SDCardGameMD5Label: TLabel;
    SDCardGamesToolsButtonsPanel: TPanel;
    LocalGamesToolsButtonsPanel: TPanel;
    SDCardGamesToolsPanel: TPanel;
    SDCardList: TCheckListBox;
    MainWindowMenu: TMainMenu;
    OpenSDCardDirectorySelectDirectoryDialog: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure CopySelectedBitBtnClick(Sender: TObject);
    procedure CreditsMenuItemClick(Sender: TObject);
    procedure DownloadCoverMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure LocalGamesListSelectionChange(Sender: TObject; User: boolean);
    procedure OpenBORDiscCreatorMenuItemClick(Sender: TObject);
    procedure RemoveGameFromSDCardBitBtnClick(Sender: TObject);
    procedure LoadSDCardBitBtnClick(Sender: TObject);
    procedure OpenLocalGamesDirectoriesDialogBitBtnClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SDCardListSelectionChange(Sender: TObject; User: boolean);
    procedure UpdateGamesListSDCardBitBtnClick(Sender: TObject);
    procedure UpdateSDCardGamesListMenuItemClick(Sender: TObject);
  private

  public

  end;

var
  MainWindow: TMainWindow;

procedure OnFinishGamesCopy;
procedure OnFinishSDCardGamesScan;
procedure OnFinishLocalGamesScan;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.OpenLocalGamesDirectoriesDialogBitBtnClick(Sender: TObject);
begin
  LocalGamesDirectoriesDialog.ShowModal;
  if LocalGamesDirectoriesDialog.ModalResult = mrOK then
  begin
    GDEmu.SetLocalGamesDirectories(LocalGamesDirectoriesDialog.DirectoriesListBox.Items);
    ProgressWindow.SetTitle('Scan local game directories');
    GDEmu.StartScanLocalGamesDirectories(@OnFinishLocalGamesScan);
    ProgressWindow.SetMax(GDEmu.CurrentLocalGamesScanActionCount);
    ProgressWindow.ShowProgress;
    Enabled:=False;
  end;
end;

procedure TMainWindow.OpenMenuItemClick(Sender: TObject);
begin

end;

procedure TMainWindow.SDCardListSelectionChange(Sender: TObject; User: boolean);
var
    coverImageFilename: String;
begin
  User:=False;
  if GDEmu.SDCardGamesListCount > 0 then
  begin
    SDCardGameDiscTypeLabel.Caption:='Extension: ' + SysUtils.UpperCase(GDEmu.SDCardGamesList[SDCardList.ItemIndex].Extension);
    SDCardGameNameLabel.Caption:='Name: ' + GDEmu.SDCardGamesList[SDCardList.ItemIndex].Name;
    SDCardGamePathLabel.Caption:='Path: ' + GDEmu.SDCardGamesList[SDCardList.ItemIndex].Path;
    SDCardGameIndexLabel.Caption:='Index: ' + Format('[%.2d]',[GDEmu.SDCardGamesList[SDCardList.ItemIndex].Index]);
    SDCardGameMD5Label.Caption:='MD5: ' + GDEmu.SDCardGamesList[SDCardList.ItemIndex].Id;
    if DownloadCoverMenuItem.Checked then
    begin
      try
        coverImageFilename:=GDEmu.GetGameCover(GDEmu.SDCardGamesList[SDCardList.ItemIndex]);
        if coverImageFilename <> '' then
        begin
          try
            SDCardCoverImage.Picture.LoadFromFile(coverImageFilename);
          except
            SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
            WriteLn('Failed to get cover image from Games Database');
          end;
        end;
      except
        SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
        WriteLn('Failed to get cover image from Games Database');
      end;
    end
      else
      begin
        SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
      end;
  end;
end;

procedure TMainWindow.LoadSDCardBitBtnClick(Sender: TObject);
begin
  if OpenSDCardDirectorySelectDirectoryDialog.Execute then
  begin
    GDEmu.SetSDCardGamesDirectory(OpenSDCardDirectorySelectDirectoryDialog.FileName);
    ProgressWindow.SetTitle('Scan SDCard games directories');
    GDEmu.StartScanSDCardGamesDirectories(@OnFinishSDCardGamesScan);
    ProgressWindow.SetMax(GDEmu.CurrentSDCardGamesScanActionCount);
    ProgressWindow.ShowProgress;
    Enabled:=False;
  end;
end;

procedure TMainWindow.CopySelectedBitBtnClick(Sender: TObject);
var i: integer;
    count: integer = 0;
    HasSelected: Boolean;
begin
  HasSelected:=False;
  for i:=0 to LocalGamesList.Count -1 do
  begin
    if LocalGamesList.Checked[i] then
    begin
      HasSelected:=True;
      Break;
    end;
  end;

  if (HasSelected) and (GDEmu.SDCardLoaded) then
  begin
    if Application.MessageBox('Do you want to proceed copying the selected games?','Confirmation', MB_YESNO) = mrYes then
    begin
      GDEmu.ClearSelectedLocalGamesToCopy;
      ProgressWindow.SetTitle('Copying to SD Card');
      MainWindow.Enabled:=False;
      for i:=0 to LocalGamesList.Count -1 do
      begin
        if LocalGamesList.Checked[i] then
        begin
          GDEmu.SelectLocalGameToCopy(i);
          count:=count + 1;
        end;
      end;
      GDEmu.StartCopySelectedLocalGamesToSDCard(@OnFinishGamesCopy);
      ProgressWindow.SetMax(count);
      ProgressWindow.ShowProgress;
    end;
    GDEmu.UpdateSDCardGameList;
  end;
end;

procedure TMainWindow.CreditsMenuItemClick(Sender: TObject);
begin
  AboutWindow.ShowModal;
end;

procedure TMainWindow.AboutMenuItemClick(Sender: TObject);
begin

end;

procedure TMainWindow.DownloadCoverMenuItemClick(Sender: TObject);
begin
  if DownloadCoverMenuItem.Checked then
    DownloadCoverMenuItem.Checked:=False
  else
    DownloadCoverMenuItem.Checked:=True;
end;

procedure TMainWindow.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainWindow.LocalGamesListSelectionChange(Sender: TObject;
  User: boolean);
var
    coverImageFilename: String;
begin
  if GDEmu.LocalGamesListCount > 0 then
  begin
    LocalGameDiscTypeLabel.Caption:='Extension: ' + SysUtils.UpperCase(GDEmu.LocalGamesList[LocalGamesList.ItemIndex].Extension);
    LocalGameNameLabel.Caption:='Name: ' + GDEmu.LocalGamesList[LocalGamesList.ItemIndex].Name;
    LocalGamePathLabel.Caption:='Path: ' + GDEmu.LocalGamesList[LocalGamesList.ItemIndex].Path;
    LocalGameMD5Label.Caption:='MD5: ' + GDEmu.LocalGamesList[LocalGamesList.ItemIndex].Id;
    if DownloadCoverMenuItem.Checked then
    begin
      try
        coverImageFilename:=GDEmu.GetGameCover(GDEmu.LocalGamesList[LocalGamesList.ItemIndex]);
        if coverImageFilename <> '' then
        begin
          try
            LocalGameCoverImage.Picture.LoadFromFile(coverImageFilename);
          except
            SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
            WriteLn('Failed to get cover image from Games Database');
          end;
        end;
      except
        SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
        WriteLn('Failed to get cover image from Games Database');
      end;
    end
      else
      begin
        SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
      end;
  end;
end;

procedure TMainWindow.OpenBORDiscCreatorMenuItemClick(Sender: TObject);
begin
  OpenBorCreatorWindow.ShowModal;
end;

procedure TMainWindow.RemoveGameFromSDCardBitBtnClick(Sender: TObject);
var i: integer;
    HasSelected: Boolean;
begin
  HasSelected:=False;
  for i:=0 to SDCardList.Count -1 do
  begin
    if SDCardList.Checked[i] then
    begin
      HasSelected:=True;
      Break;
    end;
  end;

  if (HasSelected)  and (GDEmu.SDCardLoaded) then
  begin
    GDEmu.ClearSelectedSDCardGamesToRemove;
    if Application.MessageBox('Do you want to remove the selected Games?','Confirmation', MB_YESNO) = mrYes then
    begin
      for i:=0 to SDCardList.Count -1 do
      begin
        if SDCardList.Checked[i] then
        begin
          GDEmu.SelectSDCardGameToRemove(i);
        end;
      end;
      GDEmu.RemoveFromSDCard;
      SDCardList.Clear;
      for i:=0 to GDEmu.SDCardGamesListCount -1 do
      begin
        SDCardList.AddItem(GDEmu.SDCardGamesList[i].Name,nil);
      end;
    end;
    GDEmu.UpdateSDCardGameList;
  end;
end;

procedure TMainWindow.UpdateGamesListSDCardBitBtnClick(Sender: TObject);
begin
  if GDEmu.SDCardLoaded then
  begin
    GDEmu.UpdateSDCardGameList;
    ShowMessage('GDEmu Game list has updated!');
    //GDEmu.ScanSDCardGamesDirectory;
    //SDCardList.Clear;
    //for i:=0 to GDEmu.SDCardGamesListCount -1 do
    //begin
    //  SDCardList.AddItem(GDEmu.SDCardGamesList[i].Name,nil);
    //end;
  end;
end;

procedure TMainWindow.UpdateSDCardGamesListMenuItemClick(Sender: TObject);
begin

end;

procedure OnFinishGamesCopy;
var i: integer;
begin
  GDEmu.UpdateSDCardGameList;
  GDEmu.ScanSDCardGamesDirectory;
  MainWindow.SDCardList.Clear;
  for i:=0 to GDEmu.SDCardGamesListCount -1 do
  begin
    MainWindow.SDCardList.AddItem(GDEmu.SDCardGamesList[i].Name,nil);
  end;
  MainWindow.Enabled:=True;
end;

procedure OnFinishSDCardGamesScan;
var i: integer;
begin
  MainWindow.SDCardList.Clear;
  for i:=0 to GDEmu.SDCardGamesListCount -1 do
  begin
    MainWindow.SDCardList.AddItem(GDEmu.SDCardGamesList[i].Name,nil);
  end;
  MainWindow.Enabled:=True;
end;

procedure OnFinishLocalGamesScan;
var i: integer;
begin
  MainWindow.LocalGamesList.Clear;
  for i:=0 to GDEmu.LocalGamesListCount -1 do
  begin
    MainWindow.LocalGamesList.AddItem(GDEmu.LocalGamesList[i].Name,nil);
  end;
  MainWindow.Enabled:=True;
end;

end.

