unit openborcreatorwindowunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, gdemuunit;

type

  { TOpenBorCreatorWindow }

  TOpenBorCreatorWindow = class(TForm)
    AddFileBitBtn: TBitBtn;
    Bevel1: TBevel;
    OtherFilesOpenDialog: TOpenDialog;
    OutputDirBitBtn: TBitBtn;
    OutputDirLabeledEdit: TLabeledEdit;
    MainPAKFileOpenDialog: TOpenDialog;
    Label2: TLabel;
    MainPAKFileLabeledEdit: TLabeledEdit;
    OpenConverImageBitBtn1: TBitBtn;
    RemoveFileBitBtn: TBitBtn;
    OtherFilesListBox: TListBox;
    OpenConverImageBitBtn: TBitBtn;
    CreateBitBtn: TBitBtn;
    CloseBitBtn: TBitBtn;
    Image1: TImage;
    GameCoverImage: TImage;
    Label1: TLabel;
    GameNameLabeledEdit: TLabeledEdit;
    CoverImagePathLabeledEdit: TLabeledEdit;
    CoverImageOpenDialog: TOpenDialog;
    OutputSelectDirectoryDialog: TSelectDirectoryDialog;
    TopPanel: TPanel;
    BottomPanel: TPanel;
    procedure AddFileBitBtnClick(Sender: TObject);
    procedure CloseBitBtnClick(Sender: TObject);
    procedure CreateBitBtnClick(Sender: TObject);
    procedure OpenConverImageBitBtn1Click(Sender: TObject);
    procedure OpenConverImageBitBtnClick(Sender: TObject);
    procedure OutputDirBitBtnClick(Sender: TObject);
    procedure RemoveFileBitBtnClick(Sender: TObject);
  private

  public

  end;

var
  OpenBorCreatorWindow: TOpenBorCreatorWindow;

implementation

{$R *.lfm}

{ TOpenBorCreatorWindow }

procedure TOpenBorCreatorWindow.CloseBitBtnClick(Sender: TObject);
begin

end;

procedure TOpenBorCreatorWindow.CreateBitBtnClick(Sender: TObject);
begin
  if
    (GameNameLabeledEdit.Text = '') or
    (CoverImagePathLabeledEdit.Text = '') or
    (MainPAKFileLabeledEdit.Text = '') or
    (OutputDirLabeledEdit.Text = '')
  then
  begin
    ShowMessage('Please enter all required fields!');
  end;

  if
    (GameNameLabeledEdit.Text <> '') and
    (CoverImagePathLabeledEdit.Text <> '') and
    (MainPAKFileLabeledEdit.Text <> '') and
    (OutputDirLabeledEdit.Text <> '')
  then
  begin
    GDEmu.CreateOpenBORDisc(GameNameLabeledEdit.Text,CoverImagePathLabeledEdit.Text,MainPAKFileLabeledEdit.Text,OtherFilesListBox.Items,OutputDirLabeledEdit.Text);
    ShowMessage('Open BOR Game Disc Created!');
    GameNameLabeledEdit.Clear;
    CoverImagePathLabeledEdit.Clear;
    MainPAKFileLabeledEdit.Clear;
    OtherFilesListBox.Clear;
    GameCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']));
  end;
end;

procedure TOpenBorCreatorWindow.AddFileBitBtnClick(Sender: TObject);
begin
  if OtherFilesOpenDialog.Execute then
  begin
    OtherFilesListBox.Items.AddStrings(OtherFilesOpenDialog.Files);
  end;
end;

procedure TOpenBorCreatorWindow.OpenConverImageBitBtn1Click(Sender: TObject);
begin
  if MainPAKFileOpenDialog.Execute then
  begin
    MainPAKFileLabeledEdit.Text:=MainPAKFileOpenDialog.FileName;
  end;
end;

procedure TOpenBorCreatorWindow.OpenConverImageBitBtnClick(Sender: TObject);
begin
  if CoverImageOpenDialog.Execute then
  begin
    CoverImagePathLabeledEdit.Text:=CoverImageOpenDialog.FileName;
    try
      GameCoverImage.Picture.LoadFromFile(CoverImageOpenDialog.FileName);
    except
      ShowMessage('Invalid Image File');
      CoverImagePathLabeledEdit.Text:='';
    end;
  end;
end;

procedure TOpenBorCreatorWindow.OutputDirBitBtnClick(Sender: TObject);
begin
  if OutputSelectDirectoryDialog.Execute then
  begin
    OutputDirLabeledEdit.Text:=OutputSelectDirectoryDialog.FileName;
  end;
end;

procedure TOpenBorCreatorWindow.RemoveFileBitBtnClick(Sender: TObject);
begin
  OtherFilesListBox.DeleteSelected;
end;

end.

