unit aboutwindowunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
    Label6: TLabel;
    OkBitBtn: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Version: TLabel;
    Memo1: TMemo;
    StaticText1: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure OkBitBtnClick(Sender: TObject);
  private

  public

  end;

var
  AboutWindow: TAboutWindow;

implementation

{$R *.lfm}

{ TAboutWindow }

procedure TAboutWindow.FormCreate(Sender: TObject);
begin
  Version.Caption:='0.0.1-alpha';
end;

procedure TAboutWindow.OkBitBtnClick(Sender: TObject);
begin
  Close;
end;

end.

