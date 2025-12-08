unit commandlogwindowunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, gdemuunit;

type

  { TCommandLogWindow }

  TCommandLogWindow = class(TForm)
    LogMemo: TMemo;
    OkBitBtn: TBitBtn;
    TitleLabel: TLabel;
    procedure OkBitBtnClick(Sender: TObject);
  public
    procedure ShowLog(title: String; logContent: TStringList);
  end;

var
  CommandLogWindow: TCommandLogWindow;

implementation

{$R *.lfm}

{ TCommandLogWindow }

procedure TCommandLogWindow.OkBitBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TCommandLogWindow.ShowLog(title: String; logContent: TStringList);
begin
  Caption:=title;
  TitleLabel.Caption:=title;
  LogMemo.Lines.Assign(logContent);
  LogMemo.SelStart:=0;
  LogMemo.SelLength:=0;
  ShowModal;
end;

end.

