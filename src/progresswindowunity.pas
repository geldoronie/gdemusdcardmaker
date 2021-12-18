unit progresswindowunity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, gdemuunit;

type

  { TProgressWindow }

  TProgressWindow = class(TForm)
    TextLabel: TLabel;
    ProgressBar: TProgressBar;
    CounterTimer: TTimer;
    procedure CounterTimerTimer(Sender: TObject);
  public
    TextLabelVar: String;
    PositionVar: integer;
    ProgressWindow: TProgressWindow;
    procedure SetMax(value: integer);
    procedure ResetPosition;
    procedure Next(stepLabel: string);
    procedure SetTitle(value: string);
    procedure CloseProgress;
    procedure ShowProgress;
  end;

var
  ProgressWindow: TProgressWindow;

implementation

{$R *.lfm}

procedure TProgressWindow.CounterTimerTimer(Sender: TObject);
begin
  if (GDEMU.CurrentAction = 'COPYINGTOSDCARD') and (GDEMU.CurrentActionStatus = 'COPYING') then
  begin
    ProgressBar.Position:=GDEMU.CurrentCopyToSDCardActionPosition;
    TextLabel.Caption:='Copying ' + GDEMU.CurrentCopyToSDCardActionGameName;
  end
  else if (GDEMU.CurrentAction = 'COPYINGTOSDCARD') and (GDEMU.CurrentActionStatus = 'FINISHED') then
  begin
    GDEMU.UpdateSDCardGameList;
    CloseProgress;
  end
  else if (GDEMU.CurrentAction = 'SCANLOCALGAMESDIRECTORIES') and (GDEMU.CurrentActionStatus = 'SCANNING') then
  begin
    ProgressBar.Position:=GDEMU.CurrentLocalGamesScanActionPosition;
    TextLabel.Caption:='Scanning ' + GDEMU.CurrentLocalGamesScanActionGameName;
  end
  else if (GDEMU.CurrentAction = 'SCANLOCALGAMESDIRECTORIES') and (GDEMU.CurrentActionStatus = 'FINISHED') then
  begin
    CloseProgress;
  end
  else if (GDEMU.CurrentAction = 'SCANSDCARDGAMESDIRECTORIES') and (GDEMU.CurrentActionStatus = 'SCANNING') then
  begin
    ProgressBar.Position:=GDEMU.CurrentSDCardGamesScanActionPosition;
    TextLabel.Caption:='Scanning ' + GDEMU.CurrentSDCardGamesScanActionGameName;
  end
  else if (GDEMU.CurrentAction = 'SCANSDCARDGAMESDIRECTORIES') and (GDEMU.CurrentActionStatus = 'FINISHED') then
  begin
    CloseProgress;
  end;
end;

procedure TProgressWindow.SetMax(value: integer);
begin
  ProgressBar.Min:=0;
  ProgressBar.Max:=value;
end;

procedure TProgressWindow.ResetPosition;
begin
  ProgressBar.Position:=0;
end;

procedure TProgressWindow.Next(stepLabel: string);
begin
  TextLabel.Caption:=stepLabel;
end;

procedure TProgressWindow.SetTitle(value: string);
begin
  Caption:=value;
end;

procedure TProgressWindow.CloseProgress;
begin
  CounterTimer.Enabled:=False;
  Close;
end;

procedure TProgressWindow.ShowProgress;
begin
  ShowOnTop;
  CounterTimer.Enabled:=True;
end;


end.

