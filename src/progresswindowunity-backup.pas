unit progresswindowunity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls ;

type

  { TProgressWindow }

  TProgressWindow = class(TForm)
    TextLabel: TLabel;
    ProgressBar: TProgressBar;
    Timer1: TTimer;
  end;

  { TProgressThread }

  TProgressThread = class(TThread)
  private
    Title: String;
    Min: integer;
    Max: integer;
    Visible: Boolean;
  protected
    procedure Execute; override;
  public
    TextLabel: String;
    Position: integer;
    ProgressWindow: TProgressWindow;
    Constructor Create(CreateSuspended : boolean);
    procedure SetMax(value: integer);
    procedure ResetPosition;
    procedure Next(stepLabel: string);
    procedure SetTitle(value: string);
    procedure CloseProgress;
    procedure ShowProgress;
    procedure UpdateInfo;
  end;

var
  ProgressThread: TProgressThread;

implementation

{$R *.lfm}

constructor TProgressThread.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:=True;
end;

procedure TProgressThread.Execute;
begin
  ProgressWindow:=TProgressWindow.Create(nil);
  while Application.Terminated = False do
  begin
    Synchronize(@UpdateInfo);
    //Sleep(500);
  end;
  Self.Terminate;
end;

procedure TProgressThread.SetMax(value: integer);
begin
  Min:=0;
  Max:=value;
end;

procedure TProgressThread.ResetPosition;
begin
  Position:=0;
end;

procedure TProgressThread.Next(stepLabel: string);
begin
  TextLabel:=stepLabel;
  Position:=Position + 1;
end;

procedure TProgressThread.SetTitle(value: string);
begin
  Title:=value;
end;

procedure TProgressThread.CloseProgress;
begin
  //Synchronize(@ProgressWindow.Close);
  Visible:=False;
end;

procedure TProgressThread.ShowProgress;
begin
  //Synchronize(@ProgressWindow.Show);
  Visible:=True;
end;

procedure TProgressThread.UpdateInfo;
begin
  ProgressWindow.Caption:=title;
  ProgressWindow.TextLabel.Caption:=TextLabel;
  ProgressWindow.ProgressBar.Min:=Min;
  ProgressWindow.ProgressBar.Max:=Max;
  ProgressWindow.ProgressBar.Position:=Position + 1;
  if Visible then
    ProgressWindow.Show
  else
    ProgressWindow.Close;
end;

end.

