program gdemugui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainWindowUnit, localgamesdirectorieswindows, gdemuunit,
  progresswindowunity, sysutils, aboutwindowunit, openborcreatorwindowunit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='GDEmu Creator';
  Application.Scaled:=True;
  Application.Initialize;
  GDEmu:=TGDEmu.Create(True);
  GDEmu.Start;
  GDEmu.SetApplicationPath(SysUtils.ExtractFileDir(Application.ExeName));
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TLocalGamesDirectoriesDialog, 
    LocalGamesDirectoriesDialog);
  Application.CreateForm(TProgressWindow,
    ProgressWindow);
  Application.CreateForm(TAboutWindow, AboutWindow);
  Application.CreateForm(TOpenBorCreatorWindow, OpenBorCreatorWindow);
  Application.Run;
end.

