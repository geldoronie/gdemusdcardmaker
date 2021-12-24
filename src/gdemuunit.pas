unit gdemuunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, md5, RegExpr, process, fphttpclient,
  openssl, opensslsockets, DOM_HTML, DOM, SAX_HTML, Graphics, Dos, fpjson,
  jsonparser, jsonConf;

type

    { TGDEmuGame }
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

    { THexDump }

    THexDump = class
    private
      ExecutablePath: String;
      Executable: String;
    public
      constructor Create;
      function GetIPBINInfo(sdCardGame: TGDEmuGame; outputDir: string): TGDEmuGame;
    end;

    { TGDIToolsProcess }

    TGDIToolsProcess = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
    public
      constructor Create;
      function GetMetaFile(GamePath: String; outputPath: String): String;
    end;

    { TGenISOImage }

    TGenISOImage = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
    public
      constructor Create;
      procedure GenerateISO(dataPath: String; tempPath: String; outputPath: String);
      procedure GenerateISO(name: String; ipBinPath: string; firstStReadBINPath: string; otherFiles: TStrings; outputPath: String);
    end;

    { TCDI4DC }

    TCDI4DC = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
    public
      constructor Create;
      procedure ConvertToCDI(inputPath: String; outputPath: String);
    end;

    { TCDIRIP }

    TCDIRIP = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
    public
      constructor Create;
      function ExtractIPBIN(inputPath: String; outputPath: String): String;
    end;

    { TGameDatabaseSearch }
    TGameDatabaseSearch = class
      BaseUrl: String;
      SearchText: String;
      SearchType: String;
      System: String;
      Sort: String;
    end;

    { TGDEmu }
    PStartCopySelectedLocalGamesToSDCard = procedure;
    PStartScanLocalGamesDirectories = procedure;
    PStartScanSDCardGamesDirectories = procedure;
    TGDEmu = class(TThread)
    protected
      procedure Execute; override;
      procedure OnFinishGamesCopy;
      procedure OnFinishLocalGamesScan;
      procedure OnFinishSDCardGamesScan;
    private
      HTTPClient: TFPHttpClient;
      _onFinishGamesCopy: PStartCopySelectedLocalGamesToSDCard;
      _onFinishLocalGamesScan: PStartScanLocalGamesDirectories;
      _onFinishSDCardGamesScan: PStartScanSDCardGamesDirectories;
      procedure UpdateSDCardGameInfo(index: integer);
    public
      ApplicationPath: String;
      GDIToolsPath: String;
      CDIRipPath: String;
      GDIToolsProcess: TGDIToolsProcess;
      HexDump: THexDump;
      GenISOImage: TGenISOImage;
      CDI4DC: TCDI4DC;
      CDIRIP: TCDIRIP;
      GDEmuIni: TStringList;
      GDEmuListIni: TStringList;
      GameDatabaseSearch: TGameDatabaseSearch;
      LocalGamesDirectoriesList: TStringList;
      SDCardGamesDirectory: String;
      LocalGamesList: Array of TGDEmuGame;
      SelectedLocalGamesList: Array of integer;
      SelectedLocalGamesListCount: integer;
      SelectedSDCardGamesToRemoveList: Array of integer;
      SelectedSDCardGamesToRemoveListCount: integer;
      LocalGamesListCount: integer;
      SDCardGamesList: Array of TGDEmuGame;
      SDCardGamesListCount: integer;
      SDCardGamesListIndexCount: integer;
      TestMode: Boolean;
      SDCardLoaded: Boolean;
      CurrentAction: String;
      CurrentActionStatus: String;
      CurrentCopyToSDCardActionPosition: integer;
      CurrentCopyToSDCardActionCount: integer;
      CurrentCopyToSDCardActionGameName: String;
      CurrentLocalGamesScanActionPosition: integer;
      CurrentLocalGamesScanActionCount: integer;
      CurrentLocalGamesScanActionGameName: String;
      CurrentSDCardGamesScanActionPosition: integer;
      CurrentSDCardGamesScanActionCount: integer;
      CurrentSDCardGamesScanActionGameName: String;
      constructor Create(CreateSuspended : boolean);
      procedure SetApplicationPath(value: String);
      procedure SetLocalGamesDirectories(List: TStrings);
      procedure SetSDCardGamesDirectory(Path: String);
      procedure ScanLocalGamesDirectories;
      procedure StartScanLocalGamesDirectories(onLocalGamesScanFinished: PStartScanLocalGamesDirectories);
      procedure ScanSDCardGamesDirectory;
      procedure StartScanSDCardGamesDirectories(onSDCardGamesScanFinished: PStartScanSDCardGamesDirectories);
      procedure UpdateSDCardGameList;
      procedure ClearLocalGamesDirectories;
      procedure ClearSDCardGamesDirectories;
      procedure RemoveFromSDCard;
      procedure FixSDCardFolders;
      procedure CopySelectedLocalGamesToSDCard;
      procedure StartCopySelectedLocalGamesToSDCard(onCopyFished: PStartCopySelectedLocalGamesToSDCard = nil);
      procedure SelectLocalGameToCopy(index: integer);
      procedure SelectSDCardGameToRemove(index: integer);
      procedure ClearSelectedLocalGamesToCopy;
      procedure ClearSelectedSDCardGamesToRemove;
      procedure UpdateGDEmuINI;
      procedure CreateGDEmuImage;
      procedure CreateOpenBORDisc(name: String; coverImagePath: string; mainPakFilePath: string; otherFilesPaths: TStrings; outputPath: string);
      procedure CreateInfoCacheFile(game: TGDEmuGame);
      function GetMetaFileInfo(game: TGDEmuGame): TGDEmuGame;
      function GetGameCover(game: TGDEmuGame): String;
      function GetMetaFileInfoCache(game: TGDEmuGame): TGDEmuGame;
    end;

var
  GDEmu: TGDEmu;

function GetGameName(GamePath: String): String;
function GetGameSlugName(name: String): String;
function GetGameLegalName(name: String): String;

implementation

{ THexDump }

constructor THexDump.Create;
begin
  ExecutablePath:='';
  Executable:='/usr/bin/hexdump';
end;

function THexDump.GetIPBINInfo(sdCardGame: TGDEmuGame; outputDir: string): TGDEmuGame;
var
  internalName: ansistring;
  disc: ansistring;
  vga: ansistring;
  region: ansistring;
  version: ansistring;
  date: ansistring;
  catalogID: ansistring;
begin
  //INTERNAL NAME
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x80',
      '-n',
      '128',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    internalName,
    [poWaitOnExit],
    swoNone
  );
  //DISC
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x2B',
      '-n',
      '3',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    disc,
    [poWaitOnExit],
    swoNone
  );
  //VGA
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x3D',
      '-n',
      '1',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    vga,
    [poWaitOnExit],
    swoNone
  );
  //REGION
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x30',
      '-n',
      '8',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    region,
    [poWaitOnExit],
    swoNone
  );
  //VERSION
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x4A',
      '-n',
      '6',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    version,
    [poWaitOnExit],
    swoNone
  );
  //DATE
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x50',
      '-n',
      '8',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    date,
    [poWaitOnExit],
    swoNone
  );
  //CATALOG ID
  RunCommand(
    Executable,
    [
      '-v',
      '-e',
      '"%c"',
      '-s',
      '0x40',
      '-n',
      '8',
      ConcatPaths([outputDir,'ip.bin'])
    ],
    catalogID,
    [poWaitOnExit],
    swoNone
  );
  sdCardGame.Id:=MD5Print(MD5File(ConcatPaths([outputDir,'ip.bin'])));
  sdCardGame.Disc:=Trim(disc);
  sdCardGame.VGA:=Trim(vga);
  sdCardGame.Region:=Trim(region);
  sdCardGame.Version:=Trim(version);
  sdCardGame.Date:=Trim(date);
  sdCardGame.InternalName:=Trim(internalName);
  sdCardGame.CatalogID:=catalogID;
  sdCardGame.SlugName:=StringReplace(
    LowerCase(sdCardGame.InternalName),
    ' ','-',[rfReplaceAll]
  );
  sdCardGame.SlugName:=StringReplace(
    sdCardGame.SlugName,
    '/','',[rfReplaceAll]
  );
  sdCardGame.SlugName:=StringReplace(
    sdCardGame.SlugName,
    '-disc-1','',[rfReplaceAll]
  );
  sdCardGame.SlugName:=StringReplace(
    sdCardGame.SlugName,
    '-disc-2','',[rfReplaceAll]
  );
  sdCardGame.SlugName:=StringReplace(
    sdCardGame.SlugName,
    '-disc-3','',[rfReplaceAll]
  );
  sdCardGame.SlugName:=StringReplace(
    sdCardGame.SlugName,
    '-disc-4','',[rfReplaceAll]
  );
  Result:=sdCardGame;
end;

{ TGDIToolsProcess }

constructor TGDIToolsProcess.Create;
begin
  ExecutablePath:='gditools.py';
  Executable:='/usr/bin/python';
end;

function TGDIToolsProcess.GetMetaFile(GamePath: String; outputPath: String): String;
var
  outputString: ansistring;
begin
  RunCommand(
    Executable,
    [
      ConcatPaths([ApplicationPath,'tools',ExecutablePath]),
      '-i',
      GamePath,
      '-b',
      ConcatPaths([outputPath,'ip.bin'])
    ],
    outputString,
    [poWaitOnExit],
    swoNone
  );
  Result:=outputString;
end;

{ TGenISOImage }

constructor TGenISOImage.Create;
begin
  ExecutablePath:='';
  Executable:='/usr/bin/genisoimage';
end;

procedure TGenISOImage.GenerateISO(dataPath: String; tempPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  //genisoimage -C 0,11702 -V GDMENU -G data/ip.bin -r -J -l -input-charset iso8859-1 -o gdmenu.iso data/1ST_READ.BIN $GDMENU_INI
  RunCommand(
    Executable,
    [
      '-C',
      '0,11702',
      '-V',
      'GDMENU',
      '-G',
      ConcatPaths([dataPath,'ip.bin']),
      '-r',
      '-J',
      '-l',
      '-input-charset',
      'iso8859-1',
      '-o',
      ConcatPaths([outputPath,'gdmenu.iso']),
      ConcatPaths([dataPath,'1ST_READ.BIN']),
      ConcatPaths([tempPath,'LIST.INI'])
    ],
    outputString,
    [poWaitOnExit],
    swoNone
  );
end;

procedure TGenISOImage.GenerateISO(name: String; ipBinPath: string; firstStReadBINPath: string; otherFiles: TStrings; outputPath: String);
var
  outputString: ansistring;
  i: integer;
  parameters: TStringList;
  commandOut: TStringList;
  shortName: String;
begin
  parameters:=TStringList.Create;
  commandOut:=TStringList.Create;


  if name.Length > 25 then
  begin
    shortName:=name.Substring(0,25);
  end
  else
    shortName:=name;

  parameters.AddStrings([
    '-C',
    '0,11702',
    '-V',
    '"'+ shortName + '"',
    '-G',
    ipBinPath,
    '-r',
    '-J',
    '-l',
    '-input-charset',
    'iso8859-1',
    '-o',
    outputPath
  ]);
  parameters.Add(firstStReadBINPath );
  for i:=0 to otherFiles.Count -1 do
  begin
    parameters.Add(otherFiles[i]);
  end;
  commandOut.AddStrings(parameters);
  RunCommand(
    Executable,
    parameters.ToStringArray,
    outputString,
    [poStderrToOutPut,poWaitOnExit],
    swoNone
  );
  commandOut.Add(outputString);
  //commandOut.SaveToFile(ConcatPaths([ApplicationPath,'commands.out']));
  commandOut.Destroy;
  parameters.Destroy;
end;

{ TCDI4DC }

constructor TCDI4DC.Create;
begin
  ExecutablePath:='';
  Executable:='tools/cdi4dc';
end;

procedure TCDI4DC.ConvertToCDI(inputPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  RunCommand(
    ConcatPaths([ApplicationPath,Executable]),
    [
      inputPath,
      outputPath
    ],
    outputString,
    [poWaitOnExit],
    swoNone
  );
end;

{ TCDIRIP }

constructor TCDIRIP.Create;
begin
  ExecutablePath:='';
  Executable:='tools/cdirip';
end;

function TCDIRIP.ExtractIPBIN(inputPath: String; outputPath: String): String;
var
  outputString: ansistring;
  cacheFiles: TStringList;
  isoFileStream: TFileStream;
  ipFileStream: TFileStream;
  fileBuffer: Array [0..32768] of Byte;
  bufferSize: LongInt = 32768;
begin
  cacheFiles:=TStringList.Create;
  DeleteDirectory(ConcatPaths([outputPath,'cdicache']), False);
  CreateDir(ConcatPaths([outputPath,'cdicache']));
  RunCommand(
    ConcatPaths([ApplicationPath,Executable]),
    [
      inputPath,
      ConcatPaths([outputPath,'cdicache'])
    ],
    outputString,
    [poWaitOnExit],
    swoNone
  );

  FindAllFiles(cacheFiles, ConcatPaths([outputPath,'cdicache']), '*.iso', False, faAnyFile);

  if cacheFiles.Count > 0 then
  begin
    isoFileStream:=TFileStream.Create(cacheFiles[0], fmOpenRead);
    DeleteFile(ConcatPaths([outputPath, 'cdicache', 'ip.bin']));
    ipFileStream:=TFileStream.Create(ConcatPaths([outputPath, 'cdicache', 'ip.bin']), fmCreate);
    isoFileStream.ReadBuffer(fileBuffer,bufferSize);
    isoFileStream.Free;
    ipFileStream.WriteBuffer(fileBuffer,bufferSize);
    ipFileStream.Free;
    cacheFiles.Destroy;
    Result:=outputString;
  end
  else
  begin
    Result:='';
  end;
end;

{ TGDEmu }

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

procedure TGDEmu.Execute;
begin
  while Terminated = False do
  begin
    if (CurrentAction = 'COPYINGTOSDCARD') and (CurrentActionStatus = 'PENDING') then
    begin
      CurrentActionStatus:='COPYING';
      CopySelectedLocalGamesToSDCard;
      CurrentActionStatus:='FINISHED';
      Synchronize(@OnFinishGamesCopy);
    end;

    if (CurrentAction = 'SCANLOCALGAMESDIRECTORIES') and (CurrentActionStatus = 'PENDING') then
    begin
      CurrentActionStatus:='SCANNING';
      ScanLocalGamesDirectories;
      CurrentActionStatus:='FINISHED';
      Synchronize(@OnFinishLocalGamesScan);
    end;

    if (CurrentAction = 'SCANSDCARDGAMESDIRECTORIES') and (CurrentActionStatus = 'PENDING') then
    begin
      CurrentActionStatus:='SCANNING';
      ScanSDCardGamesDirectory;
      CurrentActionStatus:='FINISHED';
      Synchronize(@OnFinishSDCardGamesScan);
    end;
    Sleep(500);
  end;
end;

procedure TGDEmu.OnFinishGamesCopy;
begin
  if _onFinishGamesCopy <> nil then
    _onFinishGamesCopy;
end;

procedure TGDEmu.OnFinishLocalGamesScan;
begin
  if _onFinishLocalGamesScan <> nil then
    _onFinishLocalGamesScan;
end;

procedure TGDEmu.OnFinishSDCardGamesScan;
begin
  if _onFinishSDCardGamesScan <> nil then
    _onFinishSDCardGamesScan;
end;

constructor TGDEmu.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate:=True;
  GDEmuIni:=TStringList.Create;
  GDEmuIni.LoadFromFile( ConcatPaths(['./','ini','GDEMU.INI']) );
  GDEmuListIni:=TStringList.Create;
  GDEmuListIni.LoadFromFile( ConcatPaths(['./','ini','LIST.INI']) );
  LocalGamesDirectoriesList:=TStringList.Create;
  GDIToolsProcess:=TGDIToolsProcess.Create;
  HexDump:=THexDump.Create;
  GenISOImage:=TGenISOImage.Create;
  CDI4DC:=TCDI4DC.Create;
  CDIRIP:=TCDIRIP.Create;
  HTTPClient:=TFPHttpClient.Create(nil);
  InitSSLInterface;
end;

procedure TGDEmu.SetApplicationPath(value: String);
begin
  ApplicationPath:=value;
  GDIToolsProcess.ApplicationPath:=ApplicationPath;
  GenISOImage.ApplicationPath:=ApplicationPath;
  CDI4DC.ApplicationPath:=ApplicationPath;
  CDIRIP.ApplicationPath:=ApplicationPath;
end;

procedure TGDEmu.SetLocalGamesDirectories(List: TStrings);
begin
  LocalGamesDirectoriesList.Clear;
  LocalGamesDirectoriesList.AddStrings(List);
end;

procedure TGDEmu.SetSDCardGamesDirectory(Path: String);
begin
  SDCardLoaded:=False;
  SDCardGamesDirectory:=Path;
  //ShowMessage(FindDiskFilename(SDCardGamesDirectory));
end;

procedure TGDEmu.ScanLocalGamesDirectories;
var i,j: integer;
    Directories: TStringList;
    GameDirectoryContentGDI: TStringList;
    GameDirectoryContentCDI: TStringList;
    CacheList: TStringList;
begin
   Directories:=TStringList.Create;
   CacheList:=TStringList.Create;
   for i:=0 to LocalGamesDirectoriesList.Count -1 do
   begin
     Directories.AddStrings(FileUtil.FindAllDirectories(LocalGamesDirectoriesList[i],False));
     ClearLocalGamesDirectories;
     for j:=0 to Directories.Count -1 do
     begin
       GameDirectoryContentGDI:=TStringList.Create;
       GameDirectoryContentCDI:=TStringList.Create;
       FileUtil.FindAllFiles(GameDirectoryContentGDI,Directories[j],'*.gdi', False, faAnyFile);
       FileUtil.FindAllFiles(GameDirectoryContentCDI,Directories[j],'*.cdi', False, faAnyFile);
       CurrentLocalGamesScanActionGameName:=SysUtils.ExtractFileName(Directories[j]);
       if (
             (GameDirectoryContentGDI.Count > 0) or
             (GameDirectoryContentCDI.Count > 0)
          ) and
          (SysUtils.ExtractFileName(Directories[j]) <> '')
       then
       begin
         SetLength(LocalGamesList,LocalGamesListCount + 1);
         LocalGamesList[LocalGamesListCount]:=TGDEmuGame.Create;
         LocalGamesList[LocalGamesListCount].Name:=SysUtils.ExtractFileName(Directories[j]);
         LocalGamesList[LocalGamesListCount].SlugName:=GetGameSlugName(LocalGamesList[LocalGamesListCount].Name);
         LocalGamesList[LocalGamesListCount].Path:=Directories[j];

         if GameDirectoryContentGDI.Count > 0 then
         begin
           LocalGamesList[LocalGamesListCount].Extension:=SysUtils.ExtractFileExt(GameDirectoryContentGDI[0]);
         end
         else if GameDirectoryContentCDI.Count > 0 then
         begin
           LocalGamesList[LocalGamesListCount].Extension:=SysUtils.ExtractFileExt(GameDirectoryContentCDI[0]);
         end;

         CacheList.Add(LocalGamesList[LocalGamesListCount].SlugName);
         GetMetaFileInfo(LocalGamesList[LocalGamesListCount]);
         CreateInfoCacheFile(LocalGamesList[LocalGamesListCount]);

         LocalGamesListCount:=LocalGamesListCount + 1;
         CacheList.SaveToFile(ConcatPaths([ApplicationPath,'cache','cache.list']));
       end;
       CurrentLocalGamesScanActionPosition:=CurrentLocalGamesScanActionPosition + 1;
       GameDirectoryContentGDI.Destroy;
       GameDirectoryContentCDI.Destroy;
     end;
   end;
   Directories.Destroy;
end;

procedure TGDEmu.ClearLocalGamesDirectories;
var i: longint;
begin
  if LocalGamesList <> nil then
  begin
    for i:=0 to LocalGamesListCount -1 do
    begin
       LocalGamesList[i].Destroy;
    end;
    SetLength(LocalGamesList,0);
    LocalGamesListCount:=0;
  end;
end;

procedure TGDEmu.ScanSDCardGamesDirectory;
var i: integer;
    Directories: TStringList;
    GameDirectoryContentGDI: TStringList;
    GameDirectoryContentCDI: TStringList;
    CacheList: TStringList;
begin
   CacheList:=TStringList.Create;
   Directories:=TStringList.Create;
   Directories.AddStrings(FileUtil.FindAllDirectories(SDCardGamesDirectory,False));
   ClearSDCardGamesDirectories;
   SDCardGamesListIndexCount:=1;
   for i:=0 to Directories.Count -1 do
   begin
     GameDirectoryContentGDI:=TStringList.Create;
     GameDirectoryContentCDI:=TStringList.Create;
     FileUtil.FindAllFiles(GameDirectoryContentGDI,Directories[i],'*.gdi', False, faAnyFile);
     FileUtil.FindAllFiles(GameDirectoryContentCDI,Directories[i],'*.cdi', False, faAnyFile);
     CurrentSDCardGamesScanActionGameName:=SysUtils.ExtractFileName(Directories[i]);

     if (
           (GameDirectoryContentGDI.Count > 0) or
           (GameDirectoryContentCDI.Count > 0)
        ) and
        (SysUtils.StrToInt(SysUtils.ExtractFileName(Directories[i])) > 1)
     then
     begin
       SetLength(SDCardGamesList,SDCardGamesListCount + 1);
       SDCardGamesList[SDCardGamesListCount]:=TGDEmuGame.Create;
       SDCardGamesList[SDCardGamesListCount].Name:=Trim(GetGameName(Directories[i]));
       SDCardGamesList[SDCardGamesListCount].SlugName:=GetGameSlugName(SDCardGamesList[SDCardGamesListCount].Name);
       SDCardGamesList[SDCardGamesListCount].Path:=Directories[i];
       if GameDirectoryContentGDI.Count > 0 then
       begin
         SDCardGamesList[SDCardGamesListCount].Extension:=SysUtils.ExtractFileExt(GameDirectoryContentGDI[0]);
       end
       else if GameDirectoryContentCDI.Count > 0 then
       begin
         SDCardGamesList[SDCardGamesListCount].Extension:=SysUtils.ExtractFileExt(GameDirectoryContentCDI[0]);
       end;

       SDCardGamesList[SDCardGamesListCount].Index:=SysUtils.StrToInt(SysUtils.ExtractFileName(Directories[i]));

       CacheList.Add(SDCardGamesList[SDCardGamesListCount].SlugName);

       GetMetaFileInfo(SDCardGamesList[SDCardGamesListCount]);
       CreateInfoCacheFile(SDCardGamesList[SDCardGamesListCount]);

       SDCardGamesListCount:=SDCardGamesListCount + 1;
       if SDCardGamesListIndexCount < SDCardGamesList[SDCardGamesListCount -1].Index then
         SDCardGamesListIndexCount:=SDCardGamesList[SDCardGamesListCount -1].Index;
       CacheList.SaveToFile(ConcatPaths([ApplicationPath,'cache','cache.list']));
     end;
     CurrentSDCardGamesScanActionPosition:=CurrentSDCardGamesScanActionPosition + 1;
     GameDirectoryContentGDI.Destroy;
     GameDirectoryContentCDI.Destroy;
   end;
   SDCardLoaded:=True;
   Directories.Destroy;
end;

procedure TGDEmu.UpdateSDCardGameList;
var GamesList: TStringList;
    RegExp: TRegExpr;
    Name: String;
    i: integer;
begin
  //GDI Tools
  RegExp:=TRegExpr.Create('([a-zA-Z0-9]+([a-zA-Z0-9])+)');
  GamesList:=TStringList.Create;
  GamesList.Add('GDEMU');
  for i:=0 to SDCardGamesListCount -1 do
  begin
    if RegExp.Exec(SDCardGamesList[i].Name) then
    begin
      Name:=RegExp.Match[1];
      While RegExp.ExecNext do
      begin
        Name:=Name + ' ' + RegExp.Match[1];
      end;
      SDCardGamesList[i].LegalName:=SysUtils.UpperCase(Name);
      GamesList.Add(SysUtils.UpperCase(Name));
    end;
    UpdateSDCardGameInfo(i);
  end;
  CreateGDEmuImage;
  GamesList.SaveToFile(ConcatPaths([SDCardGamesDirectory,'games_list.txt']));
end;

procedure TGDEmu.UpdateSDCardGameInfo(index: integer);
begin
  if SysUtils.UpperCase(SDCardGamesList[index].Extension) = '.GDI' then
  begin
    if GDIToolsProcess.GetMetaFile(
      ConcatPaths([SDCardGamesList[index].Path,'disc.gdi']),
      ConcatPaths([ApplicationPath,'cache'])
    ) <> '' then
    begin
      SDCardGamesList[index]:=HexDump.GetIPBINInfo(SDCardGamesList[index],ConcatPaths([ApplicationPath,'cache']));
    end;
  end;
end;

procedure TGDEmu.CreateGDEmuImage;
var
    NewGDEmuListIni: TStringList;
    i: integer;
begin
  NewGDEmuListIni:=TStringList.Create;
  NewGDEmuListIni.AddStrings(GDEmuListIni.Text);
  for i:=0 to SDCardGamesListCount -1 do
  begin
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.name=' + SDCardGamesList[i].LegalName);
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.disc=' + SDCardGamesList[i].Disc);
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.vga=' + SDCardGamesList[i].VGA);
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.region=' + SDCardGamesList[i].Region);
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.version=' + SDCardGamesList[i].Version);
    NewGDEmuListIni.Add(Format('%.2d',[SDCardGamesList[i].Index]) + '.date=' + SDCardGamesList[i].Date);
    NewGDEmuListIni.Add('');
  end;
  NewGDEmuListIni.SaveToFile(ConcatPaths([ApplicationPath,'temp','LIST.INI']));
  GenISOImage.GenerateISO(
    ConcatPaths([ApplicationPath,'data']),
    ConcatPaths([ApplicationPath,'temp']),
    ConcatPaths([ApplicationPath,'temp'])
  );
  CDI4DC.ConvertToCDI(
    ConcatPaths([ApplicationPath,'temp','gdmenu.iso']),
    ConcatPaths([ApplicationPath,'temp','gdmenu.cdi'])
  );
  CopyFile(
    ConcatPaths([ApplicationPath,'temp','gdmenu.cdi']),
    ConcatPaths([SDCardGamesDirectory,'01','disc.cdi'])
  );
  CopyFile(
    ConcatPaths([ApplicationPath,'ini','GDEMU.INI']),
    ConcatPaths([SDCardGamesDirectory,'GDEMU.INI'])
  );
  DeleteFile( ConcatPaths([ApplicationPath,'temp','*.*']) );
end;

procedure TGDEmu.ClearSDCardGamesDirectories;
var i: longint;
begin
  if SDCardGamesList <> nil then
  begin
    for i:=0 to SDCardGamesListCount -1 do
    begin
       SDCardGamesList[i].Destroy;
    end;
    SetLength(SDCardGamesList,0);
    SDCardGamesListCount:=0;
  end;
end;

procedure TGDEmu.RemoveFromSDCard;
var i,j: integer;
    Source: String;
    Target: String;
    FoldersIndexCount: integer = 1;
    toDelete: Boolean;
begin
  //Delete Selected Games
  for i:=0 to SelectedSDCardGamesToRemoveListCount -1 do
  begin
    FileUtil.DeleteDirectory(SDCardGamesList[SelectedSDCardGamesToRemoveList[i]].Path,False);
  end;

  //Renaming Non Deleted Games to XX_
  for i:=0 to SDCardGamesListCount -1 do
  begin
    toDelete:=False;
    for j:=0 to SelectedSDCardGamesToRemoveListCount -1 do
    begin
      if SelectedSDCardGamesToRemoveList[j] = i then
      begin
        toDelete:=True;
        Break;
      end;
    end;
    if not toDelete then
      SysUtils.RenameFile(IncludeTrailingPathDelimiter(SDCardGamesList[i].Path),IncludeTrailingPathDelimiter(SDCardGamesList[i].Path + '_'));
  end;
  //Renaming Non Deleted Games to the corret patern
  for i:=0 to SDCardGamesListCount -1 do
  begin
    toDelete:=False;
    for j:=0 to SelectedSDCardGamesToRemoveListCount -1 do
    begin
      if SelectedSDCardGamesToRemoveList[j] = i then
      begin
        toDelete:=True;
        Break;
      end;
    end;
    if not toDelete then
    begin
      FoldersIndexCount:=FoldersIndexCount + 1;
      Source:=IncludeTrailingPathDelimiter(SDCardGamesList[i].Path + '_');
      Target:=IncludeTrailingPathDelimiter(ConcatPaths( [ ExtractFileDir(SDCardGamesList[i].Path), Format('%.2d',[FoldersIndexCount]) ] ));
      SysUtils.RenameFile(Source,Target);
    end;
  end;
  // Rescan SD Card
  ScanSDCardGamesDirectory;
end;

procedure TGDEmu.FixSDCardFolders;
begin

end;

procedure TGDEmu.CopySelectedLocalGamesToSDCard;
var i: integer;
    Source: string;
    Target: string;
    ISOFileNewName: string;
    GameDirectoryContent: TStringList;
    NameTXT: TStringList;
begin
  for i:=0 to SelectedLocalGamesListCount -1 do
  begin
    CurrentCopyToSDCardActionGameName:=LocalGamesList[SelectedLocalGamesList[i]].Name;
    GameDirectoryContent:=TStringList.Create;
    NameTXT:=TStringList.Create;
    // Getting Files/Directories
    Source:=LocalGamesList[SelectedLocalGamesList[i]].Path;
    Target:=ConcatPaths([SDCardGamesDirectory,Format('%.2d',[SDCardGamesListIndexCount + 1])]);
    // Copying Files
    if TestMode = False  then
      FileUtil.CopyDirTree(Source,Target,[TCopyFileFlag.cffCreateDestDirectory, TCopyFileFlag.cffOverwriteFile]);
    SDCardGamesListIndexCount:=SDCardGamesListIndexCount + 1;
    if TestMode = False  then
      FileUtil.FindAllFiles(GameDirectoryContent,Target,'*.gdi;*.cdi', False, faAnyFile);
    if GameDirectoryContent.Count > 0 then
    begin
      ISOFileNewName:=ConcatPaths([ Target,'disc' + SysUtils.LowerCase(SysUtils.ExtractFileExt(GameDirectoryContent[0])) ]);
      if TestMode = False then
        RenameFile(GameDirectoryContent[0],ISOFileNewName);
    end;
    // Creating name.txt
    NameTXT.Add(LocalGamesList[SelectedLocalGamesList[i]].Name);
    if TestMode = False then
      NameTXT.SaveToFile(ConcatPaths([ Target,'name.txt']));
    GameDirectoryContent.Destroy;
    NameTXT.Destroy;
    CurrentCopyToSDCardActionPosition:=CurrentCopyToSDCardActionPosition + 1;
  end;
end;

procedure TGDEmu.StartCopySelectedLocalGamesToSDCard(onCopyFished: PStartCopySelectedLocalGamesToSDCard);
begin
  CurrentAction:='COPYINGTOSDCARD';
  CurrentActionStatus:='PENDING';
  CurrentCopyToSDCardActionPosition:=0;
  CurrentCopyToSDCardActionCount:=SelectedLocalGamesListCount;
  _onFinishGamesCopy:=onCopyFished;
end;

procedure TGDEmu.StartScanLocalGamesDirectories(onLocalGamesScanFinished: PStartScanLocalGamesDirectories);
var i: integer;
    Directories: TStringList;
begin
  Directories:=TStringList.Create;

  CurrentAction:='SCANLOCALGAMESDIRECTORIES';
  CurrentActionStatus:='PENDING';
  CurrentLocalGamesScanActionPosition:=0;
  CurrentLocalGamesScanActionCount:=0;

  for i:=0 to LocalGamesDirectoriesList.Count -1 do
  begin
    Directories.AddStrings(FileUtil.FindAllDirectories(LocalGamesDirectoriesList[i],False));
  end;

  CurrentLocalGamesScanActionCount:=Directories.Count;
  _onFinishLocalGamesScan:=onLocalGamesScanFinished;
  Directories.Destroy;
end;

procedure TGDEmu.StartScanSDCardGamesDirectories(onSDCardGamesScanFinished: PStartScanSDCardGamesDirectories);
var
    Directories: TStringList;
begin
  Directories:=TStringList.Create;

  CurrentAction:='SCANSDCARDGAMESDIRECTORIES';
  CurrentActionStatus:='PENDING';
  CurrentSDCardGamesScanActionPosition:=0;
  CurrentSDCardGamesScanActionCount:=0;

  Directories.AddStrings(FileUtil.FindAllDirectories(SDCardGamesDirectory,False));

  CurrentSDCardGamesScanActionCount:=Directories.Count;
  _onFinishSDCardGamesScan:=onSDCardGamesScanFinished;
  Directories.Destroy;
end;

procedure TGDEmu.ClearSelectedLocalGamesToCopy;
begin
  SelectedLocalGamesListCount:=0;
  SetLength(SelectedLocalGamesList,SelectedLocalGamesListCount);
end;

procedure TGDEmu.ClearSelectedSDCardGamesToRemove;
begin
  SelectedSDCardGamesToRemoveListCount:=0;
  SetLength(SelectedSDCardGamesToRemoveList,SelectedSDCardGamesToRemoveListCount);
end;

procedure TGDEmu.SelectLocalGameToCopy(index: integer);
begin
  SetLength(SelectedLocalGamesList,SelectedLocalGamesListCount + 1);
  SelectedLocalGamesList[SelectedLocalGamesListCount]:=index;
  SelectedLocalGamesListCount:=SelectedLocalGamesListCount + 1;
end;

procedure TGDEmu.SelectSDCardGameToRemove(index: integer);
begin
  SetLength(SelectedSDCardGamesToRemoveList,SelectedSDCardGamesToRemoveListCount + 1);
  SelectedSDCardGamesToRemoveList[SelectedSDCardGamesToRemoveListCount]:=index;
  SelectedSDCardGamesToRemoveListCount:=SelectedSDCardGamesToRemoveListCount + 1;
end;

procedure TGDEmu.UpdateGDEmuINI;
begin

end;

function TGDEmu.GetGameCover(game: TGDEmuGame): String;
var
    imageFile: TStream;
    Document: THTMLDocument;
    Elements: TDOMNodeList;
    i: integer;
    coverCacheImageFilename: String;
    gameDBFileName: String;
    coverFileTest: TPicture;
begin
  coverCacheImageFilename:='';
  coverFileTest:=TPicture.Create;

  if not FileExists(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg'])) then
  begin
    HTTPClient.ConnectTimeout:=1000000;
    HTTPClient.IOTimeout:=100000;
    Document:=THTMLDocument.Create;
    imageFile:=TFileStream.Create(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']) ,fmCreate or fmOpenWrite);
    try
      HTTPClient.AllowRedirect:=true;
      ReadHTMLFile(
        Document,
        TStringStream.Create(
          HTTPClient.Get('https://www.gamesdatabase.org/media/sega-dreamcast/artwork-box/' + game.SlugName)
        )
      );
      Elements:=Document.GetElementsByTagName('img');
      for i:=0 to Elements.Count -1 do
      begin
        if Pos('/Box/',Elements[i].Attributes.GetNamedItem('src').NodeValue) > 0 then
        begin
          gameDBFileName:=UTF8Encode(Elements[i].Attributes.GetNamedItem('src').NodeValue);
          HTTPClient.Get('https://www.gamesdatabase.org' + gameDBFileName,imageFile);
          coverCacheImageFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
        end;
      end;
    except
      coverCacheImageFilename:=ConcatPaths([ApplicationPath,'data','gdrom.png']);
    end;
    imageFile.Free;
  end
  else
  begin
    try
      coverCacheImageFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
      coverFileTest.LoadFromFile(coverCacheImageFilename);
      if coverFileTest.Width + coverFileTest.Height <= 0 then
      begin
        coverFileTest.Free;
        raise Exception.Create('Invalid file');
      end;
    except
      coverCacheImageFilename:=ConcatPaths([ApplicationPath,'data','gdrom.png']);
    end;
  end;

  Result:=coverCacheImageFilename;
end;

procedure TGDEmu.CreateInfoCacheFile(game: TGDEmuGame);
var
    gameCaheInfoFile: TJSONConfig;
    cacheInfoFileName: String;
    cacheInfoFilePath: String;
begin
  cacheInfoFileName:=game.SlugName + '.json';
  cacheInfoFilePath:=ConcatPaths([ApplicationPath,'cache',cacheInfoFileName]);

  if not FileExists(cacheInfoFilePath) then
  begin
    gameCaheInfoFile:=TJSONConfig.Create(Nil);
    try
      gameCaheInfoFile.Filename:=cacheInfoFilePath;
      gameCaheInfoFile.SetValue('name',game.Name);
      gameCaheInfoFile.SetValue('discName',game.DiscName);
      gameCaheInfoFile.SetValue('extension',game.Extension);
      gameCaheInfoFile.SetValue('catalogID',game.CatalogID);
      gameCaheInfoFile.SetValue('data',game.Date);
      gameCaheInfoFile.SetValue('disc',game.Disc);
      gameCaheInfoFile.SetValue('id',game.Id);
      gameCaheInfoFile.SetValue('internalName',game.InternalName);
    finally
      gameCaheInfoFile.Destroy;
    end;
  end;
end;

function TGDEmu.GetMetaFileInfo(game: TGDEmuGame): TGDEmuGame;
var
    sdCardGame: TGDEmuGame;
    gameFilesPaths: TStringList;
    MetaFileFound: Boolean;
begin
  if not FileExists(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.json'])) then
  begin
    gameFilesPaths:=TStringList.Create;
    FileUtil.FindAllFiles(gameFilesPaths,game.Path,'*.gdi;*.cdi',False);
    if SysUtils.LowerCase(game.Extension) = '.gdi' then
    begin
      MetaFileFound:=GDIToolsProcess.GetMetaFile(
        gameFilesPaths[0],
        ConcatPaths([ApplicationPath,'cache'])
      ) <> '';
    end
    else if SysUtils.LowerCase(game.Extension) = '.cdi' then
    begin
      MetaFileFound:=CDIRIP.ExtractIPBIN(
        gameFilesPaths[0],
        ConcatPaths([ApplicationPath,'cache'])
      ) <> '';
    end;

    if (gameFilesPaths.Count > 0) and
    (MetaFileFound) then
    begin
      if SysUtils.LowerCase(game.Extension) = '.gdi' then
      begin
        sdCardGame:=HexDump.GetIPBINInfo(game,ConcatPaths([ApplicationPath,'cache']));
      end
      else if SysUtils.LowerCase(game.Extension) = '.cdi' then
      begin
        sdCardGame:=HexDump.GetIPBINInfo(game,ConcatPaths([ApplicationPath,'cache','cdicache']));
      end;
    end;
  end
  else
  begin
    GetMetaFileInfoCache(game);
  end;
  Result:=sdCardGame;
end;

function TGDEmu.GetMetaFileInfoCache(game: TGDEmuGame): TGDEmuGame;
var
    gameCaheInfoFile: TJSONConfig;
    cacheInfoFileName: String;
    cacheInfoFilePath: String;
begin
  cacheInfoFileName:=game.SlugName + '.json';
  cacheInfoFilePath:=ConcatPaths([ApplicationPath,'cache',cacheInfoFileName]);
  if FileExists(cacheInfoFilePath) then
  begin
    gameCaheInfoFile:=TJSONConfig.Create(Nil);
    try
      gameCaheInfoFile.Filename:=cacheInfoFilePath;
      game.Name:=UTF8Encode(gameCaheInfoFile.GetValue('name',game.Name));
      game.DiscName:=UTF8Encode(gameCaheInfoFile.GetValue('discName', game.DiscName));
      game.Extension:=UTF8Encode(gameCaheInfoFile.GetValue('extension',game.Extension));
      game.CatalogID:=UTF8Encode(gameCaheInfoFile.GetValue('catalogID', game.CatalogID));
      game.Date:=UTF8Encode(gameCaheInfoFile.GetValue('data', game.Date));
      game.Disc:=UTF8Encode(gameCaheInfoFile.GetValue('disc', game.Disc));
      game.Id:=UTF8Encode(gameCaheInfoFile.GetValue('id', game.Id));
      game.InternalName:=UTF8Encode(gameCaheInfoFile.GetValue('internalName', game.InternalName));
    finally
      gameCaheInfoFile.Destroy;
    end;
  end;
  Result:=game;
end;

procedure TGDEmu.CreateOpenBORDisc(name: String; coverImagePath: string; mainPakFilePath: string; otherFilesPaths: TStrings; outputPath: string);
var
    otherFiles: TStringList;
    legalName: String;
    coverImage: TPicture;
begin

  coverImage:=TPicture.Create;
  coverImage.LoadFromFile(coverImagePath);

  legalName:=GetGameLegalName(name);
  CreateDir(ConcatPaths([outputPath,legalName]));

  coverImage.SaveToFile(ConcatPaths([outputPath,legalName,'cover.png']),'png');
  CopyFile(mainPakFilePath,ConcatPaths([ApplicationPath,'temp','BOR.PAK']));

  otherFiles:=TStringList.Create;
  otherFiles.Add(ConcatPaths([ApplicationPath,'temp','BOR.PAK']));
  otherFiles.Add(ConcatPaths([ApplicationPath,'data','openbor','IP.BIN']));
  otherFiles.Add(coverImagePath);
  otherFiles.AddStrings(otherFilesPaths);

  GenISOImage.GenerateISO(
    name,
    ConcatPaths([ApplicationPath,'data','openbor','IP.BIN']),
    ConcatPaths([ApplicationPath,'data','openbor','1ST_READ.BIN']),
    otherFiles,
    ConcatPaths([outputPath,legalName,'disc.iso'])
  );

  CDI4DC.ConvertToCDI(
    ConcatPaths([outputPath,legalName,'disc.iso']),
    ConcatPaths([outputPath,legalName,'disc.cdi'])
  );

  DeleteFile(ConcatPaths([outputPath,legalName,'disc.iso']));
  DeleteFile(ConcatPaths([ApplicationPath,'temp','BOR.PAK']));
end;

end.

