unit gdemuunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, md5, RegExpr, process, fphttpclient,
  openssl, opensslsockets, DOM_HTML, DOM, SAX_HTML, Graphics, Dos, fpjson,
  jsonparser, jsonConf, URIParser;

type

    { Forward declarations }
    TGDEmu = class;

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
      GDEmuInstance: TGDEmu;
    public
      constructor Create;
      procedure SetGDEmuInstance(instance: TGDEmu);
      function GetIPBINInfo(sdCardGame: TGDEmuGame; outputDir: string): TGDEmuGame;
    end;

    { TGDIToolsProcess }

    TGDIToolsProcess = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
      GDEmuInstance: TGDEmu;
    public
      constructor Create;
      procedure SetGDEmuInstance(instance: TGDEmu);
      function GetMetaFile(GamePath: String; outputPath: String): String;
    end;

    { TGenISOImage }

    TGenISOImage = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
      GDEmuInstance: TGDEmu;
    public
      constructor Create;
      procedure SetGDEmuInstance(instance: TGDEmu);
      procedure GenerateISO(dataPath: String; tempPath: String; outputPath: String);
      procedure GenerateISO(name: String; ipBinPath: string; firstStReadBINPath: string; otherFiles: TStrings; outputPath: String);
    end;

    { TCDI4DC }

    TCDI4DC = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
      GDEmuInstance: TGDEmu;
    public
      constructor Create;
      procedure SetGDEmuInstance(instance: TGDEmu);
      procedure ConvertToCDI(inputPath: String; outputPath: String);
    end;

    { TCDIRIP }

    TCDIRIP = class
    private
      ApplicationPath: String;
      ExecutablePath: String;
      Executable: String;
      GDEmuInstance: TGDEmu;
    public
      constructor Create;
      procedure SetGDEmuInstance(instance: TGDEmu);
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
      CommandLog: TStringList;
      procedure UpdateSDCardGameInfo(index: integer);
      procedure AddCommandLog(const command: String; const output: String);
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
      function GetCommandLog: TStringList;
      procedure ClearCommandLog;
    private
      function TryGetCoverFromGamesDatabase(game: TGDEmuGame; searchTerms: TStringList): String;
      function TryGetCoverFromTheGamesDB(game: TGDEmuGame; searchTerms: TStringList): String;
      function TryGetCoverFromScreenScraper(game: TGDEmuGame; searchTerms: TStringList): String;
      function DownloadAndValidateImage(imageUrl: String; cacheFilename: String): Boolean;
      function EncodeURLComponent(const s: String): String;
      function CleanGameNameForSearch(const gameName: String): String;
      function GenerateGamesDatabaseSlug(const gameName: String): String;
    end;

var
  GDEmu: TGDEmu;

function GetGameName(GamePath: String): String;
function GetGameSlugName(name: String): String;
function GetGameLegalName(name: String): String;

implementation

// Helper function to run command with logging
function RunCommandWithLog(const Executable: String; const Params: array of String; out OutputString: ansistring; Options: TProcessOptions = []; SWOptions: TShowWindowOptions = swoNone; GDEmuInstance: TGDEmu = nil): Boolean;
var
  commandLine: String;
  i: integer;
begin
  commandLine:=Executable;
  for i:=0 to High(Params) do
    commandLine:=commandLine + ' ' + Params[i];
  
  Result:=RunCommand(Executable, Params, OutputString, Options, SWOptions);
  
  if GDEmuInstance <> nil then
  begin
    GDEmuInstance.AddCommandLog(commandLine, OutputString);
  end
  else
  begin
    WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', commandLine);
    if OutputString <> '' then
      WriteLn(OutputString);
  end;
end;

{ THexDump }

constructor THexDump.Create;
begin
  ExecutablePath:='';
  Executable:='/usr/bin/hexdump';
  GDEmuInstance:=nil;
end;

procedure THexDump.SetGDEmuInstance(instance: TGDEmu);
begin
  GDEmuInstance:=instance;
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
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //DISC
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //VGA
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //REGION
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //VERSION
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //DATE
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  //CATALOG ID
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  sdCardGame.Id:=MD5Print(MD5File(ConcatPaths([outputDir,'ip.bin'])));
  sdCardGame.Disc:=Trim(disc);
  sdCardGame.VGA:=Trim(vga);
  sdCardGame.Region:=Trim(region);
  sdCardGame.Version:=Trim(version);
  sdCardGame.Date:=Trim(date);
  sdCardGame.InternalName:=Trim(internalName);
  sdCardGame.CatalogID:=catalogID;
  
  // Log para debug
  if Assigned(GDEmuInstance) then
  begin
    GDEmuInstance.AddCommandLog('IP.BIN InternalName extracted', Format('Raw: [%s], Trimmed: [%s]', [internalName, sdCardGame.InternalName]));
  end;
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
  GDEmuInstance:=nil;
end;

procedure TGDIToolsProcess.SetGDEmuInstance(instance: TGDEmu);
begin
  GDEmuInstance:=instance;
end;

function TGDIToolsProcess.GetMetaFile(GamePath: String; outputPath: String): String;
var
  outputString: ansistring;
begin
  RunCommandWithLog(
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
    swoNone,
    GDEmuInstance
  );
  Result:=outputString;
end;

{ TGenISOImage }

constructor TGenISOImage.Create;
begin
  ExecutablePath:='genisoimage';
  Executable:='tools/genisoimage';
  GDEmuInstance:=nil;
end;

procedure TGenISOImage.SetGDEmuInstance(instance: TGDEmu);
begin
  GDEmuInstance:=instance;
end;

procedure TGenISOImage.GenerateISO(dataPath: String; tempPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  //genisoimage -C 0,11702 -V GDMENU -G data/ip.bin -r -J -l -input-charset iso8859-1 -o gdmenu.iso data/1ST_READ.BIN $GDMENU_INI
  RunCommandWithLog(
    ConcatPaths([ApplicationPath, Executable]),
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
    swoNone,
    GDEmuInstance
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
  RunCommandWithLog(
    ConcatPaths([ApplicationPath, Executable]),
    parameters.ToStringArray,
    outputString,
    [poStderrToOutPut,poWaitOnExit],
    swoNone,
    GDEmuInstance
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
  GDEmuInstance:=nil;
end;

procedure TCDI4DC.SetGDEmuInstance(instance: TGDEmu);
begin
  GDEmuInstance:=instance;
end;

procedure TCDI4DC.ConvertToCDI(inputPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  RunCommandWithLog(
    ConcatPaths([ApplicationPath,Executable]),
    [
      inputPath,
      outputPath
    ],
    outputString,
    [poWaitOnExit],
    swoNone,
    GDEmuInstance
  );
end;

{ TCDIRIP }

constructor TCDIRIP.Create;
begin
  ExecutablePath:='';
  Executable:='tools/cdirip';
  GDEmuInstance:=nil;
end;

procedure TCDIRIP.SetGDEmuInstance(instance: TGDEmu);
begin
  GDEmuInstance:=instance;
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
  RunCommandWithLog(
    ConcatPaths([ApplicationPath,Executable]),
    [
      inputPath,
      ConcatPaths([outputPath,'cdicache'])
    ],
    outputString,
    [poWaitOnExit],
    swoNone,
    GDEmuInstance
  );

  FindAllFiles(cacheFiles, ConcatPaths([outputPath,'cdicache']), '*.iso', False, faAnyFile);

  if cacheFiles.Count > 0 then
  begin
    isoFileStream:=TFileStream.Create(cacheFiles[0], fmOpenRead);
    DeleteFile(ConcatPaths([outputPath, 'cdicache', 'ip.bin']));
    ipFileStream:=TFileStream.Create(ConcatPaths([outputPath, 'cdicache', 'ip.bin']), fmCreate);
    FillChar(fileBuffer, SizeOf(fileBuffer), 0);
    isoFileStream.ReadBuffer(fileBuffer, bufferSize);
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
  CommandLog:=TStringList.Create;
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
  GDIToolsProcess.SetGDEmuInstance(Self);
  GenISOImage.ApplicationPath:=ApplicationPath;
  GenISOImage.SetGDEmuInstance(Self);
  HexDump.SetGDEmuInstance(Self);
  CDI4DC.ApplicationPath:=ApplicationPath;
  CDI4DC.SetGDEmuInstance(Self);
  CDIRIP.ApplicationPath:=ApplicationPath;
  CDIRIP.SetGDEmuInstance(Self);
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
    coverCacheImageFilename: String;
    coverFileTest: TPicture;
    coverFileCheck: TFileStream;
    imageFound: Boolean;
    searchVariations: TStringList;
    imageSize: Int64;
    cleanInternalName: String;
    gameWithMetadata: TGDEmuGame;
    cleanedName: String;
begin
  coverCacheImageFilename:='';
  coverFileTest:=TPicture.Create;
  imageFound:=False;
  coverFileCheck:=nil;

  // Verificar se já existe no cache e é válido
  if FileExists(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg'])) then
  begin
    try
      coverFileCheck:=TFileStream.Create(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']), fmOpenRead or fmShareDenyWrite);
      try
        imageSize:=coverFileCheck.Size;
        if imageSize = 0 then
        begin
          DeleteFile(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']));
        end
        else
        begin
          // Verificar se é uma imagem válida
          coverFileCheck.Position:=0;
          coverFileTest.LoadFromStream(coverFileCheck);
          if (coverFileTest.Width > 0) and (coverFileTest.Height > 0) then
          begin
            coverCacheImageFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
            imageFound:=True;
          end
          else
          begin
            DeleteFile(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']));
          end;
        end;
      finally
        if coverFileCheck <> nil then
        begin
          coverFileCheck.Free;
          coverFileCheck:=nil;
        end;
      end;
    except
      // Se houver erro ao ler, deletar e tentar baixar novamente
      if FileExists(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg'])) then
        DeleteFile(ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']));
    end;
  end;

  // Se não encontrou no cache, tentar baixar de múltiplas fontes
  if not imageFound then
  begin
    AddCommandLog('Starting cover search', Format('Game: %s (Slug: %s)', [game.Name, game.SlugName]));
    
    // Configurar HTTP client com timeouts razoáveis e User-Agent
    HTTPClient.ConnectTimeout:=10000; // 10 segundos
    HTTPClient.IOTimeout:=30000; // 30 segundos
    HTTPClient.AllowRedirect:=true;
    HTTPClient.AddHeader('User-Agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36');
    
      // Garantir que temos os metadados do IP.BIN antes de buscar
      gameWithMetadata:=game; // Usar variável local para não modificar o parâmetro
      if (game.InternalName = '') or (game.InternalName = game.Name) then
      begin
        AddCommandLog('Extracting metadata', 'Getting IP.BIN info...');
        try
          gameWithMetadata:=GetMetaFileInfo(game);
          if Assigned(gameWithMetadata) then
          begin
            AddCommandLog('Metadata extracted', Format('InternalName: %s', [gameWithMetadata.InternalName]));
          end
          else
          begin
            AddCommandLog('Metadata extraction failed', 'Returned nil, using original game');
            gameWithMetadata:=game;
          end;
        except
          on E: Exception do
          begin
            AddCommandLog('Error extracting metadata', E.Message);
            gameWithMetadata:=game; // Continuar com o game original se houver erro
          end;
        end;
      end;
      
      // Criar lista de variações para tentar
      // PRIORIDADE: InternalName do IP.BIN é mais confiável que o nome do arquivo
      searchVariations:=TStringList.Create;
      try
        // 1. PRIORIDADE: Nome interno do IP.BIN (mais confiável - nome oficial do jogo)
        if gameWithMetadata.InternalName <> '' then
        begin
          // Limpar o InternalName: remover caracteres de controle, espaços extras, etc
          cleanInternalName:=Trim(gameWithMetadata.InternalName);
          // Remover caracteres de controle (0x00-0x1F) exceto espaço e tab
          cleanInternalName:=ReplaceRegExpr('[\x00-\x08\x0B-\x1F]', cleanInternalName, '');
          // Remover múltiplos espaços
          cleanInternalName:=ReplaceRegExpr('\s+', cleanInternalName, ' ');
          cleanInternalName:=Trim(cleanInternalName);
          
          if cleanInternalName <> '' then
          begin
            searchVariations.Add(cleanInternalName);
            AddCommandLog('Search term added (PRIORITY)', Format('InternalName: %s', [cleanInternalName]));
          end;
        end;
        
        // 2. Nome do jogo limpo (remover versões, anos, publishers, etc.)
        if gameWithMetadata.Name <> '' then
        begin
          cleanedName:=CleanGameNameForSearch(gameWithMetadata.Name);
          if (cleanedName <> '') and 
             (LowerCase(cleanedName) <> LowerCase(gameWithMetadata.InternalName)) and
             (searchVariations.IndexOf(cleanedName) = -1) then
          begin
            searchVariations.Add(cleanedName);
            AddCommandLog('Search term added (cleaned)', Format('Name: %s -> %s', [gameWithMetadata.Name, cleanedName]));
          end;
          
          // Também adicionar o nome original se for diferente
          if (LowerCase(gameWithMetadata.Name) <> LowerCase(gameWithMetadata.InternalName)) and
             (LowerCase(gameWithMetadata.Name) <> LowerCase(cleanedName)) and
             (searchVariations.IndexOf(gameWithMetadata.Name) = -1) then
          begin
            searchVariations.Add(gameWithMetadata.Name);
            AddCommandLog('Search term added', Format('Name: %s', [gameWithMetadata.Name]));
          end;
        end;
        
        // 3. Nome legal (gerado a partir do nome)
        if (gameWithMetadata.LegalName <> '') and 
           (LowerCase(gameWithMetadata.LegalName) <> LowerCase(gameWithMetadata.InternalName)) and
           (LowerCase(gameWithMetadata.LegalName) <> LowerCase(gameWithMetadata.Name)) then
        begin
          searchVariations.Add(gameWithMetadata.LegalName);
          AddCommandLog('Search term added', Format('LegalName: %s', [gameWithMetadata.LegalName]));
        end;
        
        // 4. Slug (última opção - versão formatada do InternalName)
        if (gameWithMetadata.SlugName <> '') and (searchVariations.IndexOf(gameWithMetadata.SlugName) = -1) then
        begin
          searchVariations.Add(gameWithMetadata.SlugName);
          AddCommandLog('Search term added', Format('SlugName: %s', [gameWithMetadata.SlugName]));
        end;

        AddCommandLog('Total search terms', Format('%d variations', [searchVariations.Count]));

        // Usar apenas GamesDatabase.org como fonte
        AddCommandLog('Trying source', 'GamesDatabase.org');
        coverCacheImageFilename:=TryGetCoverFromGamesDatabase(gameWithMetadata, searchVariations);
        if coverCacheImageFilename <> '' then
        begin
          imageFound:=True;
          AddCommandLog('Cover found', 'GamesDatabase.org');
        end;
      
      if not imageFound then
        AddCommandLog('Cover not found', 'All sources exhausted');
      
    finally
      searchVariations.Free;
    end;
  end;

  // Se não encontrou, usar imagem padrão
  if not imageFound then
  begin
    coverCacheImageFilename:=ConcatPaths([ApplicationPath,'data','gdrom.png']);
  end;

  // Limpar recursos
  coverFileTest.Free;

  Result:=coverCacheImageFilename;
end;

function TGDEmu.EncodeURLComponent(const s: String): String;
var
  i: Integer;
  c: Char;
begin
  Result:='';
  for i:=1 to Length(s) do
  begin
    c:=s[i];
    if ((c >= 'A') and (c <= 'Z')) or
       ((c >= 'a') and (c <= 'z')) or
       ((c >= '0') and (c <= '9')) or
       (c = '-') or (c = '_') or (c = '.') or (c = '~') then
      Result:=Result + c
    else
      Result:=Result + '%' + IntToHex(Ord(c), 2);
  end;
end;

function TGDEmu.CleanGameNameForSearch(const gameName: String): String;
var
  cleaned: String;
begin
  cleaned:=Trim(gameName);
  
  // Remover informações entre parênteses: (2000), (Capcom), (NTSC), (US), etc.
  cleaned:=ReplaceRegExpr('\([^)]*\)', cleaned, '');
  
  // Remover informações entre colchetes: [!], [T+], etc.
  cleaned:=ReplaceRegExpr('\[[^\]]*\]', cleaned, '');
  
  // Remover versões: v1.000, v1.0, ver 1.0, etc.
  cleaned:=ReplaceRegExpr('\s+v\d+\.?\d*\s*', cleaned, ' ', False);
  cleaned:=ReplaceRegExpr('\s+ver\s+\d+\.?\d*\s*', cleaned, ' ', False);
  cleaned:=ReplaceRegExpr('\s+version\s+\d+\.?\d*\s*', cleaned, ' ', False);
  
  // Remover anos: 2000, (2000), etc.
  cleaned:=ReplaceRegExpr('\s+\(?\d{4}\)?\s*', cleaned, ' ');
  
  // Remover múltiplos espaços
  cleaned:=ReplaceRegExpr('\s+', cleaned, ' ');
  
  // Remover espaços no início e fim
  cleaned:=Trim(cleaned);
  
  Result:=cleaned;
end;

function TGDEmu.GenerateGamesDatabaseSlug(const gameName: String): String;
var
  cleaned: String;
  i: Integer;
  c: Char;
begin
  // Primeiro limpar o nome
  cleaned:=CleanGameNameForSearch(gameName);
  
  // Converter para minúsculas
  cleaned:=LowerCase(cleaned);
  
  // Converter para slug: remover caracteres especiais, substituir espaços por hífens
  Result:='';
  for i:=1 to Length(cleaned) do
  begin
    c:=cleaned[i];
    if ((c >= 'a') and (c <= 'z')) or
       ((c >= '0') and (c <= '9')) then
      Result:=Result + c
    else if (c = ' ') or (c = '-') then
    begin
      // Adicionar hífen apenas se o último caractere não for hífen (evitar hífens duplos)
      if (Length(Result) = 0) or (Result[Length(Result)] <> '-') then
        Result:=Result + '-';
    end;
    // Ignorar outros caracteres
  end;
  
  // Remover hífens no início e fim
  while (Length(Result) > 0) and (Result[1] = '-') do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = '-') do
    Delete(Result, Length(Result), 1);
  
  // Se ficou vazio, usar o slug original
  if Result = '' then
    Result:=GetGameSlugName(gameName);
end;

function TGDEmu.DownloadAndValidateImage(imageUrl: String; cacheFilename: String): Boolean;
var
  imageFile: TFileStream;
  coverFileTest: TPicture;
  tempFilename: String;
begin
  Result:=False;
  tempFilename:=cacheFilename + '.tmp';
  imageFile:=nil;
  coverFileTest:=TPicture.Create;
  
  try
    try
      AddCommandLog('Downloading cover image', imageUrl);
      
      // Criar arquivo temporário
      imageFile:=TFileStream.Create(tempFilename, fmCreate or fmOpenWrite);
      
      // Baixar imagem
      HTTPClient.Get(imageUrl, imageFile);
      
      AddCommandLog('Image downloaded', Format('Size: %d bytes', [imageFile.Size]));
      
      // Verificar tamanho mínimo
      if imageFile.Size > 1024 then // Pelo menos 1KB
      begin
        imageFile.Free;
        imageFile:=nil;
        
        // Validar se é uma imagem válida
        coverFileTest.LoadFromFile(tempFilename);
        if (coverFileTest.Width > 100) and (coverFileTest.Height > 100) then
        begin
          AddCommandLog('Image validated', Format('Dimensions: %dx%d', [coverFileTest.Width, coverFileTest.Height]));
          // Renomear arquivo temporário para final
          if FileExists(cacheFilename) then
            DeleteFile(cacheFilename);
          RenameFile(tempFilename, cacheFilename);
          Result:=True;
        end
        else
        begin
          AddCommandLog('Image too small', Format('Dimensions: %dx%d', [coverFileTest.Width, coverFileTest.Height]));
          DeleteFile(tempFilename);
        end;
      end
      else
      begin
        AddCommandLog('Image too small', Format('Size: %d bytes', [imageFile.Size]));
        DeleteFile(tempFilename);
      end;
    except
      on E: Exception do
      begin
        AddCommandLog('Error downloading image', E.Message);
        if FileExists(tempFilename) then
          DeleteFile(tempFilename);
        Result:=False;
      end;
    end;
  finally
    if imageFile <> nil then
    begin
      imageFile.Free;
      imageFile:=nil;
    end;
    coverFileTest.Free;
  end;
end;

function TGDEmu.TryGetCoverFromTheGamesDB(game: TGDEmuGame; searchTerms: TStringList): String;
var
  jsonRoot: TJSONData;
  jsonData: TJSONData;
  jsonObject: TJSONObject;
  jsonArray: TJSONArray;
  jsonItem: TJSONObject;
  jsonImages: TJSONData;
  jsonBoxart: TJSONData;
  i, j: integer;
  searchUrl: String;
  response: String;
  imageUrl: String;
  cacheFilename: String;
  gameName: String;
  gameId: String;
begin
  Result:='';
  
  // TheGamesDB.net API v2 - Busca por nome
  // API: https://api.thegamesdb.net/v1/Games/ByGameName?apikey=1&name={name}&platform=sega-dreamcast
  // Não requer API key para uso básico
  
  for j:=0 to searchTerms.Count -1 do
  begin
    if Result <> '' then Break;
    
    gameName:=searchTerms[j];
    AddCommandLog('Searching TheGamesDB', Format('Game: %s', [gameName]));
    
    // URL encode adequado
    gameName:=EncodeURLComponent(gameName);
    
    searchUrl:='https://api.thegamesdb.net/v1/Games/ByGameName?apikey=1&name=' + gameName + '&platform=sega-dreamcast';
    AddCommandLog('TheGamesDB URL', searchUrl);
    jsonRoot:=nil;
    
    try
      HTTPClient.AddHeader('Accept', 'application/json');
      response:=HTTPClient.Get(searchUrl);
      
      AddCommandLog('TheGamesDB Response', Format('Length: %d chars', [Length(response)]));
      
      if response <> '' then
      begin
        jsonRoot:=GetJSON(response);
        try
          if jsonRoot is TJSONObject then
          begin
            jsonObject:=TJSONObject(jsonRoot);
            
            // Verificar se há dados
            if jsonObject.Find('data', jsonData) and (jsonData is TJSONObject) then
            begin
              jsonObject:=TJSONObject(jsonData);
              
              // Verificar se há games
              if jsonObject.Find('games', jsonData) and (jsonData is TJSONArray) then
              begin
                jsonArray:=TJSONArray(jsonData);
                
                AddCommandLog('TheGamesDB Results', Format('Found %d games', [jsonArray.Count]));
                
                // Pegar o primeiro resultado
                if jsonArray.Count > 0 then
                begin
                  jsonItem:=TJSONObject(jsonArray[0]);
                  
                  // Pegar ID do jogo
                  if jsonItem.Find('id', jsonData) then
                  begin
                    gameId:=jsonData.AsString;
                    AddCommandLog('TheGamesDB Game ID', gameId);
                    
                    // Buscar imagens do jogo
                    // API: https://api.thegamesdb.net/v1/Games/Images?apikey=1&games_id={id}
                    searchUrl:='https://api.thegamesdb.net/v1/Games/Images?apikey=1&games_id=' + gameId;
                    response:=HTTPClient.Get(searchUrl);
                    
                    AddCommandLog('TheGamesDB Images Response', Format('Length: %d chars', [Length(response)]));
                    
                    if response <> '' then
                    begin
                      // Liberar JSON anterior
                      jsonRoot.Free;
                      jsonRoot:=GetJSON(response);
                      
                      if jsonRoot is TJSONObject then
                      begin
                        jsonObject:=TJSONObject(jsonRoot);
                        
                        if jsonObject.Find('data', jsonData) and (jsonData is TJSONObject) then
                        begin
                          jsonObject:=TJSONObject(jsonData);
                          
                          // Procurar por imagens do tipo "boxart" ou "cover"
                          if jsonObject.Find('images', jsonImages) and (jsonImages is TJSONObject) then
                          begin
                            jsonObject:=TJSONObject(jsonImages);
                            
                            // Procurar por boxart
                            if jsonObject.Find('boxart', jsonBoxart) and (jsonBoxart is TJSONObject) then
                            begin
                              jsonObject:=TJSONObject(jsonBoxart);
                              
                              // Pegar a primeira imagem original
                              for i:=0 to jsonObject.Count -1 do
                              begin
                                if jsonObject.Items[i] is TJSONObject then
                                begin
                                  jsonItem:=TJSONObject(jsonObject.Items[i]);
                                  
                                  // Procurar por "original" ou a maior resolução
                                  if jsonItem.Find('original', jsonData) then
                                  begin
                                    imageUrl:=jsonData.AsString;
                                    
                                    // Construir URL completa se for relativa
                                    if Pos('http', imageUrl) = 0 then
                                      imageUrl:='https://cdn.thegamesdb.net/images/original/' + imageUrl;
                                    
                                    cacheFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
                                    
                                    if DownloadAndValidateImage(imageUrl, cacheFilename) then
                                    begin
                                      Result:=cacheFilename;
                                      Break;
                                    end;
                                  end;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          if jsonRoot <> nil then
          begin
            jsonRoot.Free;
            jsonRoot:=nil;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        AddCommandLog('TheGamesDB Error', E.Message);
        if jsonRoot <> nil then
        begin
          jsonRoot.Free;
          jsonRoot:=nil;
        end;
        Result:='';
      end;
    end;
  end;
end;

function TGDEmu.TryGetCoverFromScreenScraper(game: TGDEmuGame; searchTerms: TStringList): String;
var
  jsonRoot: TJSONData;
  jsonData: TJSONData;
  jsonObject: TJSONObject;
  jsonResponse: TJSONObject;
  jsonJeu: TJSONData;
  jsonMedias: TJSONData;
  jsonBox2D: TJSONData;
  jsonBoxItem: TJSONObject;
  j: integer;
  searchUrl: String;
  response: String;
  imageUrl: String;
  cacheFilename: String;
  gameName: String;
  romName: String;
begin
  Result:='';
  
  // ScreenScraper.fr API
  // API: https://www.screenscraper.fr/api2/jeuInfos.php?devid={devid}&devpassword={password}&softname={softname}&output=json&ssid={ssid}&sspassword={sspassword}&crc={crc}&romnom={romname}&systemeid=23
  // Requer registro para uso completo, mas tem muitas capas do Dreamcast
  // Por enquanto, vamos usar busca por nome (sem autenticação - limitada)
  
  for j:=0 to searchTerms.Count -1 do
  begin
    if Result <> '' then Break;
    
    gameName:=searchTerms[j];
    romName:=gameName;
    
    // ScreenScraper usa o nome da ROM
    // Tentar com o nome interno se disponível
    if game.InternalName <> '' then
      romName:=GetGameSlugName(game.InternalName);
    
    AddCommandLog('Searching ScreenScraper', Format('ROM: %s', [romName]));
    
    // URL encode adequado
    romName:=EncodeURLComponent(romName);
    
    // Busca básica (sem autenticação - limitada)
    searchUrl:='https://www.screenscraper.fr/api2/jeuInfos.php?output=json&systemeid=23&romnom=' + romName;
    AddCommandLog('ScreenScraper URL', searchUrl);
    jsonRoot:=nil;
    
    try
      HTTPClient.AddHeader('Accept', 'application/json');
      response:=HTTPClient.Get(searchUrl);
      
      AddCommandLog('ScreenScraper Response', Format('Length: %d chars', [Length(response)]));
      
      if response <> '' then
      begin
        jsonRoot:=GetJSON(response);
        try
          if jsonRoot is TJSONObject then
          begin
            jsonResponse:=TJSONObject(jsonRoot);
            
            // Verificar se há resposta válida
            if jsonResponse.Find('response', jsonData) and (jsonData is TJSONObject) then
            begin
              jsonObject:=TJSONObject(jsonData);
              
              // Verificar se há jogo encontrado
              if jsonObject.Find('jeu', jsonJeu) and (jsonJeu is TJSONObject) then
              begin
                jsonObject:=TJSONObject(jsonJeu);
                
                // Procurar por media
                if jsonObject.Find('medias', jsonMedias) and (jsonMedias is TJSONObject) then
                begin
                  jsonObject:=TJSONObject(jsonMedias);
                  
                  // Procurar por "box-2D" ou "box-3D" (capas)
                  if jsonObject.Find('box-2D', jsonBox2D) then
                  begin
                    if jsonBox2D is TJSONArray then
                    begin
                      if TJSONArray(jsonBox2D).Count > 0 then
                      begin
                        if TJSONArray(jsonBox2D).Items[0] is TJSONObject then
                        begin
                          jsonBoxItem:=TJSONObject(TJSONArray(jsonBox2D).Items[0]);
                          if jsonBoxItem.Find('url', jsonData) then
                          begin
                            imageUrl:=jsonData.AsString;
                            
                            // Construir URL completa se for relativa
                            if Pos('http', imageUrl) = 0 then
                              imageUrl:='https://www.screenscraper.fr' + imageUrl;
                            
                            cacheFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
                            
                            if DownloadAndValidateImage(imageUrl, cacheFilename) then
                            begin
                              Result:=cacheFilename;
                              Break;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          if jsonRoot <> nil then
          begin
            jsonRoot.Free;
            jsonRoot:=nil;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        AddCommandLog('ScreenScraper Error', E.Message);
        if jsonRoot <> nil then
        begin
          jsonRoot.Free;
          jsonRoot:=nil;
        end;
        Result:='';
      end;
    end;
  end;
end;

function TGDEmu.TryGetCoverFromGamesDatabase(game: TGDEmuGame; searchTerms: TStringList): String;
var
  Document: THTMLDocument;
  Elements: TDOMNodeList;
  htmlContent: TStringStream;
  i, j, k: integer;
  searchUrl: String;
  gameDBFileName: String;
  cacheFilename: String;
  searchText: String;
  linkElement: TDOMElement;
  href: String;
begin
  Result:='';
  
  // Usar a página de busca do GamesDatabase.org
  // URL: https://www.gamesdatabase.org/list.aspx?in=1&searchtext={nome}&searchtype=1
  for j:=0 to searchTerms.Count -1 do
  begin
    if Result <> '' then Break;
    
    // Usar o nome limpo para busca
    searchText:=CleanGameNameForSearch(searchTerms[j]);
    AddCommandLog('Searching GamesDatabase', Format('Term: %s -> Cleaned: %s', [searchTerms[j], searchText]));
    
    // URL encode do texto de busca
    searchText:=EncodeURLComponent(searchText);
    searchUrl:='https://www.gamesdatabase.org/list.aspx?in=1&searchtext=' + searchText + '&searchtype=1';
    AddCommandLog('GamesDatabase Search URL', searchUrl);
    Document:=nil;
    htmlContent:=nil;
    
    try
      htmlContent:=TStringStream.Create(HTTPClient.Get(searchUrl));
      AddCommandLog('GamesDatabase Response', Format('Length: %d chars', [htmlContent.Size]));
      Document:=THTMLDocument.Create;
      ReadHTMLFile(Document, htmlContent);
      
      // Procurar por links de jogos na página de resultados
      Elements:=Document.GetElementsByTagName('a');
      AddCommandLog('Search results', Format('Found %d links', [Elements.Count]));
      
      for i:=0 to Elements.Count -1 do
      begin
        if Result <> '' then Break;
        
        if Elements[i] is TDOMElement then
        begin
          linkElement:=TDOMElement(Elements[i]);
          href:=UTF8Encode(linkElement.GetAttribute('href'));
          
          // Procurar por links que apontam para páginas de jogos
          // GamesDatabase.org usa URLs como: /game/... ou /media/...
          if (href <> '') and ((Pos('/game/', href) > 0) or (Pos('/media/', href) > 0) or (Pos('game.aspx', href) > 0)) then
          begin
            // Construir URL completa
            if Pos('http', href) = 0 then
            begin
              if href[1] = '/' then
                href:='https://www.gamesdatabase.org' + href
              else
                href:='https://www.gamesdatabase.org/' + href;
            end;
            
            AddCommandLog('Found game link', href);
            
            // Acessar a página do jogo para buscar imagens
            try
              htmlContent.Free;
              htmlContent:=nil;
              htmlContent:=TStringStream.Create(HTTPClient.Get(href));
              
              Document.Free;
              Document:=nil;
              Document:=THTMLDocument.Create;
              ReadHTMLFile(Document, htmlContent);
              
              // Procurar por imagens na página do jogo
              Elements:=Document.GetElementsByTagName('img');
              for k:=0 to Elements.Count -1 do
              begin
                if Elements[k].Attributes.GetNamedItem('src') <> nil then
                begin
                  gameDBFileName:=UTF8Encode(Elements[k].Attributes.GetNamedItem('src').NodeValue);
                  
                  // Procurar por imagens de capa/boxart
                  if (Pos('/Box/',gameDBFileName) > 0) or 
                     (Pos('/box/',LowerCase(gameDBFileName)) > 0) or
                     (Pos('cover',LowerCase(gameDBFileName)) > 0) or
                     (Pos('artwork',LowerCase(gameDBFileName)) > 0) or
                     (Pos('boxart',LowerCase(gameDBFileName)) > 0) then
                  begin
                    if Pos('http', gameDBFileName) = 0 then
                    begin
                      if gameDBFileName[1] = '/' then
                        gameDBFileName:='https://www.gamesdatabase.org' + gameDBFileName
                      else
                        gameDBFileName:='https://www.gamesdatabase.org/' + gameDBFileName;
                    end;
                    
                    AddCommandLog('Found cover image', gameDBFileName);
                    cacheFilename:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
                    
                    if DownloadAndValidateImage(gameDBFileName, cacheFilename) then
                    begin
                      Result:=cacheFilename;
                      Break;
                    end;
                  end;
                end;
              end;
              
              // Se encontrou resultado, parar de procurar
              if Result <> '' then
                Break;
            except
              on E: Exception do
              begin
                AddCommandLog('Error accessing game page', Format('%s: %s', [E.ClassName, E.Message]));
                // Continuar para próximo link
              end;
            end;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        AddCommandLog('GamesDatabase Error', Format('%s: %s', [E.ClassName, E.Message]));
        Result:='';
      end;
    end;
    
    // Limpar recursos
    if Document <> nil then
    begin
      Document.Free;
      Document:=nil;
    end;
    if htmlContent <> nil then
    begin
      htmlContent.Free;
      htmlContent:=nil;
    end;
  end;
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
    ipBinPath: String;
    cacheJsonPath: String;
    ipBinFile: TFileStream;
begin
  // Inicializar com o game passado como parâmetro (fallback seguro)
  Result:=game;
  sdCardGame:=game;
  
  cacheJsonPath:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.json']);
  AddCommandLog('Checking cache', Format('Cache file: %s', [cacheJsonPath]));
  
  if not FileExists(cacheJsonPath) then
  begin
    AddCommandLog('Cache not found', 'Extracting IP.BIN...');
    gameFilesPaths:=TStringList.Create;
    try
      AddCommandLog('Searching game files', Format('Path: %s, Extension: %s', [game.Path, game.Extension]));
      FileUtil.FindAllFiles(gameFilesPaths,game.Path,'*.gdi;*.cdi',False);
      AddCommandLog('Game files found', Format('%d files', [gameFilesPaths.Count]));
      
      if gameFilesPaths.Count > 0 then
      begin
        AddCommandLog('Game file', gameFilesPaths[0]);
        
        if SysUtils.LowerCase(game.Extension) = '.gdi' then
        begin
          AddCommandLog('Extracting IP.BIN (GDI)', 'Using GDIToolsProcess...');
          MetaFileFound:=GDIToolsProcess.GetMetaFile(
            gameFilesPaths[0],
            ConcatPaths([ApplicationPath,'cache'])
          ) <> '';
          ipBinPath:=ConcatPaths([ApplicationPath,'cache','ip.bin']);
        end
        else if SysUtils.LowerCase(game.Extension) = '.cdi' then
        begin
          AddCommandLog('Extracting IP.BIN (CDI)', 'Using CDIRIP...');
          MetaFileFound:=CDIRIP.ExtractIPBIN(
            gameFilesPaths[0],
            ConcatPaths([ApplicationPath,'cache'])
          ) <> '';
          ipBinPath:=ConcatPaths([ApplicationPath,'cache','cdicache','ip.bin']);
        end
        else
        begin
          MetaFileFound:=False;
          AddCommandLog('Unknown extension', Format('Extension: %s', [game.Extension]));
        end;

        AddCommandLog('IP.BIN extraction result', Format('MetaFileFound: %s, IP.BIN path: %s', [BoolToStr(MetaFileFound, True), ipBinPath]));

        if MetaFileFound then
        begin
          // Verificar se o IP.BIN realmente existe
          if FileExists(ipBinPath) then
          begin
            ipBinFile:=nil;
            try
              ipBinFile:=TFileStream.Create(ipBinPath, fmOpenRead);
              AddCommandLog('IP.BIN file exists', Format('Size: %d bytes', [ipBinFile.Size]));
              ipBinFile.Free;
              ipBinFile:=nil;
            except
              on E: Exception do
              begin
                AddCommandLog('IP.BIN file exists', Format('Could not read file size: %s', [E.Message]));
                if ipBinFile <> nil then
                begin
                  ipBinFile.Free;
                  ipBinFile:=nil;
                end;
              end;
            end;
            try
              AddCommandLog('Calling GetIPBINInfo', 'Extracting metadata from IP.BIN...');
              if SysUtils.LowerCase(game.Extension) = '.gdi' then
              begin
                sdCardGame:=HexDump.GetIPBINInfo(game,ConcatPaths([ApplicationPath,'cache']));
                Result:=sdCardGame;
                AddCommandLog('IP.BIN info extracted (GDI)', Format('InternalName: [%s], Length: %d', [Result.InternalName, Length(Result.InternalName)]));
              end
              else if SysUtils.LowerCase(game.Extension) = '.cdi' then
              begin
                sdCardGame:=HexDump.GetIPBINInfo(game,ConcatPaths([ApplicationPath,'cache','cdicache']));
                Result:=sdCardGame;
                AddCommandLog('IP.BIN info extracted (CDI)', Format('InternalName: [%s], Length: %d', [Result.InternalName, Length(Result.InternalName)]));
              end;
            except
              on E: Exception do
              begin
                AddCommandLog('Error extracting IP.BIN info', Format('%s: %s', [E.ClassName, E.Message]));
                Result:=game; // Retornar o game original em caso de erro
              end;
            end;
          end
          else
          begin
            AddCommandLog('IP.BIN file not found', Format('Expected at: %s', [ipBinPath]));
          end;
        end
        else
        begin
          AddCommandLog('IP.BIN extraction failed', 'MetaFile extraction returned empty');
        end;
      end
      else
      begin
        AddCommandLog('No game files found', Format('Searched in: %s', [game.Path]));
      end;
    finally
      gameFilesPaths.Free;
    end;
  end
  else
  begin
    // Cache existe, carregar do cache
    AddCommandLog('Loading from cache', cacheJsonPath);
    try
      Result:=GetMetaFileInfoCache(game);
      AddCommandLog('Cache loaded', Format('InternalName: [%s], Length: %d', [Result.InternalName, Length(Result.InternalName)]));
      
      // Se o InternalName no cache está vazio, forçar re-extração
      if Result.InternalName = '' then
      begin
        AddCommandLog('Cache has empty InternalName', 'Forcing re-extraction...');
        // Deletar cache e tentar extrair novamente
        DeleteFile(cacheJsonPath);
        // Recursivamente chamar novamente (agora vai extrair)
        Result:=GetMetaFileInfo(game);
      end;
    except
      on E: Exception do
      begin
        AddCommandLog('Error loading from cache', Format('%s: %s', [E.ClassName, E.Message]));
        Result:=game; // Retornar o game original em caso de erro
      end;
    end;
  end;
end;

function TGDEmu.GetMetaFileInfoCache(game: TGDEmuGame): TGDEmuGame;
var
    gameCaheInfoFile: TJSONConfig;
    cacheInfoFileName: String;
    cacheInfoFilePath: String;
    cachedInternalName: String;
begin
  cacheInfoFileName:=game.SlugName + '.json';
  cacheInfoFilePath:=ConcatPaths([ApplicationPath,'cache',cacheInfoFileName]);
  AddCommandLog('Loading cache', Format('File: %s', [cacheInfoFilePath]));
  
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
      cachedInternalName:=UTF8Encode(gameCaheInfoFile.GetValue('internalName', ''));
      game.InternalName:=cachedInternalName;
      
      AddCommandLog('Cache loaded', Format('InternalName from cache: [%s], Length: %d', [cachedInternalName, Length(cachedInternalName)]));
      
      // Se o InternalName no cache está vazio, pode ser que o cache foi criado antes da extração
      // Nesse caso, vamos forçar uma nova extração
      if cachedInternalName = '' then
      begin
        AddCommandLog('Cache has empty InternalName', 'Will force re-extraction');
      end;
    finally
      gameCaheInfoFile.Destroy;
    end;
  end
  else
  begin
    AddCommandLog('Cache file not found', cacheInfoFilePath);
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

procedure TGDEmu.AddCommandLog(const command: String; const output: String);
var
  logEntry: String;
begin
  logEntry:=Format('[%s] %s', [FormatDateTime('hh:nn:ss', Now), command]);
  if output <> '' then
    logEntry:=logEntry + LineEnding + output;
  CommandLog.Add(logEntry);
  CommandLog.Add('---');
  // Também escreve no console se disponível
  WriteLn(logEntry);
end;

function TGDEmu.GetCommandLog: TStringList;
begin
  Result:=CommandLog;
end;

procedure TGDEmu.ClearCommandLog;
begin
  CommandLog.Clear;
end;

end.

