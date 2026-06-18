unit externaltools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, process, md5, gamemodel;

type

  // Callback used by the tool wrappers to report a command and its output back
  // to the engine's command log. Matches TGDEmu.AddCommandLog, so the engine
  // wires itself in without ExternalTools depending on the engine type.
  TCommandLogger = procedure(const command, output: String) of object;

  { THexDump — reads metadata fields out of an extracted IP.BIN. }
  THexDump = class
  private
    Logger: TCommandLogger;
  public
    constructor Create;
    procedure SetLogger(ALogger: TCommandLogger);
    function GetIPBINInfo(sdCardGame: TGDEmuGame; outputDir: string): TGDEmuGame;
  end;

  { TGDIToolsProcess — extracts the boot sector (IP.BIN) from a .gdi dump. }
  TGDIToolsProcess = class
  private
    Logger: TCommandLogger;
  public
    ApplicationPath: String;
    constructor Create;
    procedure SetLogger(ALogger: TCommandLogger);
    function GetMetaFile(GamePath: String; outputPath: String): String;
  end;

  { TGenISOImage — wraps genisoimage to build the GDMenu ISO. }
  TGenISOImage = class
  private
    ExecutablePath: String;
    Logger: TCommandLogger;
  public
    ApplicationPath: String;
    constructor Create;
    procedure SetLogger(ALogger: TCommandLogger);
    procedure GenerateISO(dataPath: String; tempPath: String; outputPath: String);
    procedure GenerateISO(name: String; ipBinPath: string; firstStReadBINPath: string; otherFiles: TStrings; outputPath: String);
  end;

  { TCDI4DC — wraps cdi4dc to convert an ISO into a bootable CDI. }
  TCDI4DC = class
  private
    ExecutablePath: String;
    Logger: TCommandLogger;
  public
    ApplicationPath: String;
    constructor Create;
    procedure SetLogger(ALogger: TCommandLogger);
    procedure ConvertToCDI(inputPath: String; outputPath: String);
  end;

  { TCDIRIP — wraps cdirip to pull the boot sector out of a .cdi image. }
  TCDIRIP = class
  private
    ExecutablePath: String;
    Logger: TCommandLogger;
  public
    ApplicationPath: String;
    constructor Create;
    procedure SetLogger(ALogger: TCommandLogger);
    function ExtractIPBIN(inputPath: String; outputPath: String): String;
  end;

// Run an external command, capturing its output and reporting it to the logger
// (or stdout when no logger is set).
function RunCommandWithLog(const Executable: String; const Params: array of String; out OutputString: ansistring; Options: TProcessOptions = []; SWOptions: TShowWindowOptions = swoNone; Logger: TCommandLogger = nil): Boolean;
// Resolve an external helper tool to a runnable path (bundled tools/ then PATH).
function ResolveToolPath(const AppPath, ToolName: String): String;
// Read a raw byte range from a file as a string (1 byte -> 1 char).
function ReadFileByteRange(const FilePath: String; Offset, Len: Int64): ansistring;
// Extract the Dreamcast boot sector (IP.BIN) natively from a .gdi dump.
function ExtractGDIBootSector(const GdiPath, OutIpBin: String): Boolean;

implementation

function RunCommandWithLog(const Executable: String; const Params: array of String; out OutputString: ansistring; Options: TProcessOptions = []; SWOptions: TShowWindowOptions = swoNone; Logger: TCommandLogger = nil): Boolean;
var
  commandLine: String;
  i: integer;
begin
  commandLine:=Executable;
  for i:=0 to High(Params) do
    commandLine:=commandLine + ' ' + Params[i];

  Result:=RunCommand(Executable, Params, OutputString, Options, SWOptions);

  if Assigned(Logger) then
    Logger(commandLine, OutputString)
  else
  begin
    WriteLn('[', FormatDateTime('hh:nn:ss', Now), '] ', commandLine);
    if OutputString <> '' then
      WriteLn(OutputString);
  end;
end;

// Single point of OS abstraction for invoking bundled binaries: it prefers the
// binary shipped under tools/ and falls back to whatever is on the system PATH.
// On Windows it appends the .exe suffix when missing. '' if nothing is found.
function ResolveToolPath(const AppPath, ToolName: String): String;
var
  exeName, bundled: String;
begin
  exeName:=ToolName;
  {$IFDEF WINDOWS}
  if ExtractFileExt(exeName) = '' then
    exeName:=exeName + '.exe';
  {$ENDIF}
  // 1. Bundled binary under tools/ (relative to the application directory).
  bundled:=ConcatPaths([AppPath, 'tools', exeName]);
  if FileExists(bundled) then
    Exit(bundled);
  // 2. Anything matching on the system PATH.
  Result:=FindDefaultExecutablePath(exeName);
end;

// Replaces shelling out to `hexdump -e '"%c"' -s OFFSET -n LEN` for IP.BIN fields.
function ReadFileByteRange(const FilePath: String; Offset, Len: Int64): ansistring;
var
  fs: TFileStream;
  buf: array of Byte;
  bytesRead: LongInt;
begin
  Result:='';
  if (Len <= 0) or (not FileExists(FilePath)) then
    Exit;
  fs:=TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    if Offset >= fs.Size then
      Exit;
    if Offset + Len > fs.Size then
      Len:=fs.Size - Offset;
    SetLength(buf, Len);
    fs.Position:=Offset;
    bytesRead:=fs.Read(buf[0], Len);
    if bytesRead > 0 then
      SetString(Result, PAnsiChar(@buf[0]), bytesRead);
  finally
    fs.Free;
  end;
end;

// IP.BIN is the first 16 user-data sectors (16 * 2048 = 32768 bytes) of the
// high-density data track, which by the GD-ROM spec starts at LBA 45000.
// This replaces the former gditools.py/Python 2 dependency.
function ExtractGDIBootSector(const GdiPath, OutIpBin: String): Boolean;
const
  BOOT_SECTORS = 16;
  USER_BYTES   = 2048;
var
  lines, parts: TStringList;
  gdiDir, trackFile, rawName: String;
  i, lba, sectorSize, userOffset, sector: Integer;
  found: Boolean;
  src, dst: TFileStream;
  buf: array of Byte;
begin
  Result:=False;
  if not FileExists(GdiPath) then
    Exit;
  gdiDir:=ExtractFileDir(GdiPath);

  found:=False;
  trackFile:='';
  sectorSize:=0;
  lines:=TStringList.Create;
  parts:=TStringList.Create;
  try
    lines.LoadFromFile(GdiPath);
    parts.Delimiter:=' ';
    parts.QuoteChar:='"';
    parts.StrictDelimiter:=False; // also split on tabs; merge runs of whitespace
    // Track line format: tnum lba type sectorsize filename offset
    for i:=0 to lines.Count -1 do
    begin
      if Trim(lines[i]) = '' then
        Continue;
      parts.DelimitedText:=Trim(lines[i]);
      if parts.Count < 5 then
        Continue; // header line (track count) or malformed
      lba:=StrToIntDef(parts[1], -1);
      if lba = 45000 then
      begin
        sectorSize:=StrToIntDef(parts[3], 2352);
        rawName:=StringReplace(parts[4], '\', PathDelim, [rfReplaceAll]);
        trackFile:=ConcatPaths([gdiDir, rawName]);
        found:=True;
        Break;
      end;
    end;
  finally
    parts.Free;
    lines.Free;
  end;

  if (not found) or (not FileExists(trackFile)) then
    Exit;

  // Offset of the 2048-byte user payload within each raw sector
  case sectorSize of
    2352: userOffset:=16; // Mode 1: 12 sync + 4 header
    2336: userOffset:=8;  // Mode 2
    2048: userOffset:=0;  // already cooked
  else
    Exit;                 // unsupported sector size
  end;

  SetLength(buf, USER_BYTES);
  src:=nil;
  dst:=nil;
  try
    src:=TFileStream.Create(trackFile, fmOpenRead or fmShareDenyWrite);
    if src.Size < Int64(BOOT_SECTORS) * sectorSize then
      Exit;
    dst:=TFileStream.Create(OutIpBin, fmCreate);
    for sector:=0 to BOOT_SECTORS -1 do
    begin
      src.Position:=Int64(sector) * sectorSize + userOffset;
      if src.Read(buf[0], USER_BYTES) <> USER_BYTES then
        Exit;
      dst.WriteBuffer(buf[0], USER_BYTES);
    end;
    Result:=True;
  finally
    if Assigned(dst) then
      dst.Free;
    if Assigned(src) then
      src.Free;
  end;
end;

{ THexDump }

constructor THexDump.Create;
begin
  // IP.BIN fields are read directly (see ReadFileByteRange); no external
  // hexdump process is spawned.
  Logger:=nil;
end;

procedure THexDump.SetLogger(ALogger: TCommandLogger);
begin
  Logger:=ALogger;
end;

function THexDump.GetIPBINInfo(sdCardGame: TGDEmuGame; outputDir: string): TGDEmuGame;
var
  ipBinPath: String;
  internalName: ansistring;
  disc: ansistring;
  vga: ansistring;
  region: ansistring;
  version: ansistring;
  date: ansistring;
  catalogID: ansistring;
begin
  // IP.BIN bootstrap header has fixed offsets, so read the fields directly
  // instead of spawning `hexdump` once per field (7 processes per game).
  ipBinPath:=ConcatPaths([outputDir,'ip.bin']);
  internalName:=ReadFileByteRange(ipBinPath, $80, 128); // software/game name
  disc:=ReadFileByteRange(ipBinPath, $2B, 3);            // disc number e.g. "1/1"
  vga:=ReadFileByteRange(ipBinPath, $3D, 1);             // VGA support flag
  region:=ReadFileByteRange(ipBinPath, $30, 8);          // area symbols (J/U/E)
  version:=ReadFileByteRange(ipBinPath, $4A, 6);         // product version
  date:=ReadFileByteRange(ipBinPath, $50, 8);            // release date YYYYMMDD
  catalogID:=ReadFileByteRange(ipBinPath, $40, 8);       // product/catalog number
  sdCardGame.Id:=MD5Print(MD5File(ipBinPath));
  sdCardGame.Disc:=Trim(disc);
  sdCardGame.VGA:=Trim(vga);
  sdCardGame.Region:=Trim(region);
  sdCardGame.Version:=Trim(version);
  sdCardGame.Date:=Trim(date);
  sdCardGame.InternalName:=Trim(internalName);
  sdCardGame.CatalogID:=catalogID;

  if Assigned(Logger) then
    Logger('IP.BIN InternalName extracted', Format('Raw: [%s], Trimmed: [%s]', [internalName, sdCardGame.InternalName]));
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
  // IP.BIN is extracted natively (see ExtractGDIBootSector); no Python is run.
  Logger:=nil;
end;

procedure TGDIToolsProcess.SetLogger(ALogger: TCommandLogger);
begin
  Logger:=ALogger;
end;

function TGDIToolsProcess.GetMetaFile(GamePath: String; outputPath: String): String;
var
  ipBinPath: String;
begin
  Result:='';
  ipBinPath:=ConcatPaths([outputPath,'ip.bin']);
  if ExtractGDIBootSector(GamePath, ipBinPath) then
  begin
    if Assigned(Logger) then
      Logger('IP.BIN extracted (native GDI)', ipBinPath);
    Result:=ipBinPath;
  end
  else if Assigned(Logger) then
    Logger('IP.BIN extraction failed (native GDI)', GamePath);
end;

{ TGenISOImage }

constructor TGenISOImage.Create;
begin
  // ExecutablePath is the bare tool name; ResolveToolPath maps it to a runnable
  // path (bundled tools/ first, then system PATH).
  ExecutablePath:='genisoimage';
  Logger:=nil;
end;

procedure TGenISOImage.SetLogger(ALogger: TCommandLogger);
begin
  Logger:=ALogger;
end;

procedure TGenISOImage.GenerateISO(dataPath: String; tempPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  //genisoimage -C 0,11702 -V GDMENU -G data/ip.bin -r -J -l -input-charset iso8859-1 -o gdmenu.iso data/1ST_READ.BIN $GDMENU_INI
  RunCommandWithLog(
    ResolveToolPath(ApplicationPath, ExecutablePath),
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
    Logger
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
    ResolveToolPath(ApplicationPath, ExecutablePath),
    parameters.ToStringArray,
    outputString,
    [poStderrToOutPut,poWaitOnExit],
    swoNone,
    Logger
  );
  commandOut.Add(outputString);
  commandOut.Destroy;
  parameters.Destroy;
end;

{ TCDI4DC }

constructor TCDI4DC.Create;
begin
  ExecutablePath:='cdi4dc';
  Logger:=nil;
end;

procedure TCDI4DC.SetLogger(ALogger: TCommandLogger);
begin
  Logger:=ALogger;
end;

procedure TCDI4DC.ConvertToCDI(inputPath: String; outputPath: String);
var
  outputString: ansistring;
begin
  RunCommandWithLog(
    ResolveToolPath(ApplicationPath, ExecutablePath),
    [
      inputPath,
      outputPath
    ],
    outputString,
    [poWaitOnExit],
    swoNone,
    Logger
  );
end;

{ TCDIRIP }

constructor TCDIRIP.Create;
begin
  ExecutablePath:='cdirip';
  Logger:=nil;
end;

procedure TCDIRIP.SetLogger(ALogger: TCommandLogger);
begin
  Logger:=ALogger;
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
    ResolveToolPath(ApplicationPath, ExecutablePath),
    [
      inputPath,
      ConcatPaths([outputPath,'cdicache'])
    ],
    outputString,
    [poWaitOnExit],
    swoNone,
    Logger
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

end.
