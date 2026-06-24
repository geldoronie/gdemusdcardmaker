unit gdemuunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, md5, RegExpr, process, fphttpclient,
  openssl, opensslsockets, DOM_HTML, DOM, SAX_HTML, Graphics, Dos, fpjson,
  jsonparser, jsonConf, URIParser, {$IFDEF UNIX}BaseUnix, Unix,{$ENDIF} gamemodel, externaltools;

type

    // O que fazer, na cópia, com um jogo selecionado que já está no SD Card.
    TDuplicatePolicy = (dpCopyAsNew, dpSkip, dpReplace);

    { Forward declarations }
    TGDEmu = class;

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
    PStartDownloadCovers = procedure;
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
      _onFinishCoversDownload: PStartDownloadCovers;
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
      LibretroBoxartIndex: TStringList; // cache em memória do índice de capas DC
      CatalogTable: TStringList; // cache do catálogo (normname=genero|ano|dev)
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
      DuplicateCopyPolicy: TDuplicatePolicy; // política para selecionados já no SD
      SDCardLoaded: Boolean;
      CurrentAction: String;
      CurrentActionStatus: String;
      LastError: String; // empty when the last action succeeded
      CurrentCopyToSDCardActionPosition: integer;
      CurrentCopyToSDCardActionCount: integer;
      CurrentCopyToSDCardActionGameName: String;
      CurrentLocalGamesScanActionPosition: integer;
      CurrentLocalGamesScanActionCount: integer;
      CurrentLocalGamesScanActionGameName: String;
      CurrentSDCardGamesScanActionPosition: integer;
      CurrentSDCardGamesScanActionCount: integer;
      CurrentCoverDownloadActionPosition: integer;
      CurrentCoverDownloadActionCount: integer;
      CurrentCoverDownloadActionGameName: String;
      CoverDownloadIsSD: Boolean; // alvo do lote: False=biblioteca, True=SD Card
      // Config do scraper (persistida na biblioteca):
      ScrapeBoxart: Boolean;   // baixar capa (boxart)
      ScrapeTitle: Boolean;    // baixar tela de título
      ScrapeSnap: Boolean;     // baixar screenshot in-game
      ScrapeOverwrite: Boolean; // re-baixar mesmo se já houver no cache
      CurrentSDCardId: String;    // ID do cartão carregado (de gdemugui-card.json)
      CurrentSDCardLabel: String; // rótulo amigável do cartão carregado
      SDCardRegistry: TStringList; // id=rótulo de todos os cartões conhecidos
      CurrentSDCardGamesScanActionGameName: String;
      constructor Create(CreateSuspended : boolean);
      procedure SetApplicationPath(value: String);
      procedure SetLocalGamesDirectories(List: TStrings);
      procedure SetSDCardGamesDirectory(Path: String);
      procedure ScanLocalGamesDirectories;
      procedure StartScanLocalGamesDirectories(onLocalGamesScanFinished: PStartScanLocalGamesDirectories);
      procedure ScanSDCardGamesDirectory;
      procedure CloseSDCard;
      procedure StartScanSDCardGamesDirectories(onSDCardGamesScanFinished: PStartScanSDCardGamesDirectories);
      procedure DownloadAllCovers;
      procedure StartDownloadAllLocalCovers(onFinish: PStartDownloadCovers);
      procedure StartDownloadAllSDCovers(onFinish: PStartDownloadCovers);
      procedure OnFinishCoversDownload;
      procedure MarkLocalGamesPresentOnSDCard;
      function LoadSDCardIdentity: Boolean;
      procedure SaveSDCardIdentity(const aLabel: String);
      function CardLabel(const aId: String): String;
      procedure AddCardToGame(game: TGDEmuGame; const aCardId: String);
      procedure TagGamesOnCurrentCard;
      function GetDiskSpace(const aPath: String; out totalBytes, freeBytes: Int64): Boolean;
      procedure SaveLibrary;
      function LoadLibrary: Boolean;
      function GetCatalogTable: TStringList;
      procedure EnrichGameFromCatalog(game: TGDEmuGame);
      procedure UpdateSDCardGameList;
      procedure ClearLocalGamesDirectories;
      procedure ClearSDCardGamesDirectories;
      procedure RemoveFromSDCard;
      procedure CopySelectedLocalGamesToSDCard;
      procedure StartCopySelectedLocalGamesToSDCard(onCopyFished: PStartCopySelectedLocalGamesToSDCard = nil);
      procedure SelectLocalGameToCopy(index: integer);
      procedure SelectSDCardGameToRemove(index: integer);
      procedure ClearSelectedLocalGamesToCopy;
      procedure ClearSelectedSDCardGamesToRemove;
      procedure CreateGDEmuImage;
      procedure CreateOpenBORDisc(name: String; coverImagePath: string; mainPakFilePath: string; otherFilesPaths: TStrings; outputPath: string);
      procedure CreateInfoCacheFile(game: TGDEmuGame);
      function GetMetaFileInfo(game: TGDEmuGame): TGDEmuGame;
      function GetGameCover(game: TGDEmuGame): String;
      procedure ScrapeGameImages(game: TGDEmuGame);
      function ResolveImageFile(game: TGDEmuGame; const aSuffix: String): String;
      function GetMetaFileInfoCache(game: TGDEmuGame): TGDEmuGame;
      function GetCommandLog: TStringList;
      procedure ClearCommandLog;
    private
      function TryGetCoverFromGamesDatabase(game: TGDEmuGame; searchTerms: TStringList): String;
      function GetLibretroBoxartIndex: TStringList;
      function TryGetCoverFromLibretro(game: TGDEmuGame; searchTerms: TStringList): String;
      function MatchLibretroFilename(game: TGDEmuGame; searchTerms: TStringList): String;
      function DownloadLibretroImage(const aFilename, aNamedDir, aCacheFile: String): Boolean;
      function BuildLibretroSearchTerms(game: TGDEmuGame): TStringList;
      function NormalizeForMatch(const s: String): String;
      function DownloadAndValidateImage(imageUrl: String; cacheFilename: String): Boolean;
      function EncodeURLComponent(const s: String): String;
      function CleanGameNameForSearch(const gameName: String): String;
      function GenerateGamesDatabaseSlug(const gameName: String): String;
    end;

var
  GDEmu: TGDEmu;

implementation

{ TGDEmu }


procedure TGDEmu.Execute;
begin
  // Outer try/except is a backstop: an unexpected exception must never kill the
  // worker thread (FreeOnTerminate would free it and leave GDEmu dangling).
  // Each action also handles its own failure so the UI can recover: on error we
  // still move to FINISHED (the ProgressWindow closes instead of spinning
  // forever) and fire the finish callback; LastError carries the message.
  while Terminated = False do
  begin
    try
      if (CurrentAction = 'COPYINGTOSDCARD') and (CurrentActionStatus = 'PENDING') then
      begin
        CurrentActionStatus:='COPYING';
        LastError:='';
        try
          CopySelectedLocalGamesToSDCard;
        except
          on E: Exception do
          begin
            LastError:=E.Message;
            AddCommandLog('ERROR: CopySelectedLocalGamesToSDCard', E.Message);
          end;
        end;
        CurrentActionStatus:='FINISHED';
        Synchronize(@OnFinishGamesCopy);
      end;

      if (CurrentAction = 'SCANLOCALGAMESDIRECTORIES') and (CurrentActionStatus = 'PENDING') then
      begin
        CurrentActionStatus:='SCANNING';
        LastError:='';
        try
          ScanLocalGamesDirectories;
        except
          on E: Exception do
          begin
            LastError:=E.Message;
            AddCommandLog('ERROR: ScanLocalGamesDirectories', E.Message);
          end;
        end;
        CurrentActionStatus:='FINISHED';
        Synchronize(@OnFinishLocalGamesScan);
      end;

      if (CurrentAction = 'SCANSDCARDGAMESDIRECTORIES') and (CurrentActionStatus = 'PENDING') then
      begin
        CurrentActionStatus:='SCANNING';
        LastError:='';
        try
          ScanSDCardGamesDirectory;
        except
          on E: Exception do
          begin
            LastError:=E.Message;
            AddCommandLog('ERROR: ScanSDCardGamesDirectory', E.Message);
          end;
        end;
        CurrentActionStatus:='FINISHED';
        Synchronize(@OnFinishSDCardGamesScan);
      end;

      if (CurrentAction = 'DOWNLOADINGCOVERS') and (CurrentActionStatus = 'PENDING') then
      begin
        CurrentActionStatus:='DOWNLOADING';
        LastError:='';
        try
          DownloadAllCovers;
        except
          on E: Exception do
          begin
            LastError:=E.Message;
            AddCommandLog('ERROR: DownloadAllCovers', E.Message);
          end;
        end;
        CurrentActionStatus:='FINISHED';
        Synchronize(@OnFinishCoversDownload);
      end;
    except
      on E: Exception do
        AddCommandLog('ERROR: worker loop', E.Message);
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

procedure TGDEmu.OnFinishCoversDownload;
begin
  if _onFinishCoversDownload <> nil then
    _onFinishCoversDownload;
end;

// Baixa (em lote) a capa de todos os jogos da biblioteca OU do SD Card (conforme
// CoverDownloadIsSD). GetGameCover já pula os que têm capa válida em cache, então
// é seguro/idempotente — re-rodar só busca os que faltam. Roda na worker thread;
// o progresso é lido pelo ProgressWindow.
procedure TGDEmu.DownloadAllCovers;
var i, n: integer;
    g: TGDEmuGame;
begin
  if CoverDownloadIsSD then n:=SDCardGamesListCount else n:=LocalGamesListCount;
  for i:=0 to n -1 do
  begin
    if CoverDownloadIsSD then g:=SDCardGamesList[i] else g:=LocalGamesList[i];
    CurrentCoverDownloadActionGameName:=g.Name;
    try
      ScrapeGameImages(g); // baixa as imagens habilitadas na config do scraper
    except
      on E: Exception do
        AddCommandLog('Scraper em lote: erro', Format('%s: %s', [g.Name, E.Message]));
    end;
    CurrentCoverDownloadActionPosition:=i + 1;
  end;
end;

procedure TGDEmu.StartDownloadAllLocalCovers(onFinish: PStartDownloadCovers);
begin
  CoverDownloadIsSD:=False;
  CurrentAction:='DOWNLOADINGCOVERS';
  CurrentActionStatus:='PENDING';
  CurrentCoverDownloadActionPosition:=0;
  CurrentCoverDownloadActionCount:=LocalGamesListCount;
  _onFinishCoversDownload:=onFinish;
end;

procedure TGDEmu.StartDownloadAllSDCovers(onFinish: PStartDownloadCovers);
begin
  CoverDownloadIsSD:=True;
  CurrentAction:='DOWNLOADINGCOVERS';
  CurrentActionStatus:='PENDING';
  CurrentCoverDownloadActionPosition:=0;
  CurrentCoverDownloadActionCount:=SDCardGamesListCount;
  _onFinishCoversDownload:=onFinish;
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
  LibretroBoxartIndex:=nil;
  CatalogTable:=nil;
  SDCardRegistry:=TStringList.Create;
  SDCardRegistry.NameValueSeparator:='=';
  ScrapeBoxart:=True;   // padrão: só a capa
  ScrapeTitle:=False;
  ScrapeSnap:=False;
  ScrapeOverwrite:=False;
  InitSSLInterface;
end;

procedure TGDEmu.SetApplicationPath(value: String);
begin
  ApplicationPath:=value;
  GDIToolsProcess.ApplicationPath:=ApplicationPath;
  GDIToolsProcess.SetLogger(@Self.AddCommandLog);
  GenISOImage.ApplicationPath:=ApplicationPath;
  GenISOImage.SetLogger(@Self.AddCommandLog);
  HexDump.SetLogger(@Self.AddCommandLog);
  CDI4DC.ApplicationPath:=ApplicationPath;
  CDI4DC.SetLogger(@Self.AddCommandLog);
  CDIRIP.ApplicationPath:=ApplicationPath;
  CDIRIP.SetLogger(@Self.AddCommandLog);
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

// "Ejeta" o cartão atual: zera todo o estado do SD para que outro possa ser
// carregado sem reiniciar o app.
procedure TGDEmu.CloseSDCard;
begin
  ClearSDCardGamesDirectories; // libera os objetos e zera a contagem
  SDCardGamesDirectory:='';
  SDCardGamesListIndexCount:=0;
  SDCardLoaded:=False;
  CurrentSDCardId:='';
  CurrentSDCardLabel:='';
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
         // Nome do jogo via GetGameName (lê name.txt; cai no nome da pasta quando
         // não existe) — mesma regra do scan do SD. Antes usava só o nome da pasta,
         // o que mostrava o número (ex.: "15") quando a origem era um cartão GDEMU,
         // e gravava esse número no name.txt do destino ao copiar.
         LocalGamesList[LocalGamesListCount].Name:=Trim(GetGameName(Directories[j]));
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
         EnrichGameFromCatalog(LocalGamesList[LocalGamesListCount]);
         // Tamanho do disco agora (ROM montado) p/ desempate offline depois.
         LocalGamesList[LocalGamesListCount].DiscSize:=
           LargestDiscFileSize(LocalGamesList[LocalGamesListCount].Path);

         LocalGamesListCount:=LocalGamesListCount + 1;
         CacheList.SaveToFile(ConcatPaths([ApplicationPath,'cache','cache.list']));
       end;
       CurrentLocalGamesScanActionPosition:=CurrentLocalGamesScanActionPosition + 1;
       GameDirectoryContentGDI.Destroy;
       GameDirectoryContentCDI.Destroy;
     end;
   end;
   Directories.Destroy;
   // Persiste a biblioteca recém-escaneada (diretórios + jogos).
   SaveLibrary;
end;

// Cruza a biblioteca local com o cartão pelo MD5 do IP.BIN (game.Id): marca cada
// jogo local que já está presente no SD e guarda o slot (Index) onde ele mora.
// Seguro chamar a qualquer momento; se uma das listas estiver vazia, só limpa as
// marcas. Jogos sem Id (extração falhou) não são casados.
procedure TGDEmu.MarkLocalGamesPresentOnSDCard;
var i, j, idMatch: integer;
    sizeKnown: Boolean;
begin
  // Tamanho do maior arquivo do disco no cartão (sempre computável — está montado).
  for j:=0 to SDCardGamesListCount -1 do
    if (SDCardGamesList[j].DiscSize = 0) and (SDCardGamesList[j].Path <> '') then
      SDCardGamesList[j].DiscSize:=LargestDiscFileSize(SDCardGamesList[j].Path);

  for i:=0 to LocalGamesListCount -1 do
  begin
    LocalGamesList[i].OnSDCard:=False;
    LocalGamesList[i].SDCardIndex:=0;
    if LocalGamesList[i].Id = '' then
      Continue;
    // Tenta computar o tamanho do disco local — mas pode dar 0 se a coleção de
    // ROMs estiver offline (caso comum: escaneou uma vez, depois trabalha só com
    // a biblioteca + cartão). Nesse caso, casa só pelo Id (que está persistido).
    if LocalGamesList[i].DiscSize = 0 then
      LocalGamesList[i].DiscSize:=LargestDiscFileSize(LocalGamesList[i].Path);

    idMatch:=-1;
    for j:=0 to SDCardGamesListCount -1 do
    begin
      if SDCardGamesList[j].Id <> LocalGamesList[i].Id then
        Continue;
      // Id (MD5 do IP.BIN) bate. Se AMBOS os tamanhos são conhecidos, exige
      // igualdade — isso desempata homebrews de mesmo IP.BIN (OpenBOR). Se algum
      // tamanho é 0 (não computável), casa pelo Id apenas.
      sizeKnown:=(LocalGamesList[i].DiscSize > 0) and (SDCardGamesList[j].DiscSize > 0);
      if (not sizeKnown) or (SDCardGamesList[j].DiscSize = LocalGamesList[i].DiscSize) then
      begin
        idMatch:=j;
        if sizeKnown then Break; // match forte (Id+tamanho): pode parar
      end;
    end;
    if idMatch >= 0 then
    begin
      LocalGamesList[i].OnSDCard:=True;
      LocalGamesList[i].SDCardIndex:=SDCardGamesList[idMatch].Index;
    end;
  end;
end;

// Lê a identidade do cartão carregado de <SD>/gdemugui-card.json. Define
// CurrentSDCardId/Label e registra o rótulo. False se o arquivo não existir
// (cartão ainda sem identidade).
function TGDEmu.LoadSDCardIdentity: Boolean;
var
  data: TJSONData;
  obj: TJSONObject;
  sl: TStringList;
  path: String;
begin
  Result:=False;
  CurrentSDCardId:='';
  CurrentSDCardLabel:='';
  if SDCardGamesDirectory = '' then Exit;
  path:=ConcatPaths([SDCardGamesDirectory,'gdemugui-card.json']);
  if not FileExists(path) then Exit;
  data:=nil;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(path);
    data:=GetJSON(sl.Text);
  except
    sl.Free;
    if data <> nil then data.Free;
    Exit;
  end;
  sl.Free;
  if (data <> nil) and (data.JSONType = jtObject) then
  begin
    obj:=TJSONObject(data);
    CurrentSDCardId:=obj.Get('id', '');
    CurrentSDCardLabel:=obj.Get('label', '');
    if CurrentSDCardId <> '' then
    begin
      SDCardRegistry.Values[CurrentSDCardId]:=CurrentSDCardLabel;
      Result:=True;
    end;
  end;
  if data <> nil then data.Free;
end;

// Grava/atualiza a identidade do cartão (gera um ID novo se ainda não houver) e
// registra o rótulo.
procedure TGDEmu.SaveSDCardIdentity(const aLabel: String);
var
  obj: TJSONObject;
  sl: TStringList;
  g: TGUID;
begin
  if SDCardGamesDirectory = '' then Exit;
  if CurrentSDCardId = '' then
  begin
    CreateGUID(g);
    CurrentSDCardId:=GUIDToString(g);
  end;
  CurrentSDCardLabel:=aLabel;
  SDCardRegistry.Values[CurrentSDCardId]:=aLabel;
  obj:=TJSONObject.Create;
  try
    obj.Add('id', CurrentSDCardId);
    obj.Add('label', aLabel);
    sl:=TStringList.Create;
    try
      sl.Text:=obj.FormatJSON;
      sl.SaveToFile(ConcatPaths([SDCardGamesDirectory,'gdemugui-card.json']));
    finally
      sl.Free;
    end;
  finally
    obj.Free;
  end;
end;

function TGDEmu.CardLabel(const aId: String): String;
begin
  Result:=SDCardRegistry.Values[aId];
  if Result = '' then Result:=aId; // fallback: mostra o id se não houver rótulo
end;

// Acrescenta um cartão à lista de "copiado em" de um jogo (sem duplicar).
procedure TGDEmu.AddCardToGame(game: TGDEmuGame; const aCardId: String);
var ids: TStringList;
begin
  if (game = nil) or (aCardId = '') then Exit;
  ids:=TStringList.Create;
  try
    ids.Delimiter:='|';
    ids.StrictDelimiter:=True;
    ids.DelimitedText:=game.CopiedToCardIds;
    if ids.IndexOf(aCardId) < 0 then
    begin
      ids.Add(aCardId);
      game.CopiedToCardIds:=ids.DelimitedText;
    end;
  finally
    ids.Free;
  end;
end;

// Marca todos os jogos da biblioteca presentes no cartão carregado (OnSDCard)
// como copiados nele. Faz o backfill inclusive de cartões já existentes. Persiste.
procedure TGDEmu.TagGamesOnCurrentCard;
var i: integer;
begin
  if CurrentSDCardId = '' then Exit;
  for i:=0 to LocalGamesListCount -1 do
    if LocalGamesList[i].OnSDCard then
      AddCardToGame(LocalGamesList[i], CurrentSDCardId);
  SaveLibrary;
end;

// Espaço total e livre (bytes) do sistema de arquivos onde aPath reside.
// Linux: statfs. Windows fica para a fase de portabilidade.
function TGDEmu.GetDiskSpace(const aPath: String; out totalBytes, freeBytes: Int64): Boolean;
{$IFDEF UNIX}
var info: TStatFS;
begin
  totalBytes:=0; freeBytes:=0; Result:=False;
  if fpStatFS(PChar(aPath), @info) = 0 then
  begin
    totalBytes:=Int64(info.bsize) * Int64(info.blocks);
    freeBytes:=Int64(info.bsize) * Int64(info.bavail);
    Result:=True;
  end;
end;
{$ELSE}
begin
  totalBytes:=0; freeBytes:=0; Result:=False;
  // TODO Windows: GetDiskFreeSpaceEx (fase de portabilidade).
end;
{$ENDIF}

// Persiste a biblioteca (diretórios + jogos escaneados com metadados e
// enriquecimento) em ApplicationPath/library.json.
procedure TGDEmu.SaveLibrary;
var
  root: TJSONObject;
  dirsArr, gamesArr, cardsArr: TJSONArray;
  gObj, cObj: TJSONObject;
  i: integer;
  sl: TStringList;
begin
  root:=TJSONObject.Create;
  try
    root.Add('version', 1);
    dirsArr:=TJSONArray.Create;
    for i:=0 to LocalGamesDirectoriesList.Count -1 do
      dirsArr.Add(LocalGamesDirectoriesList[i]);
    root.Add('directories', dirsArr);

    gamesArr:=TJSONArray.Create;
    for i:=0 to LocalGamesListCount -1 do
    begin
      gObj:=TJSONObject.Create;
      gObj.Add('path', LocalGamesList[i].Path);
      gObj.Add('name', LocalGamesList[i].Name);
      gObj.Add('slug', LocalGamesList[i].SlugName);
      gObj.Add('extension', LocalGamesList[i].Extension);
      gObj.Add('internalName', LocalGamesList[i].InternalName);
      gObj.Add('id', LocalGamesList[i].Id);
      gObj.Add('catalogID', LocalGamesList[i].CatalogID);
      gObj.Add('region', LocalGamesList[i].Region);
      gObj.Add('date', LocalGamesList[i].Date);
      gObj.Add('disc', LocalGamesList[i].Disc);
      gObj.Add('version', LocalGamesList[i].Version);
      gObj.Add('vga', LocalGamesList[i].VGA);
      gObj.Add('discSize', LocalGamesList[i].DiscSize);
      gObj.Add('developer', LocalGamesList[i].Developer);
      gObj.Add('releaseYear', LocalGamesList[i].ReleaseYear);
      gObj.Add('genre', LocalGamesList[i].Genre);
      gObj.Add('copiedToCards', LocalGamesList[i].CopiedToCardIds);
      gamesArr.Add(gObj);
    end;
    root.Add('games', gamesArr);

    // Registro de cartões conhecidos (id -> rótulo).
    cardsArr:=TJSONArray.Create;
    for i:=0 to SDCardRegistry.Count -1 do
    begin
      cObj:=TJSONObject.Create;
      cObj.Add('id', SDCardRegistry.Names[i]);
      cObj.Add('label', SDCardRegistry.ValueFromIndex[i]);
      cardsArr.Add(cObj);
    end;
    root.Add('cardRegistry', cardsArr);

    // Config do scraper.
    root.Add('scrapeConfig', TJSONObject.Create(['boxart', ScrapeBoxart,
      'title', ScrapeTitle, 'snap', ScrapeSnap, 'overwrite', ScrapeOverwrite]));

    sl:=TStringList.Create;
    try
      sl.Text:=root.FormatJSON;
      sl.SaveToFile(ConcatPaths([ApplicationPath,'library.json']));
    finally
      sl.Free;
    end;
    AddCommandLog('Biblioteca salva', Format('%d diretórios, %d jogos',
      [LocalGamesDirectoriesList.Count, LocalGamesListCount]));
  finally
    root.Free;
  end;
end;

// Carrega a biblioteca persistida (diretórios + jogos) sem re-escanear. Retorna
// False se não houver library.json válido.
function TGDEmu.LoadLibrary: Boolean;
var
  data, d: TJSONData;
  root, gObj: TJSONObject;
  dirsArr, gamesArr: TJSONArray;
  i: integer;
  sl: TStringList;
  libPath: String;
  game: TGDEmuGame;
begin
  Result:=False;
  libPath:=ConcatPaths([ApplicationPath,'library.json']);
  if not FileExists(libPath) then Exit;

  data:=nil;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(libPath);
    data:=GetJSON(sl.Text);
  except
    on E: Exception do
    begin
      AddCommandLog('Biblioteca: erro ao ler', E.Message);
      sl.Free;
      if data <> nil then data.Free;
      Exit;
    end;
  end;
  sl.Free;
  if (data = nil) or (data.JSONType <> jtObject) then
  begin
    if data <> nil then data.Free;
    Exit;
  end;

  try
    root:=TJSONObject(data);

    LocalGamesDirectoriesList.Clear;
    d:=root.Find('directories');
    if (d <> nil) and (d.JSONType = jtArray) then
    begin
      dirsArr:=TJSONArray(d);
      for i:=0 to dirsArr.Count -1 do
        LocalGamesDirectoriesList.Add(dirsArr.Items[i].AsString);
    end;

    ClearLocalGamesDirectories; // libera objetos antigos e zera a contagem
    d:=root.Find('games');
    if (d <> nil) and (d.JSONType = jtArray) then
    begin
      gamesArr:=TJSONArray(d);
      SetLength(LocalGamesList, gamesArr.Count);
      for i:=0 to gamesArr.Count -1 do
      begin
        gObj:=TJSONObject(gamesArr.Items[i]);
        game:=TGDEmuGame.Create;
        game.Path:=gObj.Get('path', '');
        game.Name:=gObj.Get('name', '');
        game.SlugName:=gObj.Get('slug', '');
        game.Extension:=gObj.Get('extension', '');
        game.InternalName:=gObj.Get('internalName', '');
        game.Id:=gObj.Get('id', '');
        game.CatalogID:=gObj.Get('catalogID', '');
        game.Region:=gObj.Get('region', '');
        game.Date:=gObj.Get('date', '');
        game.Disc:=gObj.Get('disc', '');
        game.Version:=gObj.Get('version', '');
        game.VGA:=gObj.Get('vga', '');
        game.DiscSize:=gObj.Get('discSize', Int64(0));
        game.Developer:=gObj.Get('developer', '');
        game.ReleaseYear:=gObj.Get('releaseYear', '');
        game.Genre:=gObj.Get('genre', '');
        game.CopiedToCardIds:=gObj.Get('copiedToCards', '');
        if (game.Genre = '') and (game.Developer = '') and (game.ReleaseYear = '') then
          EnrichGameFromCatalog(game); // back-fill de bibliotecas antigas
        LocalGamesList[i]:=game;
      end;
      LocalGamesListCount:=gamesArr.Count;
    end;

    // Config do scraper.
    d:=root.Find('scrapeConfig');
    if (d <> nil) and (d.JSONType = jtObject) then
    begin
      ScrapeBoxart:=TJSONObject(d).Get('boxart', True);
      ScrapeTitle:=TJSONObject(d).Get('title', False);
      ScrapeSnap:=TJSONObject(d).Get('snap', False);
      ScrapeOverwrite:=TJSONObject(d).Get('overwrite', False);
    end;

    // Registro de cartões conhecidos (id -> rótulo).
    SDCardRegistry.Clear;
    d:=root.Find('cardRegistry');
    if (d <> nil) and (d.JSONType = jtArray) then
      for i:=0 to TJSONArray(d).Count -1 do
        if TJSONArray(d).Items[i].JSONType = jtObject then
          SDCardRegistry.Values[TJSONObject(TJSONArray(d).Items[i]).Get('id', '')]:=
            TJSONObject(TJSONArray(d).Items[i]).Get('label', '');

    Result:=True;
    AddCommandLog('Biblioteca carregada', Format('%d diretórios, %d jogos',
      [LocalGamesDirectoriesList.Count, LocalGamesListCount]));
  finally
    data.Free;
  end;
end;

// Carrega (lazy) o catálogo embutido data/dc_catalog.tsv para um TStringList
// indexado por nome normalizado: "normname=genero|ano|developer", ordenado para
// busca binária via IndexOfName.
function TGDEmu.GetCatalogTable: TStringList;
var
  raw: TStringList;
  i: integer;
  parts: TStringArray;
  gen, yr, dv, catPath: String;
begin
  if CatalogTable <> nil then Exit(CatalogTable);
  CatalogTable:=TStringList.Create;
  CatalogTable.NameValueSeparator:='=';
  catPath:=ConcatPaths([ApplicationPath,'data','dc_catalog.tsv']);
  if not FileExists(catPath) then Exit(CatalogTable);
  raw:=TStringList.Create;
  try
    raw.LoadFromFile(catPath);
    for i:=0 to raw.Count -1 do
    begin
      if raw[i] = '' then Continue;
      parts:=raw[i].Split([#9]);
      if Length(parts) < 1 then Continue;
      gen:=''; yr:=''; dv:='';
      if Length(parts) > 2 then gen:=parts[2];
      if Length(parts) > 3 then yr:=parts[3];
      if Length(parts) > 4 then dv:=parts[4];
      CatalogTable.Add(parts[0] + '=' + gen + '|' + yr + '|' + dv);
    end;
    CatalogTable.Sorted:=True;
    AddCommandLog('Catálogo carregado', Format('%d jogos', [CatalogTable.Count]));
  finally
    raw.Free;
  end;
  Result:=CatalogTable;
end;

// Preenche Genre/ReleaseYear/Developer de um jogo casando pelo nome normalizado
// contra o catálogo embutido. Tenta o nome do arquivo e, se falhar, o InternalName.
procedure TGDEmu.EnrichGameFromCatalog(game: TGDEmuGame);
var
  tbl: TStringList;
  key, val: String;
  idx: integer;
  parts: TStringArray;
begin
  if game = nil then Exit;
  tbl:=GetCatalogTable;
  if tbl.Count = 0 then Exit;

  key:=NormalizeForMatch(CleanGameNameForSearch(game.Name));
  idx:=-1;
  if key <> '' then idx:=tbl.IndexOfName(key);
  if (idx < 0) and (game.InternalName <> '') then
  begin
    key:=NormalizeForMatch(CleanGameNameForSearch(game.InternalName));
    if key <> '' then idx:=tbl.IndexOfName(key);
  end;
  if idx < 0 then Exit;

  val:=tbl.ValueFromIndex[idx];
  parts:=val.Split(['|']);
  if Length(parts) > 0 then game.Genre:=parts[0];
  if Length(parts) > 1 then game.ReleaseYear:=parts[1];
  if Length(parts) > 2 then game.Developer:=parts[2];
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
    folderIndex: integer;
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

     // Slots GDEMU são pastas numeradas (01, 02, …). StrToIntDef evita estourar
     // EConvertError em pastas não-numéricas (BKP, DS, ou uma coleção de ISOs com
     // pastas nomeadas pelo jogo) — elas simplesmente são ignoradas aqui.
     folderIndex:=SysUtils.StrToIntDef(SysUtils.ExtractFileName(Directories[i]), -1);

     if (
           (GameDirectoryContentGDI.Count > 0) or
           (GameDirectoryContentCDI.Count > 0)
        ) and
        (folderIndex > 1)
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

       SDCardGamesList[SDCardGamesListCount].Index:=folderIndex;

       CacheList.Add(SDCardGamesList[SDCardGamesListCount].SlugName);

       GetMetaFileInfo(SDCardGamesList[SDCardGamesListCount]);
       CreateInfoCacheFile(SDCardGamesList[SDCardGamesListCount]);
       EnrichGameFromCatalog(SDCardGamesList[SDCardGamesListCount]);

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
    tempFiles: TStringList;
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
  // Clean the temp directory (DeleteFile does not support wildcards)
  tempFiles:=TStringList.Create;
  try
    FileUtil.FindAllFiles(tempFiles, ConcatPaths([ApplicationPath,'temp']), '*', False, faAnyFile);
    for i:=0 to tempFiles.Count -1 do
      DeleteFile(tempFiles[i]);
  finally
    tempFiles.Free;
  end;
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

procedure TGDEmu.CopySelectedLocalGamesToSDCard;
var i: integer;
    game: TGDEmuGame;
    isDuplicate: Boolean;
    Source: string;
    Target: string;
    ISOFileNewName: string;
    GameDirectoryContent: TStringList;
    NameTXT: TStringList;
begin
  for i:=0 to SelectedLocalGamesListCount -1 do
  begin
    game:=LocalGamesList[SelectedLocalGamesList[i]];
    CurrentCopyToSDCardActionGameName:=game.Name;
    isDuplicate:=game.OnSDCard and (game.SDCardIndex > 0);

    // Já existe no SD e o usuário pediu para ignorar duplicados: pular.
    if isDuplicate and (DuplicateCopyPolicy = dpSkip) then
    begin
      AddCommandLog('Cópia: ignorado (já no SD)', game.Name);
      CurrentCopyToSDCardActionPosition:=CurrentCopyToSDCardActionPosition + 1;
      Continue;
    end;

    GameDirectoryContent:=TStringList.Create;
    NameTXT:=TStringList.Create;
    Source:=game.Path;

    if isDuplicate and (DuplicateCopyPolicy = dpReplace) then
    begin
      // Substituir: reusa a pasta existente no SD, limpando-a antes.
      Target:=ConcatPaths([SDCardGamesDirectory, Format('%.2d',[game.SDCardIndex])]);
      AddCommandLog('Cópia: substituindo no SD', Format('%s -> slot %.2d', [game.Name, game.SDCardIndex]));
      if TestMode = False then
        FileUtil.DeleteDirectory(Target, False);
    end
    else
    begin
      // Cópia nova: próximo slot livre.
      Target:=ConcatPaths([SDCardGamesDirectory, Format('%.2d',[SDCardGamesListIndexCount + 1])]);
      SDCardGamesListIndexCount:=SDCardGamesListIndexCount + 1;
      AddCommandLog('Cópia: novo jogo', Format('%s -> %s', [game.Name, ExtractFileName(Target)]));
    end;

    // Copying Files
    if TestMode = False then
      FileUtil.CopyDirTree(Source,Target,[TCopyFileFlag.cffCreateDestDirectory, TCopyFileFlag.cffOverwriteFile]);
    if TestMode = False then
      FileUtil.FindAllFiles(GameDirectoryContent,Target,'*.gdi;*.cdi', False, faAnyFile);
    if GameDirectoryContent.Count > 0 then
    begin
      ISOFileNewName:=ConcatPaths([ Target,'disc' + SysUtils.LowerCase(SysUtils.ExtractFileExt(GameDirectoryContent[0])) ]);
      if TestMode = False then
        RenameFile(GameDirectoryContent[0],ISOFileNewName);
    end;
    // Creating name.txt
    NameTXT.Add(game.Name);
    if TestMode = False then
      NameTXT.SaveToFile(ConcatPaths([ Target,'name.txt']));
    GameDirectoryContent.Destroy;
    NameTXT.Destroy;
    // Etiqueta automática: o jogo agora está neste cartão.
    if (TestMode = False) and (CurrentSDCardId <> '') then
      AddCardToGame(game, CurrentSDCardId);
    CurrentCopyToSDCardActionPosition:=CurrentCopyToSDCardActionPosition + 1;
  end;
  if (TestMode = False) and (CurrentSDCardId <> '') then
    SaveLibrary; // persiste as etiquetas dos jogos copiados
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
    cachedCoverFile: String;
begin
  coverCacheImageFilename:='';
  coverFileTest:=TPicture.Create;
  imageFound:=False;
  coverFileCheck:=nil;

  // Verificar se já existe no cache e é válido (.png do libretro tem prioridade
  // sobre o .jpg legado do gamesdatabase).
  cachedCoverFile:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.png']);
  if not FileExists(cachedCoverFile) then
    cachedCoverFile:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.jpg']);
  if FileExists(cachedCoverFile) then
  begin
    try
      coverFileCheck:=TFileStream.Create(cachedCoverFile, fmOpenRead or fmShareDenyWrite);
      try
        imageSize:=coverFileCheck.Size;
        if imageSize = 0 then
        begin
          DeleteFile(cachedCoverFile);
        end
        else
        begin
          // Verificar se é uma imagem válida
          coverFileCheck.Position:=0;
          coverFileTest.LoadFromStream(coverFileCheck);
          if (coverFileTest.Width > 0) and (coverFileTest.Height > 0) then
          begin
            coverCacheImageFilename:=cachedCoverFile;
            imageFound:=True;
          end
          else
          begin
            DeleteFile(cachedCoverFile);
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
      if FileExists(cachedCoverFile) then
        DeleteFile(cachedCoverFile);
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

        // Fonte única: libretro-thumbnails (índice Redump, confiável). O antigo
        // scraping do GamesDatabase.org foi removido do fluxo — falhava quase
        // sempre e cada tentativa custava 5-30s de HTTP. Jogos sem capa no
        // libretro (ex.: homebrews) caem na imagem padrão, instantaneamente.
        AddCommandLog('Trying source', 'libretro-thumbnails');
        coverCacheImageFilename:=TryGetCoverFromLibretro(gameWithMetadata, searchVariations);
        if coverCacheImageFilename <> '' then
        begin
          imageFound:=True;
          AddCommandLog('Cover found', 'libretro-thumbnails');
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

// Reduz um título a uma forma canônica comparável: minúsculas, só alfanuméricos
// (descarta espaços, hífens, pontuação, "the"/"a" não — mantemos simples). Assim
// "18 Wheeler - American Pro Trucker" e "18 WHEELER AMERICAN PRO TRUCKER" batem.
function TGDEmu.NormalizeForMatch(const s: String): String;
var i: integer; c: Char;
begin
  Result:='';
  for i:=1 to Length(s) do
  begin
    c:=s[i];
    if (c >= 'A') and (c <= 'Z') then
      c:=Chr(Ord(c) + 32); // lowercase ASCII
    if ((c >= 'a') and (c <= 'z')) or ((c >= '0') and (c <= '9')) then
      Result:=Result + c;
  end;
end;

// Índice de capas de Dreamcast do libretro-thumbnails. Baixado uma vez via git
// tree API (1 request, ~1300 nomes Redump) e cacheado em disco e memória.
function TGDEmu.GetLibretroBoxartIndex: TStringList;
var
  treeJson: ansistring;
  re: TRegExpr;
  cachePath: String;
begin
  if (LibretroBoxartIndex <> nil) and (LibretroBoxartIndex.Count > 0) then
    Exit(LibretroBoxartIndex);
  if LibretroBoxartIndex = nil then
    LibretroBoxartIndex:=TStringList.Create;

  cachePath:=ConcatPaths([ApplicationPath,'cache','dc_boxart_index.list']);
  if FileExists(cachePath) then
  begin
    try
      LibretroBoxartIndex.LoadFromFile(cachePath);
      AddCommandLog('Libretro index (cache)', Format('%d capas', [LibretroBoxartIndex.Count]));
    except
      LibretroBoxartIndex.Clear;
    end;
  end;

  if LibretroBoxartIndex.Count = 0 then
  begin
    try
      AddCommandLog('Libretro index', 'Baixando índice de capas DC...');
      treeJson:=HTTPClient.Get('https://api.github.com/repos/libretro-thumbnails/Sega_-_Dreamcast/git/trees/master?recursive=1');
      re:=TRegExpr.Create('Named_Boxarts/([^"\\]+?)\.png');
      try
        if re.Exec(treeJson) then
          repeat
            LibretroBoxartIndex.Add(re.Match[1]);
          until not re.ExecNext;
      finally
        re.Free;
      end;
      AddCommandLog('Libretro index', Format('%d capas indexadas', [LibretroBoxartIndex.Count]));
      if LibretroBoxartIndex.Count > 0 then
        try LibretroBoxartIndex.SaveToFile(cachePath); except end;
    except
      on E: Exception do
        AddCommandLog('Libretro index ERRO', Format('%s: %s', [E.ClassName, E.Message]));
    end;
  end;
  Result:=LibretroBoxartIndex;
end;

// Casa o jogo contra o índice libretro pelo título-base (sem região/flags),
// preferindo a região do jogo, e baixa o PNG para cache/<slug>.png.
// Casa o jogo contra o índice libretro e devolve o nome de arquivo Redump
// (ex.: "Sonic Adventure (USA)"), ou '' se nada casar. O mesmo nome serve para
// boxart/title/snapshot (os 3 diretórios usam os mesmos nomes).
function TGDEmu.MatchLibretroFilename(game: TGDEmuGame; searchTerms: TStringList): String;
var
  index: TStringList;
  i, j, p, rank, bestRank: integer;
  entry, entryBase, entryNorm, termNorm: String;
  regionPref: array of String;
begin
  Result:='';
  index:=GetLibretroBoxartIndex;
  if (index = nil) or (index.Count = 0) then Exit;

  // Preferência de região a partir do nome/região do IP.BIN.
  if (Pos('(US)', game.Name) > 0) or (Pos('(USA)', game.Name) > 0) or
     (Pos('NTSC', UpperCase(game.Name)) > 0) or (Pos('U', game.Region) > 0) then
    regionPref:=['(USA)', '(World)', '(UK)', '(Europe)', '(Japan)']
  else if (Pos('(EU)', game.Name) > 0) or (Pos('(Europe)', game.Name) > 0) or
          (Pos('PAL', UpperCase(game.Name)) > 0) or (Pos('E', game.Region) > 0) then
    regionPref:=['(Europe)', '(UK)', '(World)', '(USA)', '(Japan)']
  else if (Pos('(JP)', game.Name) > 0) or (Pos('(Japan)', game.Name) > 0) or
          (Pos('J', game.Region) > 0) then
    regionPref:=['(Japan)', '(USA)', '(World)', '(Europe)']
  else
    regionPref:=['(USA)', '(World)', '(Europe)', '(UK)', '(Japan)'];

  bestRank:=MaxInt;
  for j:=0 to searchTerms.Count -1 do
  begin
    termNorm:=NormalizeForMatch(CleanGameNameForSearch(searchTerms[j]));
    if termNorm = '' then Continue;
    for i:=0 to index.Count -1 do
    begin
      entry:=index[i];
      p:=Pos(' (', entry);
      if p > 0 then entryBase:=Copy(entry, 1, p-1) else entryBase:=entry;
      entryNorm:=NormalizeForMatch(entryBase);
      if entryNorm = termNorm then
      begin
        rank:=Length(regionPref);
        for p:=0 to High(regionPref) do
          if Pos(regionPref[p], entry) > 0 then begin rank:=p; Break; end;
        if rank < bestRank then
        begin
          bestRank:=rank;
          Result:=entry;
          if rank = 0 then Break;
        end;
      end;
    end;
    if (Result <> '') and (bestRank = 0) then Break;
  end;
end;

// Baixa uma imagem do libretro (aNamedDir = 'Named_Boxarts'/'Named_Titles'/
// 'Named_Snaps') pelo nome Redump aFilename, valida e salva em aCacheFile.
function TGDEmu.DownloadLibretroImage(const aFilename, aNamedDir, aCacheFile: String): Boolean;
var
  encoded, url: String;
  i: integer;
  c: Char;
  ms: TMemoryStream;
  pic: TPicture;
begin
  Result:=False;
  encoded:='';
  for i:=1 to Length(aFilename) do
  begin
    c:=aFilename[i];
    if c = ' ' then encoded:=encoded + '%20'
    else if c = '#' then encoded:=encoded + '%23'
    else if c = '?' then encoded:=encoded + '%3F'
    else if c = '&' then encoded:=encoded + '%26'
    else if c = '+' then encoded:=encoded + '%2B'
    else if c = '%' then encoded:=encoded + '%25'
    else encoded:=encoded + c;
  end;
  url:='https://thumbnails.libretro.com/Sega%20-%20Dreamcast/' + aNamedDir + '/' + encoded + '.png';

  ms:=TMemoryStream.Create;
  pic:=TPicture.Create;
  try
    try
      HTTPClient.Get(url, ms);
      if ms.Size > 1024 then
      begin
        ms.Position:=0;
        pic.LoadFromStream(ms);
        if (pic.Width > 100) and (pic.Height > 100) then
        begin
          ms.Position:=0;
          ms.SaveToFile(aCacheFile);
          Result:=True;
          AddCommandLog('Libretro OK', Format('%s -> %s (%dx%d)',
            [aNamedDir, ExtractFileName(aCacheFile), pic.Width, pic.Height]));
        end;
      end;
    except
      on E: Exception do
        AddCommandLog('Libretro download ERRO', Format('%s: %s', [E.ClassName, E.Message]));
    end;
  finally
    pic.Free;
    ms.Free;
  end;
end;

function TGDEmu.TryGetCoverFromLibretro(game: TGDEmuGame; searchTerms: TStringList): String;
var best, cachePng: String;
begin
  Result:='';
  best:=MatchLibretroFilename(game, searchTerms);
  if best = '' then
  begin
    AddCommandLog('Libretro', 'Nenhuma imagem casou pelo título');
    Exit;
  end;
  AddCommandLog('Libretro match', best);
  cachePng:=ConcatPaths([ApplicationPath,'cache',game.SlugName + '.png']);
  if DownloadLibretroImage(best, 'Named_Boxarts', cachePng) then
    Result:=cachePng;
end;

// Monta as variações de nome para casar no libretro (InternalName do IP.BIN como
// prioridade, depois nome limpo/original/legal/slug). Extrai metadados do IP.BIN
// se ainda não houver. O chamador libera a lista.
function TGDEmu.BuildLibretroSearchTerms(game: TGDEmuGame): TStringList;
var gm: TGDEmuGame; cleanInternal, cleaned: String;
begin
  Result:=TStringList.Create;
  gm:=game;
  if (game.InternalName = '') or (game.InternalName = game.Name) then
  begin
    try
      gm:=GetMetaFileInfo(game);
      if not Assigned(gm) then gm:=game;
    except
      gm:=game;
    end;
  end;
  if gm.InternalName <> '' then
  begin
    cleanInternal:=Trim(gm.InternalName);
    cleanInternal:=ReplaceRegExpr('[\x00-\x08\x0B-\x1F]', cleanInternal, '');
    cleanInternal:=ReplaceRegExpr('\s+', cleanInternal, ' ');
    cleanInternal:=Trim(cleanInternal);
    if cleanInternal <> '' then Result.Add(cleanInternal);
  end;
  if gm.Name <> '' then
  begin
    cleaned:=CleanGameNameForSearch(gm.Name);
    if (cleaned <> '') and (Result.IndexOf(cleaned) = -1) then Result.Add(cleaned);
    if Result.IndexOf(gm.Name) = -1 then Result.Add(gm.Name);
  end;
  if (gm.LegalName <> '') and (Result.IndexOf(gm.LegalName) = -1) then Result.Add(gm.LegalName);
  if (gm.SlugName <> '') and (Result.IndexOf(gm.SlugName) = -1) then Result.Add(gm.SlugName);
end;

// Resolve o arquivo de imagem (sufixo ''/'-title'/'-snap') de um jogo no cache:
// primeiro pelo próprio slug; se faltar, por QUALQUER jogo (biblioteca ou SD) com
// o mesmo Id (IP.BIN) — assim a imagem baixada na biblioteca é reaproveitada no SD
// e vice-versa. Retorna o caminho existente ou ''.
function TGDEmu.ResolveImageFile(game: TGDEmuGame; const aSuffix: String): String;

  function CacheFileOf(g: TGDEmuGame): String;
  begin
    Result:=ConcatPaths([ApplicationPath,'cache', g.SlugName + aSuffix + '.png']);
    if FileExists(Result) then Exit;
    if aSuffix = '' then // boxart aceita .jpg legado
    begin
      Result:=ConcatPaths([ApplicationPath,'cache', g.SlugName + '.jpg']);
      if FileExists(Result) then Exit;
    end;
    Result:='';
  end;

var i: integer;
begin
  Result:=CacheFileOf(game);
  if (Result <> '') or (game.Id = '') then Exit;
  for i:=0 to LocalGamesListCount -1 do
    if (LocalGamesList[i].Id = game.Id) and (LocalGamesList[i] <> game) then
    begin
      Result:=CacheFileOf(LocalGamesList[i]);
      if Result <> '' then Exit;
    end;
  for i:=0 to SDCardGamesListCount -1 do
    if (SDCardGamesList[i].Id = game.Id) and (SDCardGamesList[i] <> game) then
    begin
      Result:=CacheFileOf(SDCardGamesList[i]);
      if Result <> '' then Exit;
    end;
  Result:='';
end;

// Scraper: baixa, conforme a config (ScrapeBoxart/Title/Snap), as imagens
// habilitadas para o jogo. Boxart=<slug>.png, Title=<slug>-title.png,
// Snap=<slug>-snap.png. Respeita ScrapeOverwrite. Antes de baixar, reaproveita do
// cache de outro jogo com o mesmo Id (imagem já baixada na biblioteca/SD).
procedure TGDEmu.ScrapeGameImages(game: TGDEmuGame);
var
  searchTerms: TStringList;
  best, cacheDir: String;
  anyNeed: Boolean;

  // Resolve um tipo só pelo cache (próprio ou reuso por Id). True = já resolvido.
  function Satisfied(enabled: Boolean; const suffix: String): Boolean;
  var f, reuse: String;
  begin
    Result:=True;
    if not enabled then Exit;
    f:=ConcatPaths([cacheDir, game.SlugName + suffix + '.png']);
    if (not ScrapeOverwrite) and FileExists(f) then Exit;
    if not ScrapeOverwrite then
    begin
      reuse:=ResolveImageFile(game, suffix);
      if reuse <> '' then
      begin
        try FileUtil.CopyFile(reuse, f); except end;
        AddCommandLog('Scraper reuso (mesmo Id)', ExtractFileName(f));
        Exit;
      end;
    end;
    Result:=False; // precisa baixar
  end;

  procedure FetchType(enabled: Boolean; const namedDir, suffix: String);
  var f: String;
  begin
    if not enabled then Exit;
    f:=ConcatPaths([cacheDir, game.SlugName + suffix + '.png']);
    if (not ScrapeOverwrite) and FileExists(f) then Exit;
    if best <> '' then DownloadLibretroImage(best, namedDir, f);
  end;

begin
  cacheDir:=ConcatPaths([ApplicationPath,'cache']);
  // 1) Tenta resolver tudo pelo cache/reuso por Id (sem rede nem extrair metadados).
  anyNeed:=False;
  if not Satisfied(ScrapeBoxart, '') then anyNeed:=True;
  if not Satisfied(ScrapeTitle, '-title') then anyNeed:=True;
  if not Satisfied(ScrapeSnap, '-snap') then anyNeed:=True;
  if not anyNeed then Exit;

  // 2) Falta baixar algum: aí sim extrai metadados, casa no libretro e baixa.
  searchTerms:=BuildLibretroSearchTerms(game);
  try
    best:=MatchLibretroFilename(game, searchTerms);
    FetchType(ScrapeBoxart, 'Named_Boxarts', '');
    FetchType(ScrapeTitle,  'Named_Titles',  '-title');
    FetchType(ScrapeSnap,   'Named_Snaps',   '-snap');
  finally
    searchTerms.Free;
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
    cachedExtension: String;
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
      // NÃO sobrescrever game.Extension a partir do cache: a extensão vem do scan
      // dos arquivos reais da pasta (.gdi/.cdi) e é autoritativa. O cache é keyed
      // pelo SlugName, que para jogos locais é o nome da pasta (ex.: "15") — então
      // um cache stale de OUTRO cartão (onde a pasta 15 era .cdi) poluiria a
      // extensão e mandaria a re-extração pelo caminho errado (cdirip num .gdi),
      // resultando em metadados vazios. Mantemos a extensão detectada no scan.
      cachedExtension:=UTF8Encode(gameCaheInfoFile.GetValue('extension', game.Extension));
      game.CatalogID:=UTF8Encode(gameCaheInfoFile.GetValue('catalogID', game.CatalogID));
      game.Date:=UTF8Encode(gameCaheInfoFile.GetValue('data', game.Date));
      game.Disc:=UTF8Encode(gameCaheInfoFile.GetValue('disc', game.Disc));
      game.Id:=UTF8Encode(gameCaheInfoFile.GetValue('id', game.Id));
      cachedInternalName:=UTF8Encode(gameCaheInfoFile.GetValue('internalName', ''));
      game.InternalName:=cachedInternalName;
      
      AddCommandLog('Cache loaded', Format('InternalName from cache: [%s], Length: %d', [cachedInternalName, Length(cachedInternalName)]));

      // Cache stale por colisão de SlugName entre cartões: se a extensão gravada no
      // cache diverge da extensão real detectada no scan, o cache é de OUTRO disco.
      // Invalidamos zerando o InternalName, o que dispara a re-extração no chamador
      // (GetMetaFileInfo), agora pelo caminho correto da extensão real.
      if (cachedExtension <> '') and (game.Extension <> '') and
         (SysUtils.LowerCase(cachedExtension) <> SysUtils.LowerCase(game.Extension)) then
      begin
        AddCommandLog('Cache stale', Format('Extensão do cache [%s] != extensão real [%s]; forçando re-extração', [cachedExtension, game.Extension]));
        game.InternalName:='';
      end;

      // Se o InternalName no cache está vazio, pode ser que o cache foi criado antes da extração
      // Nesse caso, vamos forçar uma nova extração
      if game.InternalName = '' then
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

