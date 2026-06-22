unit mainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ExtCtrls,
  Menus, Buttons, StdCtrls, ComCtrls, localgamesdirectorieswindows, gdemuunit,
  gamemodel, progresswindowunity, LCLType, aboutwindowunit,
  openborcreatorwindowunit, commandlogwindowunit, gamelistview;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    AddLocalGameDirectoriesMenuItem: TMenuItem;
    DownloadCoverMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    CreditsMenuItem: TMenuItem;
    DiskUseProgressBar: TProgressBar;
    ToolsMenuItem: TMenuItem;
    OpenBORDiscCreatorMenuItem: TMenuItem;
    UpdateSDCardGamesListMenuItem: TMenuItem;
    RemoveGamesFromSDCardMenuItem: TMenuItem;
    CopySelectedGamesToSDCardMenuItem: TMenuItem;
    N2: TMenuItem;
    ViewMenuItem: TMenuItem;
    UpdateGamesListSDCardBitBtn: TBitBtn;
    CopySelectedBitBtn: TBitBtn;
    RemoveGameFromSDCardBitBtn: TBitBtn;
    LocalGameMD5Label: TLabel;
    SDCardGameDiscTypeLabel: TLabel;
    LocalGameDiscTypeLabel: TLabel;
    LocalGameNameLabel: TLabel;
    SDCardGamePathLabel: TLabel;
    SDCardGameNameLabel: TLabel;
    LocalGameCoverImage: TImage;
    SDCardCoverImage: TImage;
    LocalGameDownloadCoverBitBtn: TBitBtn;
    SDCardDownloadCoverBitBtn: TBitBtn;
    LoadSDCardBitBtn: TBitBtn;
    OpenLocalGamesDirectoriesDialogBitBtn: TBitBtn;
    LocalGamesList: TCheckListBox;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    N1: TMenuItem;
    OpenMenuItem: TMenuItem;
    LocalGamesToolsPanel: TPanel;
    SDCardGameInfoPanel: TPanel;
    LocalGameInfoPanel: TPanel;
    LocalGamePathLabel: TLabel;
    SDCardGameIndexLabel: TLabel;
    SDCardGameMD5Label: TLabel;
    SDCardGamesToolsButtonsPanel: TPanel;
    LocalGamesToolsButtonsPanel: TPanel;
    SDCardGamesToolsPanel: TPanel;
    SDCardList: TCheckListBox;
    MainWindowMenu: TMainMenu;
    OpenSDCardDirectorySelectDirectoryDialog: TSelectDirectoryDialog;
    Splitter1: TSplitter;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure CopySelectedBitBtnClick(Sender: TObject);
    procedure CreditsMenuItemClick(Sender: TObject);
    procedure DownloadCoverMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure LocalGamesListSelectionChange(Sender: TObject; User: boolean);
    procedure OpenBORDiscCreatorMenuItemClick(Sender: TObject);
    procedure RemoveGameFromSDCardBitBtnClick(Sender: TObject);
    procedure LoadSDCardBitBtnClick(Sender: TObject);
    procedure OpenLocalGamesDirectoriesDialogBitBtnClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SDCardListSelectionChange(Sender: TObject; User: boolean);
    procedure UpdateGamesListSDCardBitBtnClick(Sender: TObject);
    procedure LocalGameDownloadCoverBitBtnClick(Sender: TObject);
    procedure SDCardDownloadCoverBitBtnClick(Sender: TObject);
    function GetCachedCoverImage(game: TGDEmuGame): String;
    procedure UpdateSDCardGamesListMenuItemClick(Sender: TObject);
    procedure DownloadAllCoversClick(Sender: TObject);
  private

  public

  end;

var
  MainWindow: TMainWindow;
  DiskUsageLabel: TLabel = nil; // rótulo de espaço criado em runtime sob a barra
  LibraryView: TGameLibraryView = nil; // lista rica da biblioteca (substitui o TCheckListBox)
  SDCardView: TGameLibraryView = nil; // lista rica do SD Card (substitui o TCheckListBox)

procedure OnFinishGamesCopy;
procedure OnFinishCoversDownload;
procedure UpdateDiskUsageBar;
procedure OnFinishSDCardGamesScan;
procedure OnFinishLocalGamesScan;
procedure UpdateSDCardGameListWithProgress;
procedure RefreshLocalGamesList;
procedure RefreshSDCardList;
procedure LoadLibraryIntoUI;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.OpenLocalGamesDirectoriesDialogBitBtnClick(Sender: TObject);
begin
  LocalGamesDirectoriesDialog.ShowModal;
  if LocalGamesDirectoriesDialog.ModalResult = mrOK then
  begin
    GDEmu.SetLocalGamesDirectories(LocalGamesDirectoriesDialog.DirectoriesListBox.Items);
    ProgressWindow.SetTitle('Scan local game directories');
    GDEmu.StartScanLocalGamesDirectories(@OnFinishLocalGamesScan);
    ProgressWindow.SetMax(GDEmu.CurrentLocalGamesScanActionCount);
    ProgressWindow.ShowProgress;
    Enabled:=False;
  end;
end;

procedure TMainWindow.OpenMenuItemClick(Sender: TObject);
begin

end;

procedure TMainWindow.SDCardListSelectionChange(Sender: TObject; User: boolean);
var
    coverImageFilename: String;
begin
  // User parameter is part of the event signature but not used
  if GDEmu.SDCardGamesListCount > 0 then
  begin
    SDCardGameDiscTypeLabel.Caption:='Extension: ' + SysUtils.UpperCase(GDEmu.SDCardGamesList[SDCardView.ItemIndex].Extension);
    with GDEmu.SDCardGamesList[SDCardView.ItemIndex] do
    begin
      if Genre <> '' then
        SDCardGameDiscTypeLabel.Caption:=SDCardGameDiscTypeLabel.Caption + '   ·   Gênero: ' + Genre;
      if ReleaseYear <> '' then
        SDCardGameDiscTypeLabel.Caption:=SDCardGameDiscTypeLabel.Caption + '   ·   Ano: ' + ReleaseYear;
      if Developer <> '' then
        SDCardGameDiscTypeLabel.Caption:=SDCardGameDiscTypeLabel.Caption + '   ·   Dev: ' + Developer;
    end;
    SDCardGameNameLabel.Caption:='Name: ' + GDEmu.SDCardGamesList[SDCardView.ItemIndex].Name;
    SDCardGamePathLabel.Caption:='Path: ' + GDEmu.SDCardGamesList[SDCardView.ItemIndex].Path;
    SDCardGameIndexLabel.Caption:='Index: ' + Format('[%.2d]',[GDEmu.SDCardGamesList[SDCardView.ItemIndex].Index]);
    SDCardGameMD5Label.Caption:='MD5: ' + GDEmu.SDCardGamesList[SDCardView.ItemIndex].Id;
    
    // Verificar se há capa em cache, senão mostrar padrão
    coverImageFilename:=GetCachedCoverImage(GDEmu.SDCardGamesList[SDCardView.ItemIndex]);
    try
      SDCardCoverImage.Picture.LoadFromFile(coverImageFilename);
    except
      SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
    end;
  end;
end;

procedure TMainWindow.LoadSDCardBitBtnClick(Sender: TObject);
begin
  if OpenSDCardDirectorySelectDirectoryDialog.Execute then
  begin
    GDEmu.SetSDCardGamesDirectory(OpenSDCardDirectorySelectDirectoryDialog.FileName);
    ProgressWindow.SetTitle('Scan SDCard games directories');
    GDEmu.StartScanSDCardGamesDirectories(@OnFinishSDCardGamesScan);
    ProgressWindow.SetMax(GDEmu.CurrentSDCardGamesScanActionCount);
    ProgressWindow.ShowProgress;
    Enabled:=False;
  end;
end;

procedure TMainWindow.CopySelectedBitBtnClick(Sender: TObject);
var i: integer;
    count: integer = 0;
    dupCount: integer = 0;
    resp: integer;
    proceed: Boolean;
    HasSelected: Boolean;
begin
  HasSelected:=False;
  for i:=0 to LibraryView.Count -1 do
  begin
    if LibraryView.Checked[i] then
    begin
      HasSelected:=True;
      Break;
    end;
  end;

  if (HasSelected) and (GDEmu.SDCardLoaded) then
  begin
    // Quantos dos selecionados já estão no SD Card (marca "✓").
    for i:=0 to LibraryView.Count -1 do
      if LibraryView.Checked[i] and (i < GDEmu.LocalGamesListCount) and
         GDEmu.LocalGamesList[i].OnSDCard then
        dupCount:=dupCount + 1;

    proceed:=True;
    GDEmu.DuplicateCopyPolicy:=dpCopyAsNew;
    if dupCount > 0 then
    begin
      resp:=Application.MessageBox(
        PChar(Format(
          '%d dos jogos selecionados já estão no SD Card.' + LineEnding + LineEnding +
          'Sim = Substituir no slot existente' + LineEnding +
          'Não = Ignorar (copiar só os que ainda não estão)' + LineEnding +
          'Cancelar = não copiar nada', [dupCount])),
        'Jogos duplicados', MB_YESNOCANCEL);
      if resp = IDYES then
        GDEmu.DuplicateCopyPolicy:=dpReplace
      else if resp = IDNO then
        GDEmu.DuplicateCopyPolicy:=dpSkip
      else
        proceed:=False;
    end
    else
      proceed:=Application.MessageBox('Do you want to proceed copying the selected games?','Confirmation', MB_YESNO) = IDYES;

    if proceed then
    begin
      GDEmu.ClearSelectedLocalGamesToCopy;
      ProgressWindow.SetTitle('Copying to SD Card');
      MainWindow.Enabled:=False;
      for i:=0 to LibraryView.Count -1 do
      begin
        if LibraryView.Checked[i] then
        begin
          GDEmu.SelectLocalGameToCopy(i);
          count:=count + 1;
        end;
      end;
      GDEmu.StartCopySelectedLocalGamesToSDCard(@OnFinishGamesCopy);
      ProgressWindow.SetMax(count);
      ProgressWindow.ShowProgress;
      GDEmu.UpdateSDCardGameList;
    end;
  end;
end;

procedure TMainWindow.CreditsMenuItemClick(Sender: TObject);
begin
  AboutWindow.ShowModal;
end;

procedure TMainWindow.AboutMenuItemClick(Sender: TObject);
begin

end;

procedure TMainWindow.DownloadCoverMenuItemClick(Sender: TObject);
begin
  if DownloadCoverMenuItem.Checked then
    DownloadCoverMenuItem.Checked:=False
  else
    DownloadCoverMenuItem.Checked:=True;
end;

procedure TMainWindow.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainWindow.LocalGamesListSelectionChange(Sender: TObject;
  User: boolean);
var
    coverImageFilename: String;
begin
  // User parameter is part of the event signature but not used
  if GDEmu.LocalGamesListCount > 0 then
  begin
    LocalGameDiscTypeLabel.Caption:='Extension: ' + SysUtils.UpperCase(GDEmu.LocalGamesList[LibraryView.ItemIndex].Extension);
    with GDEmu.LocalGamesList[LibraryView.ItemIndex] do
    begin
      if Genre <> '' then
        LocalGameDiscTypeLabel.Caption:=LocalGameDiscTypeLabel.Caption + '   ·   Gênero: ' + Genre;
      if ReleaseYear <> '' then
        LocalGameDiscTypeLabel.Caption:=LocalGameDiscTypeLabel.Caption + '   ·   Ano: ' + ReleaseYear;
      if Developer <> '' then
        LocalGameDiscTypeLabel.Caption:=LocalGameDiscTypeLabel.Caption + '   ·   Dev: ' + Developer;
    end;
    LocalGameNameLabel.Caption:='Name: ' + GDEmu.LocalGamesList[LibraryView.ItemIndex].Name;
    LocalGamePathLabel.Caption:='Path: ' + GDEmu.LocalGamesList[LibraryView.ItemIndex].Path;
    LocalGameMD5Label.Caption:='MD5: ' + GDEmu.LocalGamesList[LibraryView.ItemIndex].Id;
    
    // Verificar se há capa em cache, senão mostrar padrão
    coverImageFilename:=GetCachedCoverImage(GDEmu.LocalGamesList[LibraryView.ItemIndex]);
    try
      LocalGameCoverImage.Picture.LoadFromFile(coverImageFilename);
    except
      LocalGameCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
    end;
  end;
end;

procedure TMainWindow.OpenBORDiscCreatorMenuItemClick(Sender: TObject);
begin
  OpenBorCreatorWindow.ShowModal;
end;

procedure TMainWindow.RemoveGameFromSDCardBitBtnClick(Sender: TObject);
var i: integer;
    HasSelected: Boolean;
begin
  HasSelected:=False;
  for i:=0 to SDCardView.Count -1 do
  begin
    if SDCardView.Checked[i] then
    begin
      HasSelected:=True;
      Break;
    end;
  end;

  if (HasSelected)  and (GDEmu.SDCardLoaded) then
  begin
    GDEmu.ClearSelectedSDCardGamesToRemove;
    if Application.MessageBox('Do you want to remove the selected Games?','Confirmation', MB_YESNO) = mrYes then
    begin
      for i:=0 to SDCardView.Count -1 do
      begin
        if SDCardView.Checked[i] then
        begin
          GDEmu.SelectSDCardGameToRemove(i);
        end;
      end;
      GDEmu.RemoveFromSDCard;
      RefreshSDCardList;
    end;
    UpdateSDCardGameListWithProgress;
  end;
end;

procedure UpdateSDCardGameListWithProgress;
begin
  if GDEmu.SDCardLoaded then
  begin
    // Limpar log anterior
    GDEmu.ClearCommandLog;
    
    // Mostrar janela de progresso simples
    ProgressWindow.SetTitle('Updating GDEmu Game List');
    ProgressWindow.SetMax(100);
    ProgressWindow.ProgressBar.Position:=0;
    ProgressWindow.TextLabel.Caption:='Generating menu CDI...';
    ProgressWindow.ShowProgress;
    MainWindow.Enabled:=False;
    Application.ProcessMessages;
    
    // Executar atualização (isso vai gerar logs dos comandos)
    // Os comandos vão registrar logs automaticamente via RunCommandWithLog
    GDEmu.UpdateSDCardGameList;
    
    // Fechar janela de progresso
    ProgressWindow.CloseProgress;
    MainWindow.Enabled:=True;
    
    // Mostrar janela de log modal com os resultados
    CommandLogWindow.ShowLog('GDEmu Game List Update - Command Log', GDEmu.GetCommandLog);
  end;
end;

procedure TMainWindow.UpdateGamesListSDCardBitBtnClick(Sender: TObject);
begin
  if GDEmu.SDCardLoaded then
  begin
    UpdateSDCardGameListWithProgress;
    ShowMessage('GDEmu Game list has updated!');
    //GDEmu.ScanSDCardGamesDirectory;
    //SDCardList.Clear;
    //for i:=0 to GDEmu.SDCardGamesListCount -1 do
    //begin
    //  SDCardList.AddItem(GDEmu.SDCardGamesList[i].Name,nil);
    //end;
  end;
end;

procedure TMainWindow.UpdateSDCardGamesListMenuItemClick(Sender: TObject);
begin

end;

// Baixa as capas de toda a biblioteca em lote (worker thread + barra de progresso).
procedure TMainWindow.DownloadAllCoversClick(Sender: TObject);
begin
  if GDEmu.LocalGamesListCount > 0 then
  begin
    if Application.MessageBox(
        PChar(Format('Baixar capas para os %d jogos da biblioteca?' + LineEnding +
          'Os que já têm capa em cache são pulados.', [GDEmu.LocalGamesListCount])),
        'Download de capas', MB_YESNO) = IDYES then
    begin
      ProgressWindow.SetTitle('Baixando capas da biblioteca');
      ProgressWindow.SetMax(GDEmu.LocalGamesListCount);
      GDEmu.StartDownloadAllLocalCovers(@OnFinishCoversDownload);
      ProgressWindow.ShowProgress;
      Enabled:=False;
    end;
  end;
end;

// Fim do download em lote: recarrega as listas com as capas novas.
procedure OnFinishCoversDownload;
begin
  if LibraryView <> nil then LibraryView.ClearThumbCache;
  if SDCardView <> nil then SDCardView.ClearThumbCache;
  RefreshLocalGamesList;
  if GDEmu.SDCardLoaded then RefreshSDCardList;
  MainWindow.Enabled:=True;
end;

procedure OnFinishGamesCopy;
begin
  // UpdateSDCardGameList já foi chamado com progresso durante a cópia
  // Não precisa chamar novamente aqui, mas vamos manter para garantir
  GDEmu.ScanSDCardGamesDirectory;
  RefreshSDCardList;
  // O SD mudou após a cópia: re-marcar a biblioteca local.
  GDEmu.MarkLocalGamesPresentOnSDCard;
  RefreshLocalGamesList;
  UpdateDiskUsageBar;
  MainWindow.Enabled:=True;
end;

// Formata bytes em GB/MB legível.
function HumanSize(bytes: Int64): String;
const GB = Int64(1024) * 1024 * 1024; MB = Int64(1024) * 1024;
begin
  if bytes >= GB then
    Result:=FormatFloat('0.0', bytes / GB) + ' GB'
  else
    Result:=FormatFloat('0', bytes / MB) + ' MB';
end;

// Atualiza a barra de uso de disco do cartão (usado/total) + rótulo de texto.
procedure UpdateDiskUsageBar;
var total, free, used: Int64;
    pct: integer;
    txt: String;
begin
  if GDEmu.SDCardGamesDirectory = '' then Exit;
  if not GDEmu.GetDiskSpace(GDEmu.SDCardGamesDirectory, total, free) then Exit;
  if total <= 0 then Exit;
  used:=total - free;
  pct:=Round(used * 100 / total);
  MainWindow.DiskUseProgressBar.Min:=0;
  MainWindow.DiskUseProgressBar.Max:=100;
  MainWindow.DiskUseProgressBar.Position:=pct;
  txt:=Format('Cartão: %s de %s usados (%d%%)  ·  %s livres',
    [HumanSize(used), HumanSize(total), pct, HumanSize(free)]);
  MainWindow.DiskUseProgressBar.ShowHint:=True;
  MainWindow.DiskUseProgressBar.Hint:=txt;
  if DiskUsageLabel = nil then
  begin
    DiskUsageLabel:=TLabel.Create(MainWindow);
    DiskUsageLabel.Parent:=MainWindow.DiskUseProgressBar.Parent;
    DiskUsageLabel.Top:=MainWindow.DiskUseProgressBar.Top + MainWindow.DiskUseProgressBar.Height + 1;
    DiskUsageLabel.Align:=alTop;
    DiskUsageLabel.Alignment:=taCenter;
    DiskUsageLabel.BorderSpacing.Around:=2;
  end;
  DiskUsageLabel.Caption:=txt;
end;

// Repopula a lista da biblioteca (esquerda) marcando com "✓ " os jogos que já
// estão no SD Card (cruzamento por MD5 do IP.BIN feito em MarkLocalGamesPresentOnSDCard).
procedure RefreshLocalGamesList;
var i: integer;
    g: TGDEmuGame;
begin
  // Cria o componente rico na primeira chamada, no lugar do TCheckListBox antigo
  // (que continua no .lfm, apenas escondido e sem uso).
  if LibraryView = nil then
  begin
    LibraryView:=TGameLibraryView.Create(MainWindow);
    LibraryView.Parent:=MainWindow.LocalGamesList.Parent;
    // Tira o list antigo do alinhamento ANTES (dois alClient no mesmo painel
    // conflitam no LCL e o novo ficaria com tamanho zero / invisível).
    MainWindow.LocalGamesList.Align:=alNone;
    MainWindow.LocalGamesList.Visible:=False;
    LibraryView.Align:=alClient;
    LibraryView.OnSelectionChange:=@MainWindow.LocalGamesListSelectionChange;
  end;
  LibraryView.ClearGames;
  for i:=0 to GDEmu.LocalGamesListCount -1 do
  begin
    g:=GDEmu.LocalGamesList[i];
    LibraryView.AddGame(g.Name, g.Genre, g.ReleaseYear, g.Developer,
      MainWindow.GetCachedCoverImage(g), g.OnSDCard);
  end;
  LibraryView.Invalidate;
end;

// Mesmo componente rico no lado do SD Card. Jogos do SD já estão lá, então não
// recebem o badge "no SD" (passa OnSDCard=False).
procedure RefreshSDCardList;
var i: integer;
    g: TGDEmuGame;
begin
  if SDCardView = nil then
  begin
    SDCardView:=TGameLibraryView.Create(MainWindow);
    SDCardView.Parent:=MainWindow.SDCardList.Parent;
    MainWindow.SDCardList.Align:=alNone; // evita conflito de dois alClient
    MainWindow.SDCardList.Visible:=False;
    SDCardView.Align:=alClient;
    SDCardView.OnSelectionChange:=@MainWindow.SDCardListSelectionChange;
  end;
  SDCardView.ClearGames;
  for i:=0 to GDEmu.SDCardGamesListCount -1 do
  begin
    g:=GDEmu.SDCardGamesList[i];
    SDCardView.AddGame(g.Name, g.Genre, g.ReleaseYear, g.Developer,
      MainWindow.GetCachedCoverImage(g), False);
  end;
  SDCardView.Invalidate;
end;

// Carrega a biblioteca persistida (library.json) na inicialização, sem re-scan:
// repopula o diálogo de diretórios e a lista da esquerda.
procedure LoadLibraryIntoUI;
var mi: TMenuItem;
begin
  if GDEmu.LoadLibrary then
  begin
    LocalGamesDirectoriesDialog.DirectoriesListBox.Items.Assign(GDEmu.LocalGamesDirectoriesList);
    GDEmu.MarkLocalGamesPresentOnSDCard; // SD ainda não carregado: só limpa marcas
  end;
  RefreshLocalGamesList; // sempre cria o LibraryView (vazio se não houver biblioteca)

  // Item de menu (runtime) para o download de capas em lote.
  mi:=TMenuItem.Create(MainWindow);
  mi.Caption:='Baixar capas da biblioteca';
  mi.OnClick:=@MainWindow.DownloadAllCoversClick;
  MainWindow.ToolsMenuItem.Add(mi);
end;

procedure OnFinishSDCardGamesScan;
begin
  RefreshSDCardList;
  // Carregar o SD pode marcar jogos da biblioteca já carregada como duplicados.
  GDEmu.MarkLocalGamesPresentOnSDCard;
  RefreshLocalGamesList;
  UpdateDiskUsageBar;
  MainWindow.Enabled:=True;
end;

procedure OnFinishLocalGamesScan;
begin
  GDEmu.MarkLocalGamesPresentOnSDCard;
  RefreshLocalGamesList;
  MainWindow.Enabled:=True;
end;

function TMainWindow.GetCachedCoverImage(game: TGDEmuGame): String;
var
  cacheFilename: String;
  coverFileTest: TPicture;
  coverFileCheck: TFileStream;
  imageSize: Int64;
begin
  Result:=ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']); // Default
  // .png do libretro tem prioridade sobre o .jpg legado.
  cacheFilename:=ConcatPaths([GDEmu.ApplicationPath,'cache',game.SlugName + '.png']);
  if not FileExists(cacheFilename) then
    cacheFilename:=ConcatPaths([GDEmu.ApplicationPath,'cache',game.SlugName + '.jpg']);

  // Verificar se existe no cache e é válido
  if FileExists(cacheFilename) then
  begin
    coverFileTest:=TPicture.Create;
    coverFileCheck:=nil;
    try
      coverFileCheck:=TFileStream.Create(cacheFilename, fmOpenRead or fmShareDenyWrite);
      try
        imageSize:=coverFileCheck.Size;
        if imageSize > 0 then
        begin
          coverFileCheck.Position:=0;
          coverFileTest.LoadFromStream(coverFileCheck);
          if (coverFileTest.Width > 0) and (coverFileTest.Height > 0) then
          begin
            Result:=cacheFilename;
          end;
        end;
      finally
        if coverFileCheck <> nil then
          coverFileCheck.Free;
      end;
    except
      // Se houver erro, usar imagem padrão
      Result:=ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']);
    end;
    coverFileTest.Free;
  end;
end;

procedure TMainWindow.LocalGameDownloadCoverBitBtnClick(Sender: TObject);
var
  coverImageFilename: String;
begin
  if GDEmu.LocalGamesListCount > 0 then
  begin
    LocalGameDownloadCoverBitBtn.Enabled:=False;
    try
      Application.ProcessMessages;
      coverImageFilename:=GDEmu.GetGameCover(GDEmu.LocalGamesList[LibraryView.ItemIndex]);
      if coverImageFilename <> '' then
      begin
        try
          LocalGameCoverImage.Picture.LoadFromFile(coverImageFilename);
        except
          LocalGameCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
        end;
      end;
    finally
      LocalGameDownloadCoverBitBtn.Enabled:=True;
    end;
  end;
end;

procedure TMainWindow.SDCardDownloadCoverBitBtnClick(Sender: TObject);
var
  coverImageFilename: String;
begin
  if GDEmu.SDCardGamesListCount > 0 then
  begin
    SDCardDownloadCoverBitBtn.Enabled:=False;
    try
      Application.ProcessMessages;
      coverImageFilename:=GDEmu.GetGameCover(GDEmu.SDCardGamesList[SDCardView.ItemIndex]);
      if coverImageFilename <> '' then
      begin
        try
          SDCardCoverImage.Picture.LoadFromFile(coverImageFilename);
        except
          SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath ,'data','gdrom.png']));
        end;
      end;
    finally
      SDCardDownloadCoverBitBtn.Enabled:=True;
    end;
  end;
end;

end.

