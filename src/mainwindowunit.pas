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
    procedure DownloadAllSDCoversClick(Sender: TObject);
    procedure TagCardGamesClick(Sender: TObject);
    procedure RenameCardClick(Sender: TObject);
    procedure CloseSDCardClick(Sender: TObject);
    procedure ScrapeOptionClick(Sender: TObject);
    procedure ShowLocalImage;
    procedure ShowSDImage;
    procedure LocalCoverCycle(Sender: TObject);
    procedure SDCoverCycle(Sender: TObject);
  private

  public

  end;

var
  MainWindow: TMainWindow;
  DiskUsageLabel: TLabel = nil; // rótulo de espaço criado em runtime sob a barra
  LibraryView: TGameLibraryView = nil; // lista rica da biblioteca (substitui o TCheckListBox)
  SDCardView: TGameLibraryView = nil; // lista rica do SD Card (substitui o TCheckListBox)
  AutoDownloadCovers: Boolean = False; // preferência: baixar capas ao fim de cada scan
  ToolButtonLeft: integer = 1000; // ordena os botões de toolbar criados em runtime
  LocalImgType: integer = 0; // imagem mostrada no painel local: 0=capa 1=título 2=snap
  SDImgType: integer = 0;
  LocalImgLabel: TLabel = nil; // rótulo do tipo de imagem sob a capa
  SDImgLabel: TLabel = nil;

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

// --- Helpers de imagem do jogo (capa/título/screenshot) ---

// Sufixo de cache por tipo de imagem (0=capa, 1=título, 2=snapshot).
function CoverSuffix(t: integer): String;
begin
  case t of 1: Result:='-title'; 2: Result:='-snap'; else Result:=''; end;
end;

function CoverTypeName(t: integer): String;
begin
  case t of 1: Result:='Tela de título'; 2: Result:='Screenshot'; else Result:='Capa'; end;
end;

// Caminho da imagem do tipo t no cache, ou '' se não existir. Resolve pelo próprio
// slug e, se faltar, por outro jogo com o mesmo Id (reaproveita biblioteca<->SD).
function GameImageFileFor(game: TGDEmuGame; t: integer): String;
begin
  Result:=GDEmu.ResolveImageFile(game, CoverSuffix(t));
end;

// Primeiro tipo com imagem (preferindo capa); 0 se nenhum (mostra o gd-rom padrão).
function FirstImageType(game: TGDEmuGame): integer;
begin
  if GameImageFileFor(game, 0) <> '' then Exit(0);
  if GameImageFileFor(game, 1) <> '' then Exit(1);
  if GameImageFileFor(game, 2) <> '' then Exit(2);
  Result:=0;
end;

// Próximo tipo com imagem após 'cur' (ciclando); devolve 'cur' se não houver outro.
function NextImageType(game: TGDEmuGame; cur: integer): integer;
var i, t: integer;
begin
  Result:=cur;
  for i:=1 to 3 do
  begin
    t:=(cur + i) mod 3;
    if GameImageFileFor(game, t) <> '' then Exit(t);
  end;
end;

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
begin
  // User parameter is part of the event signature but not used
  if (GDEmu.SDCardGamesListCount > 0) and (SDCardView <> nil) and
     (SDCardView.ItemIndex >= 0) and (SDCardView.ItemIndex < GDEmu.SDCardGamesListCount) then
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
    
    // Imagem: primeiro tipo disponível (capa preferida); clique alterna título/snap.
    SDImgType:=FirstImageType(GDEmu.SDCardGamesList[SDCardView.ItemIndex]);
    ShowSDImage;
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

// Toggle "Baixar capas automaticamente" — guarda o estado na variável global
// AutoDownloadCovers (desacoplado do widget, pois o menu é montado em runtime).
procedure TMainWindow.DownloadCoverMenuItemClick(Sender: TObject);
begin
  AutoDownloadCovers:=not AutoDownloadCovers;
  if Sender is TMenuItem then
    TMenuItem(Sender).Checked:=AutoDownloadCovers;
end;

procedure TMainWindow.ExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainWindow.LocalGamesListSelectionChange(Sender: TObject;
  User: boolean);
begin
  // User parameter is part of the event signature but not used
  if (GDEmu.LocalGamesListCount > 0) and (LibraryView <> nil) and
     (LibraryView.ItemIndex >= 0) and (LibraryView.ItemIndex < GDEmu.LocalGamesListCount) then
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
    
    // Imagem: primeiro tipo disponível (capa preferida); clique alterna título/snap.
    LocalImgType:=FirstImageType(GDEmu.LocalGamesList[LibraryView.ItemIndex]);
    ShowLocalImage;
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

// Marca, na biblioteca, todos os jogos presentes no cartão carregado como
// copiados nele (backfill — funciona para cartões já cheios de antes).
procedure TMainWindow.TagCardGamesClick(Sender: TObject);
begin
  if not GDEmu.SDCardLoaded then
  begin
    ShowMessage('Carregue um cartão SD primeiro.');
    Exit;
  end;
  if GDEmu.CurrentSDCardId = '' then
  begin
    ShowMessage('Este cartão ainda não tem identidade. Recarregue-o para nomeá-lo.');
    Exit;
  end;
  GDEmu.TagGamesOnCurrentCard;
  RefreshLocalGamesList;
  ShowMessage(Format('Jogos presentes em "%s" marcados na biblioteca.',
    [GDEmu.CurrentSDCardLabel]));
end;

// Renomeia o cartão carregado (atualiza o arquivo no cartão e o registro).
procedure TMainWindow.RenameCardClick(Sender: TObject);
var cardName: String;
begin
  if not GDEmu.SDCardLoaded then
  begin
    ShowMessage('Carregue um cartão SD primeiro.');
    Exit;
  end;
  cardName:=GDEmu.CurrentSDCardLabel;
  if InputQuery('Renomear cartão', 'Nome do cartão:', cardName) then
  begin
    GDEmu.SaveSDCardIdentity(cardName);
    GDEmu.SaveLibrary;     // persiste o registro de cartões
    RefreshLocalGamesList; // chips com o novo nome
  end;
end;

// Liga/desliga uma opção do scraper (Tag: 1=boxart,2=title,3=snap,4=overwrite)
// e persiste a config na biblioteca.
procedure TMainWindow.ScrapeOptionClick(Sender: TObject);
var mi: TMenuItem;
begin
  if not (Sender is TMenuItem) then Exit;
  mi:=TMenuItem(Sender);
  case mi.Tag of
    1: GDEmu.ScrapeBoxart:=not GDEmu.ScrapeBoxart;
    2: GDEmu.ScrapeTitle:=not GDEmu.ScrapeTitle;
    3: GDEmu.ScrapeSnap:=not GDEmu.ScrapeSnap;
    4: GDEmu.ScrapeOverwrite:=not GDEmu.ScrapeOverwrite;
  end;
  case mi.Tag of
    1: mi.Checked:=GDEmu.ScrapeBoxart;
    2: mi.Checked:=GDEmu.ScrapeTitle;
    3: mi.Checked:=GDEmu.ScrapeSnap;
    4: mi.Checked:=GDEmu.ScrapeOverwrite;
  end;
  GDEmu.SaveLibrary; // persiste a config do scraper
end;

// "Fecha" o cartão atual para carregar outro sem reiniciar o app.
procedure TMainWindow.CloseSDCardClick(Sender: TObject);
begin
  if not GDEmu.SDCardLoaded then
  begin
    ShowMessage('Nenhum cartão carregado.');
    Exit;
  end;
  GDEmu.CloseSDCard;
  if SDCardView <> nil then
  begin
    SDCardView.ClearGames;
    SDCardView.Invalidate;
  end;
  GDEmu.MarkLocalGamesPresentOnSDCard; // SD vazio: limpa as marcas "✓ no SD"
  RefreshLocalGamesList;
  // Reseta a barra de uso de disco e limpa as labels de info do SD.
  DiskUseProgressBar.Position:=0;
  if DiskUsageLabel <> nil then DiskUsageLabel.Caption:='';
  SDCardGameNameLabel.Caption:='';
  SDCardGameDiscTypeLabel.Caption:='';
  SDCardGamePathLabel.Caption:='';
  SDCardGameIndexLabel.Caption:='';
  SDCardGameMD5Label.Caption:='';
end;

// Download de capas em lote para os jogos do SD Card (manual, sob demanda).
procedure TMainWindow.DownloadAllSDCoversClick(Sender: TObject);
begin
  if GDEmu.SDCardGamesListCount > 0 then
  begin
    if Application.MessageBox(
        PChar(Format('Baixar capas para os %d jogos do SD Card?' + LineEnding +
          'Os que já têm capa em cache são pulados.', [GDEmu.SDCardGamesListCount])),
        'Download de capas', MB_YESNO) = IDYES then
    begin
      ProgressWindow.SetTitle('Baixando capas do SD Card');
      ProgressWindow.SetMax(GDEmu.SDCardGamesListCount);
      GDEmu.StartDownloadAllSDCovers(@OnFinishCoversDownload);
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

// Junta os rótulos dos cartões onde o jogo já foi copiado, para o chip do card.
function CardsLabelFor(game: TGDEmuGame): String;
var ids: TStringList; i: integer;
begin
  Result:='';
  if game.CopiedToCardIds = '' then Exit;
  ids:=TStringList.Create;
  try
    ids.Delimiter:='|';
    ids.StrictDelimiter:=True;
    ids.DelimitedText:=game.CopiedToCardIds;
    for i:=0 to ids.Count -1 do
    begin
      if Result <> '' then Result:=Result + ', ';
      Result:=Result + GDEmu.CardLabel(ids[i]);
    end;
  finally
    ids.Free;
  end;
end;

// Caminho da capa (thumbnail) do jogo: boxart própria ou de outro jogo com mesmo
// Id; se não houver, o gd-rom padrão.
function ThumbPathFor(game: TGDEmuGame): String;
begin
  Result:=GameImageFileFor(game, 0);
  if Result = '' then Result:=ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']);
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
      ThumbPathFor(g), g.OnSDCard, CardsLabelFor(g));
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
      ThumbPathFor(g), False);
  end;
  SDCardView.Invalidate;
end;

// Carrega a biblioteca persistida (library.json) na inicialização, sem re-scan:
// repopula o diálogo de diretórios e a lista da esquerda.
// Cria um item de menu já parentado e com handler.
function AddMenu(AParent: TMenuItem; const ACaption: String;
  AHandler: TNotifyEvent): TMenuItem;
begin
  Result:=TMenuItem.Create(MainWindow);
  Result.Caption:=ACaption;
  Result.OnClick:=AHandler;
  AParent.Add(Result);
end;

function AddSeparator(AParent: TMenuItem): TMenuItem;
begin
  Result:=TMenuItem.Create(MainWindow);
  Result.Caption:='-';
  AParent.Add(Result);
end;

// Reconstrói a barra de menus do zero, com grupos EXPLÍCITOS: o que é da
// Biblioteca de Jogos e o que é do SD Card ficam separados e nomeados. Os menus
// originais do .lfm (confusos e com handlers trocados) são descartados.
procedure BuildMainMenu;
var top, autoItem, scraper, mi: TMenuItem;
begin
  MainWindow.MainWindowMenu.Items.Clear;

  // Arquivo
  top:=AddMenu(MainWindow.MainWindowMenu.Items, 'Arquivo', nil);
  AddMenu(top, 'Sair', @MainWindow.ExitMenuItemClick);

  // Biblioteca de Jogos (lado esquerdo)
  top:=AddMenu(MainWindow.MainWindowMenu.Items, 'Biblioteca de Jogos', nil);
  AddMenu(top, 'Adicionar diretórios de jogos…', @MainWindow.OpenLocalGamesDirectoriesDialogBitBtnClick);
  AddMenu(top, 'Scrapar biblioteca', @MainWindow.DownloadAllCoversClick);

  // SD Card (lado direito)
  top:=AddMenu(MainWindow.MainWindowMenu.Items, 'SD Card', nil);
  AddMenu(top, 'Carregar SD Card…', @MainWindow.LoadSDCardBitBtnClick);
  AddMenu(top, 'Fechar SD Card', @MainWindow.CloseSDCardClick);
  AddMenu(top, 'Renomear este cartão…', @MainWindow.RenameCardClick);
  AddSeparator(top);
  AddMenu(top, 'Marcar jogos deste cartão na biblioteca', @MainWindow.TagCardGamesClick);
  AddSeparator(top);
  AddMenu(top, 'Copiar selecionados para o SD Card', @MainWindow.CopySelectedBitBtnClick);
  AddMenu(top, 'Remover jogo do SD Card', @MainWindow.RemoveGameFromSDCardBitBtnClick);
  AddMenu(top, 'Atualizar lista de jogos (GDMenu)', @MainWindow.UpdateGamesListSDCardBitBtnClick);
  AddSeparator(top);
  AddMenu(top, 'Scrapar SD Card', @MainWindow.DownloadAllSDCoversClick);

  // Ferramentas
  top:=AddMenu(MainWindow.MainWindowMenu.Items, 'Ferramentas', nil);
  AddMenu(top, 'Criar disco OpenBOR', @MainWindow.OpenBORDiscCreatorMenuItemClick);
  AddSeparator(top);
  // Submenu Scraper — o usuário escolhe o que baixar.
  scraper:=AddMenu(top, 'Scraper', nil);
  mi:=AddMenu(scraper, 'Baixar capa (boxart)', @MainWindow.ScrapeOptionClick);
  mi.Tag:=1; mi.ShowAlwaysCheckable:=True; mi.Checked:=GDEmu.ScrapeBoxart;
  mi:=AddMenu(scraper, 'Baixar tela de título', @MainWindow.ScrapeOptionClick);
  mi.Tag:=2; mi.ShowAlwaysCheckable:=True; mi.Checked:=GDEmu.ScrapeTitle;
  mi:=AddMenu(scraper, 'Baixar screenshot', @MainWindow.ScrapeOptionClick);
  mi.Tag:=3; mi.ShowAlwaysCheckable:=True; mi.Checked:=GDEmu.ScrapeSnap;
  AddSeparator(scraper);
  mi:=AddMenu(scraper, 'Sobrescrever existentes', @MainWindow.ScrapeOptionClick);
  mi.Tag:=4; mi.ShowAlwaysCheckable:=True; mi.Checked:=GDEmu.ScrapeOverwrite;
  autoItem:=AddMenu(scraper, 'Scraping automático ao escanear',
    @MainWindow.DownloadCoverMenuItemClick);
  autoItem.ShowAlwaysCheckable:=True;
  autoItem.Checked:=AutoDownloadCovers;

  // Ajuda
  top:=AddMenu(MainWindow.MainWindowMenu.Items, 'Ajuda', nil);
  AddMenu(top, 'Sobre', @MainWindow.AboutMenuItemClick);
  AddMenu(top, 'Créditos', @MainWindow.CreditsMenuItemClick);
end;

// Cria um botão de toolbar (ícone 24px de data/icons + Hint) no painel, à esquerda.
procedure AddToolButton(panel: TWinControl; const icon, hint: String;
  handler: TNotifyEvent);
var b: TBitBtn; png: TPortableNetworkGraphic; p: String;
begin
  b:=TBitBtn.Create(MainWindow);
  b.Parent:=panel;
  b.Align:=alLeft;
  // Left crescente só ordena o grupo alLeft: novos botões ficam depois dos
  // existentes (que têm Left baixo) e entre si na ordem de criação.
  b.Left:=ToolButtonLeft;
  Inc(ToolButtonLeft, 50);
  b.Width:=38;
  b.Height:=40;
  b.Hint:=hint;
  b.ShowHint:=True;
  p:=ConcatPaths([GDEmu.ApplicationPath,'data','icons',icon]);
  if FileExists(p) then
  begin
    png:=TPortableNetworkGraphic.Create;
    try
      png.LoadFromFile(p);
      b.Glyph.Assign(png);
    finally
      png.Free;
    end;
  end;
  b.OnClick:=handler;
end;

// Adiciona, em runtime, os botões que espelham as ações de menu, em cada toolbar.
procedure BuildToolbars;
begin
  // Biblioteca de Jogos (esquerda). Alinha o botão de pasta à esquerda p/ empacotar.
  MainWindow.OpenLocalGamesDirectoriesDialogBitBtn.Align:=alLeft;
  AddToolButton(MainWindow.LocalGamesToolsButtonsPanel, 'covers.png',
    'Scrapar biblioteca', @MainWindow.DownloadAllCoversClick);

  // SD Card (direita).
  AddToolButton(MainWindow.SDCardGamesToolsButtonsPanel, 'eject.png',
    'Fechar SD Card', @MainWindow.CloseSDCardClick);
  AddToolButton(MainWindow.SDCardGamesToolsButtonsPanel, 'mark.png',
    'Marcar jogos deste cartão na biblioteca', @MainWindow.TagCardGamesClick);
  AddToolButton(MainWindow.SDCardGamesToolsButtonsPanel, 'rename.png',
    'Renomear este cartão', @MainWindow.RenameCardClick);
  AddToolButton(MainWindow.SDCardGamesToolsButtonsPanel, 'covers.png',
    'Scrapar SD Card', @MainWindow.DownloadAllSDCoversClick);
end;

procedure TMainWindow.ShowLocalImage;
var game: TGDEmuGame; f: String;
begin
  if (LibraryView = nil) or (LibraryView.ItemIndex < 0) or
     (LibraryView.ItemIndex >= GDEmu.LocalGamesListCount) then Exit;
  game:=GDEmu.LocalGamesList[LibraryView.ItemIndex];
  f:=GameImageFileFor(game, LocalImgType);
  if f = '' then f:=ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']);
  try
    LocalGameCoverImage.Picture.LoadFromFile(f);
  except
    LocalGameCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']));
  end;
  if LocalImgLabel <> nil then
    if NextImageType(game, LocalImgType) <> LocalImgType then
      LocalImgLabel.Caption:=CoverTypeName(LocalImgType) + '  ▸'
    else
      LocalImgLabel.Caption:=CoverTypeName(LocalImgType);
end;

procedure TMainWindow.ShowSDImage;
var game: TGDEmuGame; f: String;
begin
  if (SDCardView = nil) or (SDCardView.ItemIndex < 0) or
     (SDCardView.ItemIndex >= GDEmu.SDCardGamesListCount) then Exit;
  game:=GDEmu.SDCardGamesList[SDCardView.ItemIndex];
  f:=GameImageFileFor(game, SDImgType);
  if f = '' then f:=ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']);
  try
    SDCardCoverImage.Picture.LoadFromFile(f);
  except
    SDCardCoverImage.Picture.LoadFromFile(ConcatPaths([GDEmu.ApplicationPath,'data','gdrom.png']));
  end;
  if SDImgLabel <> nil then
    if NextImageType(game, SDImgType) <> SDImgType then
      SDImgLabel.Caption:=CoverTypeName(SDImgType) + '  ▸'
    else
      SDImgLabel.Caption:=CoverTypeName(SDImgType);
end;

procedure TMainWindow.LocalCoverCycle(Sender: TObject);
begin
  if (LibraryView = nil) or (LibraryView.ItemIndex < 0) or
     (LibraryView.ItemIndex >= GDEmu.LocalGamesListCount) then Exit;
  LocalImgType:=NextImageType(GDEmu.LocalGamesList[LibraryView.ItemIndex], LocalImgType);
  ShowLocalImage;
end;

procedure TMainWindow.SDCoverCycle(Sender: TObject);
begin
  if (SDCardView = nil) or (SDCardView.ItemIndex < 0) or
     (SDCardView.ItemIndex >= GDEmu.SDCardGamesListCount) then Exit;
  SDImgType:=NextImageType(GDEmu.SDCardGamesList[SDCardView.ItemIndex], SDImgType);
  ShowSDImage;
end;

// Liga o clique-para-alternar nas capas e cria os rótulos de tipo sob elas.
procedure BuildImageSwitchers;
begin
  // O antigo "Download Cover" agora força re-obter todo o metadado do jogo.
  MainWindow.LocalGameDownloadCoverBitBtn.Caption:='Download MetaData';
  MainWindow.LocalGameDownloadCoverBitBtn.Hint:='Força re-baixar/re-extrair todo o metadado deste jogo';
  MainWindow.LocalGameDownloadCoverBitBtn.ShowHint:=True;
  MainWindow.SDCardDownloadCoverBitBtn.Caption:='Download MetaData';
  MainWindow.SDCardDownloadCoverBitBtn.Hint:='Força re-baixar/re-extrair todo o metadado deste jogo';
  MainWindow.SDCardDownloadCoverBitBtn.ShowHint:=True;

  MainWindow.LocalGameCoverImage.OnClick:=@MainWindow.LocalCoverCycle;
  MainWindow.LocalGameCoverImage.Cursor:=crHandPoint;
  MainWindow.LocalGameCoverImage.ShowHint:=True;
  MainWindow.LocalGameCoverImage.Hint:='Clique para alternar capa / título / screenshot';
  LocalImgLabel:=TLabel.Create(MainWindow);
  LocalImgLabel.Parent:=MainWindow.LocalGameInfoPanel;
  LocalImgLabel.SetBounds(10, 148, 154, 16); // faixa-legenda sobre a base da imagem
  LocalImgLabel.Alignment:=taCenter;
  LocalImgLabel.Transparent:=False;
  LocalImgLabel.Color:=RGBToColor(40, 40, 40);
  LocalImgLabel.Font.Color:=clWhite;
  LocalImgLabel.Cursor:=crHandPoint;
  LocalImgLabel.OnClick:=@MainWindow.LocalCoverCycle;

  MainWindow.SDCardCoverImage.OnClick:=@MainWindow.SDCoverCycle;
  MainWindow.SDCardCoverImage.Cursor:=crHandPoint;
  MainWindow.SDCardCoverImage.ShowHint:=True;
  MainWindow.SDCardCoverImage.Hint:='Clique para alternar capa / título / screenshot';
  SDImgLabel:=TLabel.Create(MainWindow);
  SDImgLabel.Parent:=MainWindow.SDCardGameInfoPanel;
  SDImgLabel.SetBounds(10, 148, 154, 16);
  SDImgLabel.Alignment:=taCenter;
  SDImgLabel.Transparent:=False;
  SDImgLabel.Color:=RGBToColor(40, 40, 40);
  SDImgLabel.Font.Color:=clWhite;
  SDImgLabel.Cursor:=crHandPoint;
  SDImgLabel.OnClick:=@MainWindow.SDCoverCycle;
end;

procedure LoadLibraryIntoUI;
begin
  if GDEmu.LoadLibrary then
  begin
    LocalGamesDirectoriesDialog.DirectoriesListBox.Items.Assign(GDEmu.LocalGamesDirectoriesList);
    GDEmu.MarkLocalGamesPresentOnSDCard; // SD ainda não carregado: só limpa marcas
  end;
  RefreshLocalGamesList; // sempre cria o LibraryView (vazio se não houver biblioteca)
  BuildMainMenu;         // menu reorganizado: Biblioteca vs SD Card explícitos
  BuildToolbars;         // botões com ícones espelhando os menus
  BuildImageSwitchers;   // capa clicável p/ alternar título/screenshot
end;

procedure OnFinishSDCardGamesScan;
var cardName: String;
    doScrape: Boolean;
begin
  // Identidade do cartão: lê de gdemugui-card.json; se for um cartão novo (sem
  // arquivo), pede um nome e cria a identidade (vale também p/ cartões já cheios).
  if not GDEmu.LoadSDCardIdentity then
  begin
    cardName:='Cartão SD';
    InputQuery('Cartão SD novo', 'Dê um nome a este cartão (para etiquetar os jogos):', cardName);
    GDEmu.SaveSDCardIdentity(cardName);
  end;

  RefreshSDCardList;
  // Carregar o SD pode marcar jogos da biblioteca já carregada como duplicados.
  GDEmu.MarkLocalGamesPresentOnSDCard;
  RefreshLocalGamesList;
  UpdateDiskUsageBar;

  // Scraping ao carregar: pode demorar, então é uma decisão explícita. Se o
  // "Scraping automático" estiver ligado, roda direto; senão, pergunta agora.
  doScrape:=False;
  if GDEmu.SDCardGamesListCount > 0 then
  begin
    if AutoDownloadCovers then
      doScrape:=True
    else if GDEmu.ScrapeBoxart or GDEmu.ScrapeTitle or GDEmu.ScrapeSnap then
      doScrape:=Application.MessageBox(
        PChar('Baixar as imagens (scraping) dos jogos deste cartão agora?' + LineEnding +
          'Pode demorar — dá para fazer depois em SD Card > Scrapar SD Card.'),
        'Scraper', MB_YESNO) = IDYES;
  end;

  if doScrape then
  begin
    ProgressWindow.SetTitle('Scrapando o SD Card');
    ProgressWindow.SetMax(GDEmu.SDCardGamesListCount);
    GDEmu.StartDownloadAllSDCovers(@OnFinishCoversDownload);
    ProgressWindow.ShowProgress;
    MainWindow.Enabled:=False;
  end
  else
    MainWindow.Enabled:=True;
end;

procedure OnFinishLocalGamesScan;
begin
  GDEmu.MarkLocalGamesPresentOnSDCard;
  RefreshLocalGamesList;
  // Só baixa capas no fim do scan se o usuário ativou o toggle (padrão OFF).
  if AutoDownloadCovers and (GDEmu.LocalGamesListCount > 0) then
  begin
    ProgressWindow.SetTitle('Baixando capas da biblioteca');
    ProgressWindow.SetMax(GDEmu.LocalGamesListCount);
    GDEmu.StartDownloadAllLocalCovers(@OnFinishCoversDownload);
    ProgressWindow.ShowProgress;
    MainWindow.Enabled:=False;
  end
  else
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

// "Download MetaData": força re-obter TODO o metadado do jogo selecionado da
// biblioteca (re-extrai IP.BIN, re-enriquece do catálogo, re-baixa imagens).
procedure TMainWindow.LocalGameDownloadCoverBitBtnClick(Sender: TObject);
var idx: integer;
begin
  if (GDEmu.LocalGamesListCount = 0) or (LibraryView.ItemIndex < 0) then Exit;
  idx:=LibraryView.ItemIndex;
  LocalGameDownloadCoverBitBtn.Enabled:=False;
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    GDEmu.ForceRefreshGameMetadata(GDEmu.LocalGamesList[idx]);
    GDEmu.SaveLibrary;
    RefreshLocalGamesList; // recria a lista (zera o ItemIndex)
    if idx < LibraryView.Count then LibraryView.ItemIndex:=idx; // restaura a seleção
    LocalGamesListSelectionChange(nil, False); // re-exibe info + imagem atualizados
  finally
    Screen.Cursor:=crDefault;
    LocalGameDownloadCoverBitBtn.Enabled:=True;
  end;
end;

// "Download MetaData": idem para o jogo selecionado do SD Card.
procedure TMainWindow.SDCardDownloadCoverBitBtnClick(Sender: TObject);
var idx: integer;
begin
  if (GDEmu.SDCardGamesListCount = 0) or (SDCardView.ItemIndex < 0) then Exit;
  idx:=SDCardView.ItemIndex;
  SDCardDownloadCoverBitBtn.Enabled:=False;
  Screen.Cursor:=crHourGlass;
  try
    Application.ProcessMessages;
    GDEmu.ForceRefreshGameMetadata(GDEmu.SDCardGamesList[idx]);
    RefreshSDCardList;
    if idx < SDCardView.Count then SDCardView.ItemIndex:=idx;
    SDCardListSelectionChange(nil, False);
  finally
    Screen.Cursor:=crDefault;
    SDCardDownloadCoverBitBtn.Enabled:=True;
  end;
end;

end.

