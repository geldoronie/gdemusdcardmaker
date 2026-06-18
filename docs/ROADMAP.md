# GDEMUGUI — Roadmap de Evolução

> Status: rascunho inicial (2026-06-16). Documento vivo — ajustar conforme o projeto evolui.

GDEMUGUI é hoje um **gerenciador de cartão SD para GDEMU/Dreamcast** em Lazarus/Free Pascal
(builds GTK2 e Qt5). Ele escaneia jogos locais e o cartão SD, copia/remove jogos, regenera o
menu (GDMenu CDI), extrai metadados do `IP.BIN` e baixa capas. O objetivo deste roadmap é
evoluí-lo de "utilitário" para uma ferramenta de referência, em quatro frentes.

## Visão

1. **Launcher visual rico** — grid de capas, busca/filtro, biblioteca, não só listas de texto.
2. **Suporte multi-hardware** — além do GDEMU, perfis para MODE / USB-GD-ROM e afins.
3. **Criador de discos homebrew genérico** — assistente que empacota qualquer homebrew em CDI/GDI booteável.
4. **Saúde do código** — modularizar o motor, portabilidade Windows/macOS, CI. Alicerce do resto.

A ordem importa: **saúde do código vem primeiro** — não dá para fazer multi-hardware nem um
launcher decente sobre um monólito Linux-only de 2245 linhas.

---

## Diagnóstico do estado atual (jun/2026)

### Motor (`src/gdemuunit.pas`, ~2245 linhas)
Monólito numa única `TThread` que faz scan, cópia, geração de CDI, scraping, INI e log.

- **Performance:** extrai metadados chamando `hexdump` **7×/jogo** (um processo por campo do IP.BIN).
- **Código morto / bugs:** `FixSDCardFolders` e `UpdateGDEmuINI` são stubs vazios;
  `DeleteFile(...'*.*')` usa wildcard que não funciona em `DeleteFile`;
  2 das 3 fontes de capa (TheGamesDB, ScreenScraper) estão implementadas mas **nunca usadas**
  (só GamesDatabase.org está ativa).
- **Portabilidade:** `hexdump` e `python` hardcoded já foram **eliminados** (leitura de IP.BIN e
  extração de boot sector agora são Pascal nativo). Resta: zero `{$IFDEF WINDOWS/DARWIN}` e os
  binários em `tools/` (`genisoimage`, `cdi4dc`, `cdirip`, …) são ELF Linux x86-64. **Hoje só
  roda em Linux.**
- **Robustez:** `Execute` da thread sem `try/except`; cache JSON sem versionamento;
  HTTP sem retry/throttle; User-Agent fixo "Linux".

### UI (`src/*.pas` + `.lfm`)
Funcional, porém crua: duas `TCheckListBox` de texto puro, **sem** busca/filtro/ordenação,
sem grid de capas, sem "selecionar todos", progresso via *polling* de strings de estado a cada
500 ms. i18n preparado (`.po`/`.pot`/`.lrj`) mas com traduções vazias.

### Repositório / build
- **Git tracking está enxuto:** apenas ~9.9 MB versionados. `releases/` e os binários da raiz
  (`*.bin`/`*.debug`) **já estão ignorados**. Os ~760 MB da working tree são arquivos
  **não-versionados** (`cache/` com ~2200 itens, `releases/`, builds de debug).
- **`tools/` (1.8 MB) e `data/` (7 MB) são versionados — e isso é intencional por enquanto:**
  são **dependências de runtime** (executáveis chamados pelo app e boot sectors `IP.BIN`/
  `1ST_READ.BIN` usados na criação de discos), não artefatos de build. Migram para
  release-assets/build-from-source na Fase 1, quando houver substituto.
- `src/Makefile` continha um comando de limpeza de `/tmp` sem relação com o projeto (removido).
- Sem CI, sem testes.

---

## Fases

### Fase 0 — Higiene & build  ·  _em andamento_
Alicerce de baixo risco, sem mexer em lógica.

- [x] `Makefile` real na raiz (`lazbuild` Qt5 + GTK2, `clean`, `help`).
- [x] Remover `src/Makefile` lixo.
- [x] `.gitignore` reorganizado e comentado; assets de `data/` protegidos por negação explícita.
- [x] Destrackear `data/dcevo/ffmpeg2pass-0.log` (log regenerável).
- [x] **Corrigir compilação quebrada:** `TryGetCoverFromTheGamesDB` e
      `TryGetCoverFromScreenScraper` estavam implementadas mas não declaradas na classe
      `TGDEmu` — fonte versionada não compilava. Declarações adicionadas; Qt5 e GTK2 voltam
      a buildar.
- [x] Remover stubs vazios (`FixSDCardFolders`, `UpdateGDEmuINI`) — eram código morto não chamado.
- [x] Corrigir o bug do `DeleteFile('*.*')` em `CreateGDEmuImage` (wildcard não funciona em
      `DeleteFile`; trocado por `FindAllFiles` + loop).
- [x] README: instruções de build via `make`, deps e nota de portabilidade.
- [ ] ~~CI no GitHub Actions~~ — adiado a pedido do mantenedor.

### Fase 1 — Modularizar o motor + portabilidade
Destrava todas as fases seguintes.

- [~] Quebrar `gdemuunit.pas` em unidades coesas:
      `GameModel`, `MetadataExtractor`, `DiscBuilder`, `CoverScraper`, `SDCardManager`,
      `ExternalTools`.
      **Feito:** `GameModel` (`src/gamemodel.pas` — `TGDEmuGame` + helpers de nome) e
      `ExternalTools` (`src/externaltools.pas` — 5 wrappers de ferramenta + helpers, desacoplados
      do motor via callback `TCommandLogger`). Monólito caiu de ~2245 → ~1640 linhas.
      **Falta:** extrair `MetadataExtractor`/`DiscBuilder`/`CoverScraper`/`SDCardManager` — isso
      exige reestruturar a god-class `TGDEmu` (seus ~50 campos de estado), trabalho maior e mais
      arriscado, a fazer incrementalmente.
- [x] Camada `ExternalTools` com abstração por SO e descoberta de executáveis no PATH:
      `ResolveToolPath` (binário em `tools/` primeiro, depois `FindDefaultExecutablePath` no PATH;
      sufixo `.exe` via `{$IFDEF WINDOWS}`); extraída para `src/externaltools.pas`.
      Subpastas `tools/<os>/` por plataforma ficam para quando houver binários não-Linux.
- [x] Ler o `IP.BIN` direto em Pascal (struct de offsets fixos) e **eliminar as 7 chamadas a
      `hexdump`** por jogo.
- [x] **Reimplementar a extração do boot sector de `.gdi` em Pascal nativo**
      (`ExtractGDIBootSector`) e **remover a dependência de `gditools.py`/Python** — que estava
      quebrada sob Python 3 (sem Python 2 nas distros atuais). `tools/gditools.py` e
      `tools/iso9660.py` removidos. Validado byte-a-byte nos modos de setor 2352 e 2048.
- [~] Mover `tools/`/`data/` para release-assets/submódulo + script de download por plataforma;
      então removê-los do tracking com segurança.
      **Feito:** removidos 8 binários de `tools/` que o código nunca invoca (`devdump`, `dirsplit`,
      `geteltorito`, `isodump`, `isoinfo`, `isovfy`, `mkzftree`, `mkisofs` — ~966 KB de peso morto);
      restam só os 3 usados (`genisoimage`, `cdi4dc`, `cdirip`). Corrigido o bit de execução de
      `tools/cdirip` (estava `100644` → falharia ao rodar). Adicionado `scripts/check-tools.sh`.
      **Falta (decisão do mantenedor):** definir onde hospedar os 3 binários (release do GitHub /
      build-from-source) antes de destrackeá-los; `data/` permanece tracked (é dep de runtime real:
      boot sectors + templates de homebrew).
- [x] `try/except` no `Execute` da thread + relato de erro estruturado: cada ação é isolada
      (falha vira `FINISHED` + `LastError`, a `ProgressWindow` fecha em vez de girar pra sempre e
      mostra a mensagem); loop externo blindado para a thread worker nunca morrer.
- [x] Reativar/limpar as fontes de capa não usadas: removidas `TryGetCoverFromTheGamesDB` e
      `TryGetCoverFromScreenScraper` (~300 linhas de código morto nunca chamado). `GetGameCover`
      usa só GamesDatabase.org.

### Fase 2 — Launcher visual
- [ ] Grid de capas (reusando o cache existente) com lazy-load.
- [ ] Busca/filtro e ordenação nas listas; "selecionar todos / nenhum".
- [ ] Info rica (tamanho, região, versão, data) e tooltips.
- [ ] Persistir preferências (posição/tamanho de janela, ordenação, diretórios recentes).
- [ ] Tema claro/escuro.
- [ ] Download de capas em lote, com progresso real (bytes, ETA, cancelar).

### Fase 3 — Suporte multi-hardware
- [ ] Conceito de **perfil de cartão** (GDEMU / MODE / USB-GD-ROM), abstraindo layout de pastas
      e formato de menu.
- [ ] Detecção/seleção de perfil ao carregar um cartão.
- [ ] Geração de menu específica por perfil.

### Fase 4 — Criador de discos homebrew genérico
- [ ] Generalizar o OpenBOR Creator num assistente que empacota qualquer homebrew
      (boot sector + binário + assets) em CDI/GDI booteável.
- [ ] Templates por tipo de homebrew (OpenBOR, DC Evolution, players de mídia já presentes em `data/`).
- [ ] Preview e validação dos arquivos antes de gerar o disco.

---

## Princípios

- Mudanças de higiene não devem quebrar o app: `tools/`/`data/` só saem do tracking quando
  houver mecanismo de obtenção equivalente.
- Cada fase deve manter as builds Qt5 e GTK2 compilando.
- Preferir reimplementar extrações simples em Pascal a depender de binários externos.
