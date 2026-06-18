# GDEMU SD Card Maker

GUI (Lazarus / Free Pascal) to manage Sega Dreamcast game collections on a GDEMU
SD card: scan local games and the SD card side by side, copy/remove games, regenerate
the GDMenu (CDI), extract `IP.BIN` metadata and download box-art covers. It also bundles
an OpenBOR disc creator.

Two widgetset builds are produced: **Qt5** and **GTK2**.

> See [`docs/ROADMAP.md`](docs/ROADMAP.md) for where the project is headed.

## Building

### Dependencies

- **Free Pascal** + **Lazarus** (`lazbuild`, `fpc`)
- **Qt5 build:** `libqt5pas-dev`
- **GTK2 build:** `libgtk-2-dev`
- **Runtime tools:** `cdrkit` (provides `genisoimage`); the remaining helpers
  (`cdi4dc`, `cdirip`, …) are bundled under `tools/`. GDI boot-sector (`IP.BIN`)
  extraction is now native Pascal — no Python is required.

> Note: the bundled `tools/` binaries are Linux x86-64 only, so the app currently runs
> on Linux. Cross-platform support is planned (see the roadmap).

### Compile

A `Makefile` at the repo root drives `lazbuild`:

```sh
make            # build both release binaries (Qt5 + GTK2)
make qt5        # build only gdemugui.qt5.bin
make gtk2       # build only gdemugui.gtk2.bin
make debug-qt5  # debug build (Qt5)
make clean      # remove build artifacts
make help       # list all targets
```

The release binaries are written to the repo root (`gdemugui.qt5.bin`,
`gdemugui.gtk2.bin`).

## Used Resources

### Images / Icons

https://iconarchive.com/show/console-icons-by-sykonist/Sega-Dreamcast-icon.html

### Based on other projects

- **DC Card Maker Script:** https://github.com/jstolarek/dc-card-maker-script
- **img4dc:** https://github.com/Kazade/img4dc
- **cdirip:** https://github.com/jozip/cdirip
