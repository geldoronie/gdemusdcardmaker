# GDEMUGUI - build automation
# Requires: lazbuild (Lazarus) + fpc (Free Pascal). See README.md for deps.

LPI      := src/gdemugui.lpi
LAZBUILD := lazbuild
LAZFLAGS := -B

.PHONY: all qt5 gtk2 debug-qt5 debug-gtk2 release clean help

all: release ## Build both release binaries (Qt5 + GTK2)

release: qt5 gtk2 ## Alias for building both release targets

qt5: ## Build the Qt5 release binary (-> gdemugui.qt5.bin)
	$(LAZBUILD) $(LAZFLAGS) --build-mode="Release QT5" $(LPI)

gtk2: ## Build the GTK2 release binary (-> gdemugui.gtk2.bin)
	$(LAZBUILD) $(LAZFLAGS) --build-mode="Release GTK" $(LPI)

debug-qt5: ## Build the Qt5 debug binary
	$(LAZBUILD) $(LAZFLAGS) --build-mode="Debug QT5" $(LPI)

debug-gtk2: ## Build the GTK2 debug binary
	$(LAZBUILD) $(LAZFLAGS) --build-mode="Debug GTK" $(LPI)

clean: ## Remove Lazarus build artifacts (lib/, *.bin, *.debug)
	rm -rf src/lib
	rm -f gdemugui.qt5.bin gdemugui.qt5.debug gdemugui.gtk2.bin gdemugui.gtk2.debug

help: ## Show this help
	@grep -E '^[a-zA-Z0-9_-]+:.*## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN{FS=":.*## "}{printf "  \033[36m%-14s\033[0m %s\n", $$1, $$2}'
