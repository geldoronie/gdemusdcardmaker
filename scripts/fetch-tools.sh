#!/usr/bin/env bash
#
# fetch-tools.sh — download the external helper binaries GDEMUGUI needs.
#
# The three tools (genisoimage, cdi4dc, cdirip) are no longer stored in git.
# They live as assets of the 'tools-v1' GitHub release and are fetched into
# tools/ on demand, with sha256 verification. Re-running is safe: a tool that is
# already present with the right checksum is skipped.
#
# Linux x86-64 only (the published binaries are ELF x86-64).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS_DIR="$ROOT/tools"
BASE_URL="https://github.com/geldoronie/gdemusdcardmaker/releases/download/tools-v1"

# tool -> expected sha256
declare -A SHA=(
  [genisoimage]="48dd2cf116561683a4a4a32c51570590fc79bcc6bcce6fc806a3c3ee4f3b8118"
  [cdi4dc]="e90a608b52c281778e118b997b98fa52482e8398ffcffb01cebdea03d9d2d785"
  [cdirip]="d0b11d580c12f974091251e589ea2f8a91dabd24d72b62aa4d61958f579ae254"
)

download() {
  # download <url> <dest>
  if command -v curl >/dev/null 2>&1; then
    curl -fsSL "$1" -o "$2"
  elif command -v wget >/dev/null 2>&1; then
    wget -qO "$2" "$1"
  else
    echo "error: need curl or wget to download tools" >&2
    exit 1
  fi
}

mkdir -p "$TOOLS_DIR"

for tool in genisoimage cdi4dc cdirip; do
  dest="$TOOLS_DIR/$tool"
  want="${SHA[$tool]}"

  if [[ -f "$dest" ]] && echo "$want  $dest" | sha256sum -c --status 2>/dev/null; then
    printf '  [skip]  %-12s already present\n' "$tool"
    continue
  fi

  printf '  [get]   %-12s downloading...\n' "$tool"
  download "$BASE_URL/$tool" "$dest"

  if ! echo "$want  $dest" | sha256sum -c --status; then
    echo "error: checksum mismatch for $tool (expected $want)" >&2
    rm -f "$dest"
    exit 1
  fi
  chmod +x "$dest"
done

echo
echo "All tools fetched into $TOOLS_DIR"
