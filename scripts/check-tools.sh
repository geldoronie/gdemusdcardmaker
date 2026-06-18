#!/usr/bin/env bash
#
# check-tools.sh — verify the external helper tools GDEMUGUI needs are available.
#
# The app resolves each tool the same way at runtime (see ResolveToolPath in
# src/externaltools.pas): it prefers the bundled binary under tools/ and falls
# back to the system PATH. This script reports, for each required tool, where it
# was found (or how to obtain it if missing).

set -u

# Resolve relative to the repo root regardless of where the script is called from.
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS_DIR="$ROOT/tools"

# Required tool -> hint on how to obtain it when missing.
declare -A HINTS=(
  [genisoimage]="install 'cdrkit' (Debian/Ubuntu/Arch) or 'cdrtools'"
  [cdi4dc]="build from https://github.com/Kazade/img4dc"
  [cdirip]="build from https://github.com/jozip/cdirip"
)

missing=0
for tool in genisoimage cdi4dc cdirip; do
  if [[ -x "$TOOLS_DIR/$tool" ]]; then
    printf '  [ok]      %-12s bundled: %s\n' "$tool" "$TOOLS_DIR/$tool"
  elif path="$(command -v "$tool" 2>/dev/null)"; then
    printf '  [ok]      %-12s PATH:    %s\n' "$tool" "$path"
  else
    printf '  [MISSING] %-12s %s\n' "$tool" "${HINTS[$tool]}"
    missing=1
  fi
done

if [[ "$missing" -ne 0 ]]; then
  echo
  echo "Some required tools are missing. The easiest fix is:"
  echo "    ./scripts/fetch-tools.sh    # downloads them into tools/"
  echo "Or install them yourself (see hints above) and put them on the PATH."
  exit 1
fi

echo
echo "All required tools are available."
