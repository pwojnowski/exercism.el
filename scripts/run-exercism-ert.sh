#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

emacs_cmd="${EMACS:-$(command -v emacs 2>/dev/null || true)}"
if [[ -z "${emacs_cmd}" && -x /Applications/Emacs.app/Contents/MacOS/Emacs ]]; then
  emacs_cmd=/Applications/Emacs.app/Contents/MacOS/Emacs
fi
if [[ -z "${emacs_cmd}" ]]; then
  echo "emacs not found; set EMACS to your Emacs binary" >&2
  exit 127
fi

exec "${emacs_cmd}" -batch -l exercism-ert-bootstrap.el
