#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

DEP="$1"
URL="$2"
CHECKSUM="$3"

curl -L# "${URL}" -o "/usr/local/bin/${DEP}"
if ! sha512sum --check - <<< "${CHECKSUM}	/usr/local/bin/${DEP}"; then
  printf 'Invalid SHA-512 checksum!\nExpected: %s\nGot: %s\n' "${CHECKSUM}" "$(sha512sum "/usr/local/bin/${DEP}")" 1>&2
  exit 1
fi
chmod 755 "/usr/local/bin/${DEP}"
