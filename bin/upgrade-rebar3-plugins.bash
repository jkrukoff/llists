#!/usr/bin/env bash
set -euo pipefail

function upgrade {
  local plugins profile
  declare -a plugins profile

  profile=()
  if [ "${1:-}" ]; then
    profile=(as "$1")
  fi

  mapfile -t plugins <<< "$(rebar3 "${profile[@]}" plugins list | tail -n +2 | sed '/^$/d' | cut -f 1 -d ' ')"
  for plugin in "${plugins[@]}"; do
    printf '%s\n' "rebar3 ${profile[*]} plugins upgrade \"${plugin}\""
    rebar3 "${profile[@]}" plugins upgrade "${plugin}"
  done
}

upgrade
upgrade test
