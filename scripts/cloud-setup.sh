#!/usr/bin/env bash
# Cloud sandbox only: installs what working on this configuration needs
# there, skipping whatever is already in place.  The environment's setup
# script runs it as root when a fresh container starts; by hand:
#   scripts/cloud-setup.sh
#
# - system packages: build tools for Emacs, enchant and aspell for
#   spelling, direnv for envrc;
# - Emacs 31.1 without GUI, built from the GitHub mirror (the GNU hosts
#   are blocked) into $EMACS_PREFIX, about 15 minutes;
# - clojure-lsp, pinned, into $CLOJURE_LSP_DIR, linked into
#   /usr/local/bin.
set -euo pipefail

emacs_version=31.1
emacs_prefix="${EMACS_PREFIX:-/opt/emacs-$emacs_version}"
clojure_lsp_version=2026.10.15-11.42.38
clojure_lsp_dir="${CLOJURE_LSP_DIR:-/opt/clojure-lsp}"

packages=(
  build-essential autoconf automake texinfo pkg-config
  libgnutls28-dev libncurses-dev libxml2-dev libtree-sitter-dev
  libjansson-dev zlib1g-dev
  libenchant-2-dev aspell aspell-de aspell-en
  direnv
)

install_packages() {
  local missing=()
  for package in "${packages[@]}"; do
    dpkg -s "$package" >/dev/null 2>&1 || missing+=("$package")
  done
  if ((${#missing[@]})); then
    echo "Installing ${missing[*]}"
    apt-get update -q
    apt-get install -y -q "${missing[@]}"
  fi
}

build_emacs() {
  [[ -x "$emacs_prefix/bin/emacs" ]] && return
  local source
  source="$(mktemp -d)"
  echo "Building Emacs $emacs_version into $emacs_prefix (log in $source)"
  git -c advice.detachedHead=false clone -q --depth 1 \
    --branch "emacs-$emacs_version" \
    https://github.com/emacs-mirror/emacs "$source/emacs"
  (
    cd "$source/emacs"
    ./autogen.sh > "$source/autogen.log" 2>&1
    ./configure --prefix="$emacs_prefix" --without-x --without-ns \
      --with-gnutls --with-tree-sitter --without-native-compilation \
      --without-sound --without-dbus --without-gsettings \
      --without-selinux > "$source/configure.log" 2>&1
    make -j"$(nproc)" > "$source/make.log" 2>&1
    make install > "$source/install.log" 2>&1
  )
  rm -rf "$source"
}

install_clojure_lsp() {
  local binary="$clojure_lsp_dir/clojure-lsp"
  if [[ -x "$binary" ]] &&
       "$binary" --version | grep -q "$clojure_lsp_version"; then
    return
  fi
  echo "Installing clojure-lsp $clojure_lsp_version into $clojure_lsp_dir"
  mkdir -p "$clojure_lsp_dir"
  local release=https://github.com/clojure-lsp/clojure-lsp/releases/download
  local asset=clojure-lsp-native-static-linux-amd64.zip
  curl -sSL -o "$clojure_lsp_dir/clojure-lsp.zip" \
    "$release/$clojure_lsp_version/$asset"
  unzip -o -q "$clojure_lsp_dir/clojure-lsp.zip" -d "$clojure_lsp_dir"
  rm "$clojure_lsp_dir/clojure-lsp.zip"
}

link_clojure_lsp() {
  [[ "$clojure_lsp_dir" == /opt/clojure-lsp ]] || return 0
  ln -sf "$clojure_lsp_dir/clojure-lsp" /usr/local/bin/clojure-lsp
}

install_packages
build_emacs
install_clojure_lsp
link_clojure_lsp
"$emacs_prefix/bin/emacs" --version | head -1
"$clojure_lsp_dir/clojure-lsp" --version | head -1
