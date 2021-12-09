#!/bin/sh -x

export NODE_VERSION=14.12.0
export NVM_NODEJS_ORG_MIRROR=https://unofficial-builds.nodejs.org/download/release
export NVM_DIR="$HOME/.nvm"

#shellcheck disable=SC1090
. "$HOME/.nvm/nvm.sh"

nvm_get_arch() { nvm_echo "x64-musl"; }

nvm install $NODE_VERSION
nvm use --delete-prefix $NODE_VERSION
