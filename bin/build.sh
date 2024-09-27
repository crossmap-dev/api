#!/bin/bash
set -euxo pipefail
VERSION=$(tr -d '\n' < VERSION.txt)
sed -i "s/version: .*/version: ${VERSION}/" package.yaml
hpack
cabal install --installdir=target --install-method=copy --overwrite-policy=always
