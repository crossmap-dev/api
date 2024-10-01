#!/bin/bash
set -euxo pipefail

TAG=${1:-"development"}
VERSION=$(tr -d '\n' < VERSION.txt)

sed -i "s/version: .*/version: ${VERSION}/" package.yaml

cat <<EOF > src/CROSSMAP/Version.hs
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Version ( version ) where
import Data.Text ( Text )
version :: Text
version = "${TAG}"
EOF

hpack

cabal install --installdir=target --install-method=copy --overwrite-policy=always
