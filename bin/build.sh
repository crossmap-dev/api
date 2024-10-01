#!/bin/bash
set -euxo pipefail

VERSION=$(tr -d '\n' < VERSION.txt)

sed -i "s/version: .*/version: ${VERSION}/" package.yaml

cat <<EOF > src/CROSSMAP/Version.hs
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Version ( version ) where
import Data.Text ( Text )
version :: Text
version = "${VERSION}"
EOF

hpack

cabal install --installdir=target --install-method=copy --overwrite-policy=always
