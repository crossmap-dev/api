#!/bin/bash
set -euxo pipefail
hpack
cabal build
