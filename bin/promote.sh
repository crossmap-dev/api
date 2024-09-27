#!/bin/bash
set -euxo pipefail

# Promote develop to main
git checkout main

# Merge develop into main
git merge develop

# Read the version from the VERSION.txt file
VERSION=$(tr -d '\n' < VERSION.txt)

# Update the image tag in the kustomization.yaml file
pushd deploy
kustomize edit set image images.home.mtaylor.io/crossmap-dev-api:${VERSION}
popd
