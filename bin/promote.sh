#!/bin/bash
set -euxo pipefail

# Promote develop to main
git checkout main

# Merge develop into main
git merge -s ort -X theirs develop -m "Merge develop into main"

# Read the version from the VERSION.txt file
VERSION=$(tr -d '\n' < VERSION.txt)

# Update the image tag in the kustomization.yaml file
pushd deploy
kustomize edit set image images.home.mtaylor.io/crossmap-api:${VERSION}
popd
git add deploy/kustomization.yml
git commit -m "Update image tag to ${VERSION}"

# Push the changes to the main branch
git push origin main
