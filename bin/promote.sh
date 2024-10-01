#!/bin/bash
set -euxo pipefail

# Fetch the latest changes
git fetch origin

# Switch to the develop branch
git checkout develop

# Pull the latest changes
git pull origin develop

# Switch to the main branch
git checkout main

# Pull the latest changes
git pull origin main

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

# Merge main back into develop
git checkout develop
git merge -s ours main -m "Release ${VERSION}"

# Push the changes to the develop branch
git push origin develop
