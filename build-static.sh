#!/usr/bin/env bash

## Fail on errors including pipe failures:
set -eo pipefail

## NOTE: Things would be much easier if we could use Nix, but we can
## not (or I find it rather tedious). So, we have to use Docker.
##
## Also, `cabal install` does not work with
## `--enable-executable-static` flag. So, we have to use `cabal build`
## instead. Finally, `cabal build` does not work with
## `--enable-executable-stripping`, hence the `strip` command usage.

## GHC version:
GHC_VERSION="9.8.4"

## Docker image:
DOCKER_IMAGE="quay.io/benz0li/ghc-musl:${GHC_VERSION}"

## Executable name:
EXECUTABLE_NAME="clompse"

## Final executable name:
FINAL_EXECUTABLE_NAME="${EXECUTABLE_NAME}-static-$(uname --kernel-name | tr '[:upper:]' '[:lower:]')-$(uname --machine)"

## Final executable path:
FINAL_EXECUTABLE_PATH="/tmp/${FINAL_EXECUTABLE_NAME}"

## Docker container name:
CONTAINER_NAME="static-builder-for-${EXECUTABLE_NAME}"

## Create/update .cabal file:
hpack

## Update package list:
cabal update

## Cleanup first:
cabal clean
cabal v1-clean

## First, pin all packages as per Nix:
cabal freeze

## Fix tls package version (Remove the line that contains "any.tls ==2.1.1,"):
##
## Note that this is a workaround until nixpkgs provides tls>=2.1.1 as stock dependency.
sed -i '/any.tls ==2.1.1,/d' cabal.project.freeze

## Run the Docker container:
docker run -i --detach -v "$(pwd):/app" --name "${CONTAINER_NAME}" "${DOCKER_IMAGE}" /bin/bash

## Whitelist codebase directory for Git queries:
docker exec "${CONTAINER_NAME}" git config --global --add safe.directory /app

## Update cabal database:
docker exec "${CONTAINER_NAME}" cabal update

## Build the static binary:
##
## Note that the `--allow-newer` flag is used to allow newer versions of
## dependencies, which is to allow jail-broken nixpkgs dependencies in a
## non-nix build environment that adopts our .freeze file generated under
## Nix.
docker exec -w "/app" "${CONTAINER_NAME}" cabal build --enable-executable-static --allow-newer

## Get the path to the executable:
##
## Note that the `--allow-newer` flag is used to allow newer versions of
## dependencies, which is to allow jail-broken nixpkgs dependencies in a
## non-nix build environment that adopts our .freeze file generated under
## Nix.
BUILD_PATH="$(docker exec -w "/app" "${CONTAINER_NAME}" cabal list-bin --allow-newer "${EXECUTABLE_NAME}")"

## Strip debugging symbols:
docker exec "${CONTAINER_NAME}" strip "${BUILD_PATH}"

## Copy the binary to the host:
docker cp "${CONTAINER_NAME}:${BUILD_PATH}" "${FINAL_EXECUTABLE_PATH}"

## Compress the executable:
upx "${FINAL_EXECUTABLE_PATH}"

## Cleanup:
docker exec -w "/app" "${CONTAINER_NAME}" cabal clean
docker exec -w "/app" "${CONTAINER_NAME}" cabal v1-clean
docker rm -f "${CONTAINER_NAME}"
rm cabal.project.freeze
file "${FINAL_EXECUTABLE_PATH}"
