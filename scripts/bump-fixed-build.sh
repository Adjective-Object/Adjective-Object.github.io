#!/usr/bin/env bash


echo $PATH | tr ':' '\n'

set -ex

nix-prefetch-git https://github.com/Adjective-Object/Adjective-Object.github.io.git --rev refs/heads/v2.1 > ./fixed-version.json