#!/usr/bin/env bash

set -ex

BUILD_DIR=$(nix-build)

cd `mktemp -d`
cp -r "$BUILD_DIR" ./deploy
cd deploy
chmod +w .

pwd
ls -la

GIT="git -c user.email=\"nix-autobuild@huang-hobbs.co\" \
    -c user.name=\"nix-autobuild\" "

$GIT init
$GIT add *
$GIT commit -am "automatic-build at `date`"

BRANCH_NAME=`git rev-parse --abbrev-ref HEAD`
$GIT push --force --quiet \
    https://${GITHUB_TOKEN}:x-oauth-basic@${GITHUB_REMOTE} "$BRANCH_NAME:gh-pages"

