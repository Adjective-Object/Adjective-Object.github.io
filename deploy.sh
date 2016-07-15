#!/usr/bin/env bash


make
cd _site

git init
git config user.email "nix-autobuild@huang-hobbs.co"
git config user.name "nix-autobuild"
git config http.sslVerify false
git add * > /dev/null
git commit -am "automatic-build at `date`"

set -x

git push --force --quiet \
    https://${github_token}:x-oauth-basic@${github_remote} ${branches}


