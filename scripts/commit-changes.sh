#!/bin/bash
set -e

git config --global user.email "developer@47deg.com";
git config --global user.name "47Deg (Travis CI)";
git config --global push.default simple;

git checkout master
git pull --rebase
git add AUTHORS.md CONTRIBUTING.md contributors.sbt LICENSE;
git commit -m 'Updating policy files';
git remote add travis-remote git@github.com:47deg/sbt-dependencies.git;
git push travis-remote master;
