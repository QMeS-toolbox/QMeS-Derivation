#!/bin/bash

mkdir -p QMeSderivation
cp -r example QMeSderivation
cp QMeSderivation.m QMeSderivation
cp QMeSTools.m QMeSderivation
cp PacletInfo.m QMeSderivation
cp LICENSE QMeSderivation
cp ReleaseNotes.md QMeSderivation
cp README.md QMeSderivation

rm ./QMeSderivation.zip
zip -r QMeSderivation.zip QMeSderivation

rm -rf ./QMeSderivation/
