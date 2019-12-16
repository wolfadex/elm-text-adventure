#!/bin/bash

echo Building demo ...

cd example

# Build
parcel build src/index.html

cd ..

# Copy files to the root
cp example/dist/index.html index.html
cp -r example/dist/*.js .

# Cleanup
rm -rf example/dist

echo Demo built