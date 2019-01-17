#!/bin/bash
echo "begin deploy.sh"
cp .stack-work/install/x86_64-linux/lts-12.25/8.4.4/bin/recetario-back recetario-back
echo "after copy"
git add recetario-back
git commit -m "add release"
echo "end deploy.sh"
