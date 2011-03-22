#!/bin/bash
set -x
cp dexy_artifacts/*.png ../doc/"${1}"/ 
cp dexy_artifacts/*.pdf ../doc/"${1}"/ 
cp dexy_artifacts/*.eps ../doc/"${1}"/ 
redcloth -o html ../doc/"${1}"/${1}.textile > ../doc/"${1}"/${1}.html
/System/Library/Printers/Libraries/convert -f ../doc/"${1}"/${1}.html -o ../doc/"${1}"/${1}.pdf
rm -r ../doc_save/"${1}"/ 
mkdir ../doc_save/"${1}"/ 
cp -rf ../doc/"${1}"/ ../doc_save/"${1}"/ 
ls -R ../doc_save
