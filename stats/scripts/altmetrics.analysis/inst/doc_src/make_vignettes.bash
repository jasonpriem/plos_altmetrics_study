#!/bin/bash

# call this like 
# bash make_vignettes.bash cleaning description correlation normalization factor_analysis prediction

set -x

rm -r ../doc_save
mkdir ../doc_save

dexy --cache-dir=../doc --artifacts-dir=dexy_artifacts -spn .
cp ../doc/index.* ../doc_save/

path_to_change="file:///Users/hpiwowar/Documents/altmetrics.analysis/inst/doc_src/dexy_artifacts"
change_it_to="\."

for vignette in "$@"
do
    dexy --cache-dir=../doc --artifacts-dir=dexy_artifacts -sp $vignette
    bash runlast.bash $vignette
    sed -i '' s*$path_to_change*$change_it_to*g ../doc_save/*/*.html
done

