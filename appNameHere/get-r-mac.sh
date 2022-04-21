# Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn

#!/usr/bin/env bash
set -e

# Download and extract the main Mac Resources directory
mkdir -p r-mac
curl -o r-mac/latest_r.pkg \
https://cran.r-project.org/bin/macosx/base/R-4.1.2.pkg

cd r-mac
xar -xf latest_r.pkg
rm -r r-app.pkg Resources tcltk.pkg texinfo.pkg Distribution latest_r.pkg
cat r-fw.pkg/Payload | gunzip -dc | cpio -i
mv R.framework/Versions/Current/Resources/* .
rm -r r-fw.pkg R.framework

# Patch the main R script
sed -i.bak '/^R_HOME_DIR=/d' bin/R
sed -i.bak 's;/Library/Frameworks/R.framework/Resources;${R_HOME};g' \
bin/R
chmod +x bin/R
rm -f bin/R.bak

# Remove unneccessary files
rm -r doc tests
rm -r lib/*.dSYM
