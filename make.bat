mkdir docs
haddock -h System/PathUtils.hs --odir=docs

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

