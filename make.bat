mkdir docs
haddock -h System/FilePath.hs --odir=docs

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

