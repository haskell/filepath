mkdir docs
mkdir docs\System
mkdir docs\System\FilePath
copy System\FilePath.hs docs\System\FilePath.hs
cpphs --noline System\FilePath\Windows.hs -D__HADDOCK__ > docs\System\FilePath\Windows.hs
cpphs --noline System\FilePath\Posix.hs -D__HADDOCK__ > docs\System\FilePath\Posix.hs

haddock -h docs\System\FilePath.hs docs\System\FilePath\Posix.hs docs\System\FilePath\Windows.hs --odir=docs --lib=d:\bin --source-module=FilePath.html --source-entity=FilePath.html#%%{NAME}

copy d:\bin\hscolour.css docs\hscolour.css
hscolour -anchorCSS System\FilePath.hs > docs\FilePath.html

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

