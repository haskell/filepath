mkdir docs
cpphs --noline System\FilePath.hs         -D__HADDOCK__ > docs\System.FilePath.hs
cpphs --noline System\FilePath\Windows.hs -D__HADDOCK__ > docs\System.FilePath.Windows.hs
cpphs --noline System\FilePath\Posix.hs   -D__HADDOCK__ > docs\System.FilePath.Posix.hs

haddock -h docs\System.FilePath.hs docs\System.FilePath.Posix.hs docs\System.FilePath.Windows.hs --odir=docs --source-module=%%{MODULE}.html --source-entity=%%{MODULE}.html#%%{NAME}

copy d:\bin\hscolour.css docs\hscolour.css
hscolour -anchor -css docs\System.FilePath.hs         > docs\System.FilePath.html
hscolour -anchor -css docs\System.FilePath.Windows.hs > docs\System.FilePath.Windows.html
hscolour -anchor -css docs\System.FilePath.Posix.hs   > docs\System.FilePath.Posix.html

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

