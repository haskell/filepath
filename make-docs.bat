mkdir docs
copy System\FilePath.hs docs\System.FilePath.hs
copy System\FilePath\Compat.hs docs\System.FilePath.Compat.hs
cpphs --noline System\FilePath\Windows.hs -D__HADDOCK__ > docs\System.FilePath.Windows.hs
cpphs --noline System\FilePath\Posix.hs -D__HADDOCK__ > docs\System.FilePath.Posix.hs

haddock -h docs\System.FilePath.hs docs\System.FilePath.Compat.hs docs\System.FilePath.Posix.hs docs\System.FilePath.Windows.hs --odir=docs --lib=d:\bin --source-module=%%{MODULE}.html --source-entity=%%{MODULE}.html#%%{NAME}

copy d:\bin\hscolour.css docs\hscolour.css
hscolour -anchorCSS docs\System.FilePath.hs > docs\System.FilePath.html
hscolour -anchorCSS docs\System.FilePath.Compat.hs > docs\System.FilePath.Compat.html
hscolour -anchorCSS docs\System.FilePath.Windows.hs > docs\System.FilePath.Windows.html
hscolour -anchorCSS docs\System.FilePath.Posix.hs > docs\System.FilePath.Posix.html

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

