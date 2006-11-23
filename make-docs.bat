mkdir docs
copy System\FilePath.hs docs\System.FilePath.hs
copy System\FilePath\Version_0_09.hs docs\System.FilePath.Version_0_09.hs
copy System\FilePath\Version_0_10.hs docs\System.FilePath.Version_0_10.hs
copy System\FilePath\Version_0_11.hs docs\System.FilePath.Version_0_11.hs
copy System\FilePath\Version_0_12.hs docs\System.FilePath.Version_0_12.hs
cpphs --noline System\FilePath\Windows.hs -D__HADDOCK__ > docs\System.FilePath.Windows.hs
cpphs --noline System\FilePath\Posix.hs -D__HADDOCK__ > docs\System.FilePath.Posix.hs

haddock -h docs\System.FilePath.hs docs\System.FilePath.Version_0_09.hs docs\System.FilePath.Version_0_10.hs docs\System.FilePath.Version_0_11.hs docs\System.FilePath.Version_0_12.hs docs\System.FilePath.Posix.hs docs\System.FilePath.Windows.hs --odir=docs --source-module=%%{MODULE}.html --source-entity=%%{MODULE}.html#%%{NAME}

copy d:\bin\hscolour.css docs\hscolour.css
hscolour -anchorCSS docs\System.FilePath.hs > docs\System.FilePath.html
hscolour -anchorCSS docs\System.FilePath.Version_0_09.hs > docs\System.FilePath.Version_0_09.html
hscolour -anchorCSS docs\System.FilePath.Version_0_10.hs > docs\System.FilePath.Version_0_10.html
hscolour -anchorCSS docs\System.FilePath.Version_0_11.hs > docs\System.FilePath.Version_0_11.html
hscolour -anchorCSS docs\System.FilePath.Version_0_12.hs > docs\System.FilePath.Version_0_12.html
hscolour -anchorCSS docs\System.FilePath.Windows.hs > docs\System.FilePath.Windows.html
hscolour -anchorCSS docs\System.FilePath.Posix.hs > docs\System.FilePath.Posix.html

if "%1" == "deploy" copy "docs\*.*" "W:\projects\filepath\*.*"

