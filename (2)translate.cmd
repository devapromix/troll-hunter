cls
set sourceroot=%CD%\

echo Compiling language files

echo English...
cd %sourceroot%locale\en\LC_MESSAGES
msgfmt.exe -o default.mo default.po

echo Russian...
cd ..\..\ru\LC_MESSAGES
msgfmt.exe -o default.mo default.po

echo Compiling language files completed

cd %sourceroot%
pause