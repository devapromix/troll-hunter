cls
set sourceroot=%CD%\

echo Compiling language files and embedding those
echo English...
cd %sourceroot%locale\en\LC_MESSAGES
msgfmt.exe -o default.mo default.po
echo Russian...
cd ..\..\ru\LC_MESSAGES
msgfmt.exe -o default.mo default.po
cd ..\..\..

echo Embedding translations...
copy Project1.exe Project1_Translated.exe
assemble.exe --dxgettext Project1_Translated.exe

echo Compiling language files and embedding those completed

cd %sourceroot%
pause