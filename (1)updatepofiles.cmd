rem @echo off
cls

set LANG=C

echo Extracting texts from source code

dxgettext -q --delphi --useignorepo -b .

echo Updating Russian translations
pushd locale\ru\LC_MESSAGES
copy default.po default-backup.po
ren default.po default-old.po
echo Merging
msgmergedx default-old.po ..\..\..\default.po -o default.po
del default-old.po
popd

echo Updating English translations
pushd locale\en\LC_MESSAGES
copy default.po default-backup.po
ren default.po default-old.po
echo Merging
msgmergedx default-old.po ..\..\..\default.po -o default.po
del default-old.po
popd

pause

