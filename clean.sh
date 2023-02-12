find . -name '*.~*' -delete
find . -name '*.dcu' -delete
find . -name '*.txt' -delete
find . -name '*.ppu' -delete
# файли типу lps містять розклад вікна програми, тому їх бажано не видаляти, а от грузити на гіт - не обов’язково
# find . -name '*.lps' -delete
find . -name '*.bak' -delete
find . -name '*.identcache' -delete
find . -name '*.otares' -delete
find . -name '*.cfg' -delete
find . -name '*.ddp' -delete
find . -name '*.o' -delete
find . -name '*.a' -delete
find . -name '*.local' -delete

rm .fuse_hidden*
find . -name '*.so' -delete
find . -name '*.dbg' -delete
rm -r backup
rm -r lib


