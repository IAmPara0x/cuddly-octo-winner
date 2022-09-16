echo "formatting using brittany ..."
find ./src -name "*.hs" | xargs -L1 brittany --write-mode=inplace
echo "done"

echo "formatting using stylish-haskell ..."
stylish-haskell -i -r .
echo "done"

echo "formatting Miku.cabal ..."
cabal-fmt -i Miku.cabal
echo "done"
