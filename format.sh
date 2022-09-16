verbose=true

if  [[ $1 == "-v" ]]; then
  verbose=true
  echo "VERBOSE Mode"
else
  verbose=false
  echo "Not VERBOSE Mode"
fi

if [[ $verbose == true ]]; then
  echo "formatting using brittany ..."
  find ./src -name "*.hs" | xargs -L1 brittany --write-mode=inplace -v
  echo "done"

  echo "formatting using stylish-haskell ..."
  stylish-haskell -i -r -v .
  echo "done"

  echo "formatting Miku.cabal ..."
  cabal-fmt -i Miku.cabal
  echo "done"
else
  echo "formatting using brittany ..."
  find ./src -name "*.hs" | xargs -L1 brittany --write-mode=inplace
  echo "done"

  echo "formatting using stylish-haskell ..."
  stylish-haskell -i -r .
  echo "done"

  echo "formatting Miku.cabal ..."
  cabal-fmt -i Miku.cabal
  echo "done"
fi
