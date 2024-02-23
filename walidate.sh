#!/usr/bin/env sh
wasm-as \
  --enable-gc \
  --enable-multivalue \
  --enable-exception-handling \
  --enable-reference-types \
  --enable-tail-call \
  --enable-bulk-memory \
  --enable-nontrapping-float-to-int \
  --enable-strings \
  -g \
  $@
