default:
  @just --choose

run *args:
  cabal run daffm -- {{args}}

test *args:
  cabal test {{args}}

doc:
  pandoc -f man -t markdown docs/daffm.1 -o docs/daffm.md

