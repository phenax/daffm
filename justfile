default:
  @just --choose

run *args:
  cabal run daffm -- {{args}}

test *args:
  cabal test {{args}}
