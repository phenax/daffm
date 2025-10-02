default:
  @just --choose

run *args:
  cabal run daffm -- {{args}}

test *args:
  cabal test {{args}}

testw *args:
  nodemon -e .hs -w lib -w specs --exec 'clear && just test {{args}}'

build:
  nix build
