# clompse - Take a Glimpse at Your Cloud

> **TODO** Provide minimum viable documentation.

## Development

Provision Nix Shell via `direnv`:

```sh
direnv allow
```

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 clompse -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```
