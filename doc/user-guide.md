% Nix loves Haskell
% Peter Simons \<simons@cryp.to\>
% 2015-05-15

-------------------------------------------------------------------------------

# What can Nix do for you?

1) Install a Haskell compiler

2) Create a Haskell development environment


-------------------------------------------------------------------------------

# Installing a Haskell compiler

    $ nix-env -iA haskell.compiler.ghc784

    $ nix-env -qaP -A haskell.compiler
    haskell.compiler.ghc6104        ghc-6.10.4
    haskell.compiler.ghc6123        ghc-6.12.3
    haskell.compiler.ghc704         ghc-7.0.4
    haskell.compiler.ghc722         ghc-7.2.2
    haskell.compiler.ghc742         ghc-7.4.2
    haskell.compiler.ghc763         ghc-7.6.3
    haskell.compiler.ghc784         ghc-7.8.4
    haskell.compiler.ghc7101        ghc-7.10.1
    haskell.compiler.ghcHEAD        ghc-7.11.20150402
    haskell.compiler.ghcjs          ghcjs-0.1.0
    haskell.compiler.jhc            jhc-0.8.2
    haskell.compiler.uhc            uhc-1.1.9.0

-------------------------------------------------------------------------------

# Installing more than one Haskell compiler

- Multiple environments:

        $ nix-env -p ~/ghc-7.6.3 -iA haskell.compiler.ghc763
        $ nix-env -p ~/ghc-7.8.4 -iA haskell.compiler.ghc784
        [...]

        $ export PATH=~/ghc-7.6.3/bin:$PATH
        $ ghc --numeric-version
        7.6.3

- Temporary environment:

        $ nix-shell -p haskell.compiler.ghc7101
        [nix-shell:~]$ ghc --numeric-version
        7.10.1
        [nix-shell:~]$ exit

-------------------------------------------------------------------------------

# Where to get help ...

- `https://nixos.org/`

- `nix-dev@lists.science.uu.nl`

- `#nixos` auf `irc.freenode.org`
