# list-haskell-store-paths.nix

let

  inherit (import <nixpkgs> { config.allowBroken = true; }) lib haskell;

  listStorePaths = pkgset: lib.mapAttrs
                             (attr: drv: builtins.toPath drv)
                             (lib.filterAttrs isActiveBuild haskell.packages.${pkgset});

  isActiveBuild = name: drv: let r = builtins.tryEval (hasNonEmptyPlatforms drv && isNotBroken drv);
                             in if r.success then r.value else false;

  hasNonEmptyPlatforms = drv: lib.isDerivation drv && getPlatforms drv != [];

  isNotBroken = drv: !(lib.attrByPath ["meta" "broken"] false drv);

  getPlatforms = drv: lib.attrByPath ["hydraPlatforms"]
                         (lib.attrByPath ["Platforms"] [])
                           (drv.meta or {});

in

# Using "lib.attrNames haskell.packages" instead of pkgset would be nice, but
# nix-instantiate doesn't cope.

{ pkgset }: lib.genAttrs [pkgset] listStorePaths
