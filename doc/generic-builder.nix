{ stdenv, fetchurl, ghc }:
{ pname, version, sha256, buildDepends ? [], extraLibraries ? [], ... }:

stdenv.mkDerivation {
  name = "${pname}-${version}";

  src = fetchurl { url = "mirror://hackage/${pname}-${version}.tar.gz"; inherit sha256; };

  nativeBuildInputs = extraLibraries;
  propagatedNativeBuildInputs = buildDepends;

  configurePhase = ''
    export PATH="${ghc}/bin:$PATH"
    configureFlags="-v --prefix=$out --ghc-option=-j$NIX_BUILD_CORES $configureFlags"
    for p in ${stdenv.lib.concatStringsSep " " (stdenv.lib.closePropagation buildDepends)}; do
      if [ -d "$p/lib/ghc-${ghc.version}/package.conf.d" ]; then
        for db in "$p/lib/ghc-${ghc.version}/package.conf.d/"*".db"; do
          configureFlags+=" --package-db=$db"
        done
        continue
      fi
    done
    for p in $nativeBuildInputs; do
      if [ -d "$p/include" ]; then
        configureFlags+=" --extra-include-dirs=$p/include"
      fi
      for d in lib{,64}; do
        if [ -d "$p/$d" ]; then
          configureFlags+=" --extra-lib-dirs=$p/$d"
        fi
      done
    done
    ghc --make Setup
    echo configureFlags: $configureFlags
    ./Setup configure $configureFlags
  '';

  buildPhase = "./Setup build && ./Setup haddock";

  checkPhase = "./Setup check";

  installPhase = ''
    ./Setup copy
    local confDir=$out/lib/ghc-${ghc.version}/package.conf.d
    local packageDb=$confDir/${pname}-${version}.db
    local pkgConf=$confDir/${pname}-${version}.conf
    mkdir -p $confDir
    ./Setup register --gen-pkg-config=$pkgConf
    if test -f $pkgConf; then
      echo '[]' > $packageDb
      GHC_PACKAGE_PATH=$packageDb ghc-pkg --global register $pkgConf --force
    fi
  '';
}
