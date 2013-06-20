% Managing the Haskell Dependency Hell with Nix
% Peter Simons \<simons@cryp.to\>
% 2013-06-21

# Ablauf der Präsentation

1) Vorstellung des Nix Paket-Managers

    - Übersicht der besonderen Features
    - Die Paket-Datenbank
    - Nix als Programmiersprache

2) Installation von Haskell-Paketen

    - Einfache Installation
    - Mehrere Versionen gleichzeitig installieren
    - Funktionsweise von `ghc-wrapper`
    - Nix in Kombination mit `cabal-dev`

3) Erweiterung der Nix Paket-Datenbank

    - Der `cabal2nix` Konverter

4) Continuous-Integration mit der Hydra Build-Farm

5) Fragen & Antworten

# Was ist Nix und warum will ich das wissen?

- Nix ist ein rein funktionaler Paket-Manager.

- Nix kann mehrere Paket-Versionen gleichzeitig installieren.

- Nix garantiert, dass alle Paket-Abhängigkeiten bekannt sind.

- Nix erlaubt atomische Updates.

- Nix erlaubt das Zurücknehmen von Updates.

- Nix erlaubt die Installation von Binär-Paketen.

- Nix ist unabhängig vom Betriebsystem.

- Nix hat erstklassige Unterstützung für Multi-User-Systeme.

- Nix ist freie Software unter GNU LGPL 2.1.

# Der Nix Paket-Store

- Nix lebt komplett unterhalb des Verzeichnisses `/nix`.

    - Nix deinstallieren: `rm -rf /nix`
    - Backup erstellen: `tar cfz nix-backup.tar.gz /nix`

- Meta-Daten liegen unterhalb von `/nix/var`.

- Alle Pakete werden unterhalb von `/nix/store` installiert:

        /nix/store/3768znryvs7nmnr3p2cdlr95hj-ghc-6.12.3
        /nix/store/8z0v9s56nc2f5zgdn6hgzchwba-ghc-7.0.4
        /nix/store/njwikqrziy7nsc18bpxlvncq1q-ghc-7.6.3

- In `/nix/store` installierte Pakete referenzieren niemals
  Dateien außerhalb des Stores.

# Die Paketdatenbank

- Nixpkgs ist eine Sammlung von Build-Rezepten, die in der rein
funktionalen Sprache *Nix* geschrieben sind.

- Ein Paket wird erzeugt durch die Funktion `derivation`:

        derivation :: String             -- Arch-OS
                   -> String             -- Name-Version
                   -> [String]           -- Build-Kommando
                   -> [StorePath]        -- Abhängigkeiten
                   -> [(String,String)]  -- Shell-Umgebung
                   -> IO StorePath

- Nixpkgs bietet verschiedene `mkDerivation`-Wrapper, um die Verwendung
von `derivation` vereinfachen.

# Das Paket `parsec 3.1.3`

    { cabal, mtl, text }:

    cabal.mkDerivation (self: {
      pname = "parsec";
      version = "3.1.3";
      sha256 = "1a64gzirgpa1i78gbbp9z059nh29xmcja4g8vgjb1";
      buildDepends = [ mtl text ];
    })

# Installation von `parsec 3.1.3`

    $ nix-env -iA haskellPackages.parsec_3_1_3
    installing `haskell-parsec-ghc7.6.3-3.1.3'
    these paths will be fetched:
      /nix/store/09zlb...-haskell-parsec-ghc7.6.3-3.1.3
      /nix/store/4p1rb...-haskell-mtl-ghc7.6.3-2.1.2
      /nix/store/i0k3s...-haskell-text-ghc7.6.3-0.11.3.1
      /nix/store/q8163...-haskell-transformers-ghc7.6.3-0.3.0.0
    [...]
    building path(s) `/nix/store/0xfxh...-user-environment'
    created 22 symlinks in user environment

# Das Paket für `yesod 1.2.1`

    { cabal, aeson, blazeHtml, blazeMarkup, dataDefault,
    , hamlet monadControl, networkConduit, safe,
    , shakespeareCss shakespeareJs text, transformers,
    , unorderedContainers wai, waiExtra, warp yaml,
    , yesodAuth, yesodCore yesodForm, yesodPersistent
    }:
    cabal.mkDerivation (self: {
      pname = "yesod";
      version = "1.2.1";
      sha256 = "19gwhav6sr6gd2kh92ga8a09hq9grllmnacdqk6zg";
      buildDepends = [ aeson blazeHtml blazeMarkup
        dataDefault hamlet monadControl networkConduit safe
        shakespeareCss shakespeareJs text transformers
        unorderedContainers wai waiExtra warp yaml yesodAuth
        yesodCore yesodForm yesodPersistent
      ];
    })

# Die Programmiersprache Nix

- Nix ist lazy-evaluiertes ungetyptes Lambda-Kalkül.

- Strings: `"hello world"`, `http://example.org/`

- Listen: `[ 12 "abc" http://example.org ]`

- Funktionen: `list: element: list ++ [element]`

- Dictionaries: { foo = "foo"; foo = "bar; }

- Rekursive Dictionaries:

        rec { bar = "foo"; bar = "bar; foobar = foo + bar; }

- Let-Statements:

        let append = { list ? [], element }: list++[element];
        in append { list = ["abc" 3]; element = null; }
