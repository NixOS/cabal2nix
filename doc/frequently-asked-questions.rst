==========================
Frequently Asked Questions
==========================

How to build with profiling enabled
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every Haskell package set takes a function called ``overrides`` that you
can use to manipulate the package as much as you please. One useful
application of this feature is to replace the default ``mkDerivation``
function with one that enables library profiling for all packages. To
accomplish that add the following snippet to your
``~/.config/nixpkgs/config.nix`` file:

.. code:: nix

   {
     packageOverrides = super: let self = super.pkgs; in
     {
       profiledHaskellPackages = self.haskellPackages.override {
         overrides = self: super: {
           mkDerivation = args: super.mkDerivation (args // {
             enableLibraryProfiling = true;
           });
         };
       };
     };
   }

Then, replace instances of ``haskellPackages`` in the
``cabal2nix``-generated ``default.nix`` or ``shell.nix`` files with
``profiledHaskellPackages``.

How to override package versions in a compiler-specific package set
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Nixpkgs provides the latest version of
```ghc-events`` <http://hackage.haskell.org/package/ghc-events>`__,
which is 0.4.4.0 at the time of this writing. This is fine for users of
GHC 7.10.x, but GHC 7.8.4 cannot compile that binary. Now, one way to
solve that problem is to register an older version of ``ghc-events`` in
the 7.8.x-specific package set. The first step is to generate Nix build
instructions with ``cabal2nix``:

.. code:: shell

   cabal2nix cabal://ghc-events-0.4.3.0 > ~/.nixpkgs/ghc-events-0.4.3.0.nix

Then add the override in ``~/.config/nixpkgs/config.nix``:

.. code:: nix

   {
     packageOverrides = super: let self = super.pkgs; in
     {
       haskell = super.haskell // {
         packages = super.haskell.packages // {
           ghc784 = super.haskell.packages.ghc784.override {
             overrides = self: super: {
               ghc-events = self.callPackage ./ghc-events-0.4.3.0.nix {};
             };
           };
         };
       };
     };
   }

This code is a little crazy, no doubt, but it’s necessary because the
intuitive version

.. code:: nix

   { # ...

     haskell.packages.ghc784 = super.haskell.packages.ghc784.override {
       overrides = self: super: {
         ghc-events = self.callPackage ./ghc-events-0.4.3.0.nix {};
       };
     };
   }

doesn’t do what we want it to: that code replaces the ``haskell``
package set in Nixpkgs with one that contains only one
entry,\ ``packages``, which contains only one entry ``ghc784``. This
override loses the ``haskell.compiler`` set, and it loses the
``haskell.packages.ghcXYZ`` sets for all compilers but GHC 7.8.4. To
avoid that problem, we have to perform the convoluted little dance from
above, iterating over each step in hierarchy.

Once it’s accomplished, however, we can install a variant of
``ghc-events`` that’s compiled with GHC 7.8.4:

.. code:: shell

   nix-env -f "<nixpkgs>" -iA haskell.packages.ghc784.ghc-events

Unfortunately, it turns out that this build fails again while executing
the test suite! Apparently, the release archive on Hackage is missing
some data files that the test suite requires, so we cannot run it. We
accomplish that by re-generating the Nix expression with the
``--no-check`` flag:

.. code:: shell

   cabal2nix --no-check cabal://ghc-events-0.4.3.0 > ~/.nixpkgs/ghc-events-0.4.3.0.nix

Now the builds succeeds.

Of course, in the concrete example of ``ghc-events`` this whole exercise
is not an ideal solution, because ``ghc-events`` can analyze the output
emitted by any version of GHC later than 6.12 regardless of the compiler
version that was used to build the ``ghc-events`` executable, so
strictly speaking there’s no reason to prefer one built with GHC 7.8.x
in the first place. However, for users who cannot use GHC 7.10.x at all
for some reason, the approach of downgrading to an older version might
be useful.

How to override packages in all compiler-specific package sets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous section we learned how to override a package in a single
compiler-specific package set. You may have some overrides defined that
you want to use across multiple package sets. To accomplish this you
could use the technique that we learned in the previous section by
repeating the overrides for all the compiler-specific package sets. For
example:

.. code:: nix

   {
     packageOverrides = super: let self = super.pkgs; in
     {
       haskell = super.haskell // {
         packages = super.haskell.packages // {
           ghc784 = super.haskell.packages.ghc784.override {
             overrides = self: super: {
               my-package = ...;
               my-other-package = ...;
             };
           };
           ghc822 = super.haskell.packages.ghc784.override {
             overrides = self: super: {
               my-package = ...;
               my-other-package = ...;
             };
           };
           ...
         };
       };
     };
   }

However there’s a more convenient way to override all compiler-specific
package sets at once:

.. code:: nix

   {
     packageOverrides = super: let self = super.pkgs; in
     {
       haskell = super.haskell // {
         packageOverrides = self: super: {
           my-package = ...;
           my-other-package = ...;
         };
       };
     };
   }

How to specify source overrides for your Haskell package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When starting a Haskell project you can use ``developPackage`` to define
a derivation for your package at the ``root`` path as well as source
override versions for Hackage packages, like so:

.. code:: nix

   # default.nix
   { compilerVersion ? "ghc842" }:
   let
     # pinning nixpkgs using new Nix 2.0 builtin `fetchGit`
     pkgs = import (fetchGit (import ./version.nix)) { };
     compiler = pkgs.haskell.packages."${compilerVersion}";
     pkg = compiler.developPackage {
       root = ./.;
       source-overrides = {
         # Let's say the GHC 8.4.2 haskellPackages uses 1.6.0.0 and your test suite is incompatible with >= 1.6.0.0
         HUnit = "1.5.0.0";
       };
     };
   in pkg

This could be used in place of a simplified ``stack.yaml`` defining a
Nix derivation for your Haskell package.

As you can see this allows you to specify only the source version found
on Hackage and nixpkgs will take care of the rest.

You can also specify ``buildInputs`` for your Haskell derivation for
packages that directly depend on external libraries like so:

.. code:: nix

   # default.nix
   { compilerVersion ? "ghc842" }:
   let
     # pinning nixpkgs using new Nix 2.0 builtin `fetchGit`
     pkgs = import (fetchGit (import ./version.nix)) { };
     compiler = pkgs.haskell.packages."${compilerVersion}";
     pkg = compiler.developPackage {
       root = ./.;
       source-overrides = {
         HUnit = "1.5.0.0"; # Let's say the GHC 8.4.2 haskellPackages uses 1.6.0.0 and your test suite is incompatible with >= 1.6.0.0
       };
     };
     # in case your package source depends on any libraries directly, not just transitively.
     buildInputs = [ zlib ];
   in pkg.overrideAttrs(attrs: {
     buildInputs = attrs.buildInputs ++ buildInputs;
   })

Notice that you will need to override (via ``overrideAttrs`` or similar)
the derivation returned by the ``developPackage`` Nix lambda as there is
no ``buildInputs`` named argument you can pass directly into the
``developPackage`` lambda.

How to recover from GHC’s infamous non-deterministic library ID bug
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC and distributed build farms don’t get along well:

-  https://ghc.haskell.org/trac/ghc/ticket/4012

When you see an error like this one

::

   package foo-0.7.1.0 is broken due to missing package
   text-1.2.0.4-98506efb1b9ada233bb5c2b2db516d91

then you have to download and re-install ``foo`` and all its dependents
from scratch:

.. code:: shell

   nix-store -q --referrers /nix/store/*-haskell-text-1.2.0.4 \
     | xargs -L 1 nix-store --repair-path

If you’re using additional Hydra servers other than ``hydra.nixos.org``,
then it might be necessary to purge the local caches that store data
from those machines to disable these binary channels for the duration of
the previous command, i.e. by running:

.. code:: shell

   rm ~/.cache/nix/binary-cache*.sqlite

Builds on Darwin fail with ``math.h`` not found
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Users of GHC on Darwin have occasionally reported that builds fail,
because the compiler complains about a missing include file:

::

   fatal error: 'math.h' file not found

The issue has been discussed at length in `ticket
6390 <https://github.com/NixOS/nixpkgs/issues/6390>`__, and so far no
good solution has been proposed. As a work-around, users who run into
this problem can configure the environment variables

.. code:: shell

   export NIX_CFLAGS_COMPILE="-idirafter /usr/include"
   export NIX_CFLAGS_LINK="-L/usr/lib"

in their ``~/.bashrc`` file to avoid the compiler error.

Builds using Stack complain about missing system libraries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   --  While building package zlib-0.5.4.2 using:
     runhaskell -package=Cabal-1.22.4.0 -clear-package-db [... lots of flags ...]
   Process exited with code: ExitFailure 1
   Logs have been written to: /home/foo/src/stack-ide/.stack-work/logs/zlib-0.5.4.2.log

   Configuring zlib-0.5.4.2...
   Setup.hs: Missing dependency on a foreign library:
   * Missing (or bad) header file: zlib.h
   This problem can usually be solved by installing the system package that
   provides this library (you may need the "-dev" version). If the library is
   already installed but in a non-standard location then you can use the flags
   --extra-include-dirs= and --extra-lib-dirs= to specify where it is.
   If the header file does exist, it may contain errors that are caught by the C
   compiler at the preprocessing stage. In this case you can re-run configure
   with the verbosity flag -v3 to see the error messages.

When you run the build inside of the nix-shell environment, the system
is configured to find ``libz.so`` without any special flags – the
compiler and linker “just know” how to find it. Consequently, Cabal
won’t record any search paths for ``libz.so`` in the package
description, which means that the package works fine inside of
nix-shell, but once you leave the shell the shared object can no longer
be found. That issue is by no means specific to Stack: you’ll have that
problem with any other Haskell package that’s built inside of nix-shell
but run outside of that environment.

You can remedy this issue in several ways. The easiest is to add a
``nix`` section to the ``stack.yaml`` like the following:

.. code:: yaml

   nix:
     enable: true
     packages: [ zlib ]

Stack’s Nix support knows to add ``${zlib.out}/lib`` and
``${zlib.dev}/include`` as an ``--extra-lib-dirs`` and
``extra-include-dirs``, respectively. Alternatively, you can achieve the
same effect by hand. First of all, run

::

   $ nix-build --no-out-link "<nixpkgs>" -A zlib
   /nix/store/alsvwzkiw4b7ip38l4nlfjijdvg3fvzn-zlib-1.2.8

to find out the store path of the system’s zlib library. Now, you can

1. add that path (plus a “/lib” suffix) to your ``$LD_LIBRARY_PATH``
   environment variable to make sure your system linker finds
   ``libz.so`` automatically. It’s no pretty solution, but it will work.

2. As a variant of (1), you can also install any number of system
   libraries into your user’s profile (or some other profile) and point
   ``$LD_LIBRARY_PATH`` to that profile instead, so that you don’t have
   to list dozens of those store paths all over the place.

3. The solution I prefer is to call stack with an appropriate
   –extra-lib-dirs flag like so:
   ``shell     stack --extra-lib-dirs=/nix/store/alsvwzkiw4b7ip38l4nlfjijdvg3fvzn-zlib-1.2.8/lib build``

Typically, you’ll need ``--extra-include-dirs`` as well. It’s possible
to add those flag to the project’s ``stack.yaml`` or your user’s global
``~/.stack/global/stack.yaml`` file so that you don’t have to specify
them manually every time. But again, you’re likely better off using
Stack’s Nix support instead.

The same thing applies to ``cabal configure``, of course, if you’re
building with ``cabal-install`` instead of Stack.

Creating statically linked binaries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two levels of static linking. The first option is to configure
the build with the Cabal flag ``--disable-executable-dynamic``. In Nix
expressions, this can be achieved by setting the attribute:

::

   enableSharedExecutables = false;

That gives you a binary with statically linked Haskell libraries and
dynamically linked system libraries.

To link both Haskell libraries and system libraries statically, the
additional flags
``--ghc-option=-optl=-static --ghc-option=-optl=-pthread`` need to be
used. In Nix, this is accomplished with:

::

   configureFlags = [ "--ghc-option=-optl=-static" "--ghc-option=-optl=-pthread" ];

It’s important to realize, however, that most system libraries in Nix
are built as shared libraries only, i.e. there is just no static library
available that Cabal could link!

Building GHC with integer-simple
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default GHC implements the Integer type using the `GNU Multiple
Precision Arithmetic (GMP) library <https://gmplib.org/>`__. The
implementation can be found in the
`integer-gmp <http://hackage.haskell.org/package/integer-gmp>`__
package.

A potential problem with this is that GMP is licensed under the `GNU
Lesser General Public License
(LGPL) <https://www.gnu.org/copyleft/lesser.html>`__, a kind of
“copyleft” license. According to the terms of the LGPL, paragraph 5, you
may distribute a program that is designed to be compiled and dynamically
linked with the library under the terms of your choice (i.e.,
commercially) but if your program incorporates portions of the library,
if it is linked statically, then your program is a “derivative”–a “work
based on the library”–and according to paragraph 2, section c, you “must
cause the whole of the work to be licensed” under the terms of the LGPL
(including for free).

The LGPL licensing for GMP is a problem for the overall licensing of
binary programs compiled with GHC because most distributions (and
builds) of GHC use static libraries. (Dynamic libraries are currently
distributed only for macOS.) The LGPL licensing situation may be worse:
even though `The Glasgow Haskell Compiler
License <https://www.haskell.org/ghc/license>`__ is essentially a “free
software” license (BSD3), according to paragraph 2 of the LGPL, GHC must
be distributed under the terms of the LGPL!

To work around these problems GHC can be build with a slower but
LGPL-free alternative implementation for Integer called
`integer-simple <http://hackage.haskell.org/package/integer-simple>`__.

To get a GHC compiler build with ``integer-simple`` instead of
``integer-gmp`` use the attribute:
``haskell.compiler.integer-simple."${ghcVersion}"``. For example:

::

   $ nix-build -E '(import <nixpkgs> {}).haskell.compiler.integer-simple.ghc802'
   ...
   $ result/bin/ghc-pkg list | grep integer
       integer-simple-0.1.1.1

The following command displays the complete list of GHC compilers build
with ``integer-simple``:

::

   $ nix-env -f "<nixpkgs>" -qaP -A haskell.compiler.integer-simple
   haskell.compiler.integer-simple.ghc7102  ghc-7.10.2
   haskell.compiler.integer-simple.ghc7103  ghc-7.10.3
   haskell.compiler.integer-simple.ghc722   ghc-7.2.2
   haskell.compiler.integer-simple.ghc742   ghc-7.4.2
   haskell.compiler.integer-simple.ghc783   ghc-7.8.3
   haskell.compiler.integer-simple.ghc784   ghc-7.8.4
   haskell.compiler.integer-simple.ghc801   ghc-8.0.1
   haskell.compiler.integer-simple.ghc802   ghc-8.0.2
   haskell.compiler.integer-simple.ghcHEAD  ghc-8.1.20170106

To get a package set supporting ``integer-simple`` use the attribute:
``haskell.packages.integer-simple."${ghcVersion}"``. For example use the
following to get the ``scientific`` package build with
``integer-simple``:

.. code:: shell

   nix-build -A haskell.packages.integer-simple.ghc802.scientific

Quality assurance
~~~~~~~~~~~~~~~~~

The ``haskell.lib`` library includes a number of functions for checking
for various imperfections in Haskell packages. It’s useful to apply
these functions to your own Haskell packages and integrate that in a
Continuous Integration server like `hydra <https://nixos.org/hydra/>`__
to assure your packages maintain a minimum level of quality. This
section discusses some of these functions.

failOnAllWarnings
^^^^^^^^^^^^^^^^^

Applying ``haskell.lib.failOnAllWarnings`` to a Haskell package enables
the ``-Wall`` and ``-Werror`` GHC options to turn all warnings into
build failures.

buildStrictly
^^^^^^^^^^^^^

Applying ``haskell.lib.buildStrictly`` to a Haskell package calls
``failOnAllWarnings`` on the given package to turn all warnings into
build failures. Additionally the source of your package is gotten from
first invoking ``cabal sdist`` to ensure all needed files are listed in
the Cabal file.

checkUnusedPackages
^^^^^^^^^^^^^^^^^^^

Applying ``haskell.lib.checkUnusedPackages`` to a Haskell package
invokes the
`packunused <http://hackage.haskell.org/package/packunused>`__ tool on
the package. ``packunused`` complains when it finds packages listed as
build-depends in the Cabal file which are redundant. For example:

::

   $ nix-build -E 'let pkgs = import <nixpkgs> {}; in pkgs.haskell.lib.checkUnusedPackages {} pkgs.haskellPackages.scientific'
   these derivations will be built:
     /nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv
   ...
   detected package components
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    - library
    - testsuite(s): test-scientific
    - benchmark(s): bench-scientific*

   (component names suffixed with '*' are not configured to be built)

   library
   ~~~~~~~

   The following package dependencies seem redundant:

    - ghc-prim-0.5.0.0

   testsuite(test-scientific)
   ~~~~~~~~~~~~~~~~~~~~~~~~~~

   no redundant packages dependencies found

   builder for ‘/nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv’ failed with exit code 1
   error: build of ‘/nix/store/3lc51cxj2j57y3zfpq5i69qbzjpvyci1-scientific-0.3.5.1.drv’ failed

As you can see, ``packunused`` finds out that although the testsuite
component has no redundant dependencies the library component of
``scientific-0.3.5.1`` depends on ``ghc-prim`` which is unused in the
library.
