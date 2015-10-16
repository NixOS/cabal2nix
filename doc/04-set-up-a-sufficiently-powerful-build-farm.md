Set up a Sufficiently Powerful Build Farm
=========================================

The Problem
-----------

`hydra.nixos.org` does not compile any of our LTS Haskell package sets. This
means that users of `haskell.packages.lts-x_y` cannot get any pre-compiled
binaries. It also means that those builds aren't verified, i.e. we won't notice
when changes to Nixpkgs break builds in those package sets. Furthermore, we
have [no pre-compiled binaries with profiling support][1].

The Situation Today
-------------------

We have 66 active package sets that define the following number of active
builds per platform:

            pkgset     builds
     1:    ghc6123       5173
     2:     ghc704       5182
     3:    ghc7102       5189
     4:     ghc722       5183
     5:     ghc742       5183
     6:     ghc763       5182
     7:     ghc783       5173
     8:     ghc784       5173
     9:    ghcHEAD       5188
    10: ghcNokinds       5188
    11:      ghcjs       5172
    12:    lts-0_0        795
    13:    lts-0_1        795
    14:    lts-0_2        795
    15:    lts-0_3        795
    16:    lts-0_4        795
    17:    lts-0_5        795
    18:    lts-0_6        795
    19:    lts-0_7        795
    20:    lts-1_0        827
    21:    lts-1_1        827
    22:   lts-1_10        828
    23:   lts-1_11        829
    24:   lts-1_12        829
    25:   lts-1_13        829
    26:   lts-1_14        830
    27:   lts-1_15        831
    28:    lts-1_2        828
    29:    lts-1_4        828
    30:    lts-1_5        828
    31:    lts-1_7        828
    32:    lts-1_8        828
    33:    lts-1_9        827
    34:    lts-2_0       1019
    35:    lts-2_1       1019
    36:   lts-2_10       1023
    37:   lts-2_11       1023
    38:   lts-2_12       1023
    39:   lts-2_13       1022
    40:   lts-2_14       1022
    41:   lts-2_15       1022
    42:   lts-2_16       1022
    43:   lts-2_17       1023
    44:   lts-2_18       1022
    45:   lts-2_19       1022
    46:    lts-2_2       1018
    47:   lts-2_20       1024
    48:   lts-2_21       1023
    49:   lts-2_22       1023
    50:    lts-2_3       1018
    51:    lts-2_4       1018
    52:    lts-2_5       1018
    53:    lts-2_6       1017
    54:    lts-2_7       1017
    55:    lts-2_8       1023
    56:    lts-2_9       1023
    57:    lts-3_0       1322
    58:    lts-3_1       1322
    59:    lts-3_2       1321
    60:    lts-3_3       1321
    61:    lts-3_4       1321
    62:    lts-3_5       1322
    63:    lts-3_6       1321
    64:    lts-3_7       1323
    65:    lts-3_8       1323
    66:    lts-3_9       1324
            pkgset     builds

That gives a total of 111,647 active builds. Many of those builds are
identical: all package sets combined define 77,445 distinct store paths, i.e.
some 34,202 builds can be shared across package sets.

Now, `hydra.nixos.org` compiles only `haskellPackages` at the moment. Out of a
total of [46,862 builds in trunk][2], 15,446 (33%) come from the Haskell
package set. If we'd enable every Haskell package set on Linux/i686,
Linux/x86_64, and Darwin/x86_64, then we'd have a total of 263,751 builds ---
5.6 times as much as before ---, and 88% of all builds would be related to
Haskell.

A complete set of builds of `haskellPackages` takes up approx. 80 GByte of disk
space per platform, i.e. 240 GByte for all our 3 active platforms. How would
that number develop if we'd enable everything? The store path sizes in MByte
are distributed as follows (based on 13,070 samples excluding `ghc`):

    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
    0.02    0.37    0.91    3.92    2.71  678.90

Multiplying the average size by the number of distinct store paths tells us
that storing *everything* requires approx. 300 GByte per platform. With 3
active platforms, we'd need about 1 TByte of disk space for one complete set of
Haskell packages.

Now, we might be able to reduce that number by disabling some particularly
large builds. It turns out that the size distribution is skewed to the left,
i.e. towards smaller builds. Approximately 81% of all store paths lie below the
average. Our top-20 biggest Haskell builds are:

                        pkg  size
     1:                 ghc 895.5
     2:            metadata 678.9
     3:           uhc-light 253.5
     4:           OpenGLRaw 249.3
     5:             FpMLv53 229.5
     6:        amazonka-ec2 214.8
     7:                Agda 212.6
     8:                 xhb 189.0
     9:  unicode-properties 175.0
    10:    scholdoc-texmath 165.0
    11:               idris 137.7
    12:                  gf 126.4
    13:              pandoc 124.8
    14:       unicode-names 118.4
    15:              wxcore 113.8
    16:      java-character 112.1
    17:                 hat 111.1
    18:             texmath 109.6
    19:      open-symbology 107.6
    20: turkish-deasciifier 104.0

If we'd make an effort to disable some of those expensive builds --- or maybe
reduce their output size ---, then that would make a noticeable dent into the
space requirements.

Anyway, it's clear that `hydra.nixos.org` cannot cope with that load. Curiously
enough, compiling the packages is the least of our problems; that's no big
deal. But evaluating 66 package sets with some 111,000 derivations in them
every 10 minutes might be unfeasible. Hydra has undergone some architectural
changes recently that might make a difference, but I don't have any reliable
data concerning the performance of the `hydra-evaluator` process, so I cannot
say.

We know for sure that the currently available disk space would not suffice.
Disk space is notoriously low on `hydra.nixos.org` and storing another terabyte
Haskell data is certainly impossible at the moment.

Possible Improvements
---------------------






[1]: https://github.com/NixOS/nixpkgs/issues/10143
[2]: http://hydra.nixos.org/jobset/nixpkgs/trunk
