Set up a Sufficiently Powerful Build Farm
=========================================

The Problem
-----------

`hydra.nixos.org` compiles and provides binaries only for the `haskellPackages`
package set. The build farm compiles none of our LTS Haskell package sets,
which means that users of `haskell.packages.lts-x_y` cannot get any
pre-compiled binaries. It also means that those builds aren't verified, i.e. we
won't notice when changes to Nixpkgs break builds in those package sets.

Furthermore, we have [no pre-compiled binaries with profiling support][1] for
any of our package sets.

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

That gives a total of 111,647 active builds, many of which are identical. All
package sets combined define 77,445 distinct store paths, i.e. some 34,202
builds are shared across package sets.

Now, `hydra.nixos.org` compiles only `haskellPackages` at the moment. Out of a
total of [46,862 builds in trunk][2], 15,446 (33%) come from the Haskell
package set. If we'd enable every Haskell package set on Linux/i686,
Linux/x86_64, and Darwin/x86_64, then we'd have a total of 263,751 builds ---
5.6 times as much as before ---, and 88% of all builds would be related to
Haskell.

A complete build of the active derivations in `haskellPackages` takes up
approx. 27 GByte of disk space per platform. That gives about 80 GByte for all
of our 3 active platforms. How would that number develop if we'd enable
everything? The store path sizes in MByte are distributed as follows (based on
7,620 samples excluding `ghc`):

    Minimum  1st Quart.  Median     Mean   3rd Quart.  Maximum
    0.0169   0.3557      0.9497   4.6300   3.0640      678.9000

Multiplying the average store path size by the number of distinct store paths
tells us that storing *everything* requires approx. 360 GByte per platform.
With 3 active platforms, we'd need about 1 TByte of disk space for one complete
set of Haskell packages.

Now, we might be able to reduce that number by disabling some particularly
large builds. The store path size distribution is skewed to the left, i.e.
towards smaller builds. Approximately 82% of all store paths are actually
smaller than the numerical average. Our top-20 biggest Haskell builds are:

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
reduce their output size ---, then we'd make a noticeable dent into the space
requirements. Even so, it's clear that `hydra.nixos.org` cannot provide that
much disk space today.

Curiously enough, the CPU power necessary to compile all those packages is the
least of our problems. Our build farm can easily re-compile everything from
scratch within 2-3 days, which is "good enough" for all practical purposes.
Also, changes to `stdenv` occur rarely (and we typically know about them in
advance). The normal update cycle triggers only a handful of builds -- maybe
20-300 per day --, because the versions fundamental Haskell packages are fixed
in the LTS package sets.

It's unclear whether the Hydra software would cope with 66 package sets with
some 111,000 derivations in them that need to be evaluated, say, once an hour.
Hydra has undergone some architectural changes recently that might make such a
load possible --- i.e. `hydra-evaluator` is more efficient than it used to be
---, but I don't have any reliable data concerning the performance of the
process, so I cannot say what is possible and what is not.

We know for sure that the currently available disk space doesn't suffice. Disk
space is notoriously low on `hydra.nixos.org`, and storing another terabyte
Haskell data is certainly impossible at the moment.

Possible Improvements
---------------------

We have basically two alternatives:

1. Throw hardware (money) at `hydra.nixos.org`.
2. Establish a separate build farm for Haskell packages.

Either solution requires money, which we could probably raise through crowd
funding. At the moment, the NixOS Foundation collects donations for purposes of
NixOS in general, but it should be possible to start a funding campaign that
collects donations specifically for the purposes of establishing a Haskell
build farm so that people who care about that particular topic have an
incentive to participate.

Now, if we'd go for approach (1), then we could use those funds to buy bigger
disks and more RAM for `hydra.nixos.org`, which would be beneficial for
everyone -- not just Haskell users. The downside is that `hydra.nixos.org` is a
bit of a black box. Only very few people have access to those machines, and
that situation is not going to change any time soon. Personally, I have no idea
whether adding RAM or disks to the cluster is feasible at all, and whether
those upgrades would enable the build farm to cope with the number of builds
that we're considering here.

Solution (2) seems more manageable, because we could set up an environment from
scratch as we see fit. Experience from managing `hydra.cryp.to` suggests that
one powerful KVM-based virtual server can serve as the Hydra master. In
addition, we'd need 2-3 additional build slaves to compile packages. For
massive re-builds, we could spawn another 10-15 builds slaves in EC2 to reduce
the time it takes to re-build everything from scratch. Such a setup would
probably work well in practice, and it should be available at a yearly cost of
1,000 dollars or less.

Anyhow, that's just a rough estimate. I don't know, really, what an ideal
hardware / service platform for running such a virtual service would be. It
would be great if a resident virtual server / NAS / system management guru
could chime in with suggestions; I'm sure the NixOS crowd has people who know
that kind of stuff and who can design the infrastructure for such a build farm.


[1]: https://github.com/NixOS/nixpkgs/issues/10143
[2]: http://hydra.nixos.org/jobset/nixpkgs/trunk
