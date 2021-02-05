Packaging Workflow
==================

We used to distribute (at least) the latest version of every package on
Hackage, which resulted in some 15,669 packages being included in Nixpkgs. Of
those, approximately 6,111 (39%) packages actually built successfully. This
model seems pretty wasteful, because more than 60% of the information we
maintain in `hackage-packages.nix` doesn't have any purpose.

The goal of the new packaging workflow is to have 99% of all Haskell packages
that we distribute compile successfully. Packages that won't compile
successfully need to be fixed or they'll be dropped after an appropriate grace
period.

How to Add a Package
--------------------

1. A Nixpkgs user requests that a certain Hackage package be included in the
distribution through a custom web interface or a Github pull request. Users can
choose whether they want to be recorded as a maintainer for those packages or
not. Maintainers need to provide a provide a valid Github user name. Everyone
else needs a valid e-mail address where they can received some "please click on
this confirmation URL" message to actually trigger the action. If we know the
Github nickname, too, then we'll record that in the meta.maintainers section.

2. A `hackage2nix`-like bot generates the necessary build instructions and
creates a pull request on Github that adds the new package (and all of its
required dependencies). At the time we generate those new builds, our tools
check whether we can build all packages or whether there are unresolved version
constraints in our package set. If that is the case, we tell the user about
that issue and offer them to include a `doJailbreak` in the PR.

3. CI verifies that all new packages compile. If they do, the request is merged
automatically. If they don't, the PR needs further attention from a human
being.

How Packages Are Removed
------------------------

1. We frequently query Hydra for failing builds. A human checks that list of
build failures to make sure they are genuine, and if they are, we open a Github
issue for every build failure, report the problem, and assign the ticket to the
meta.maintainers.

2. We frequently query the list of open Github issues we created and compare
them to the list of build failures:

    a) If a build has succeeded on Hydra for, say 3 days, then we'll
    automatically close the Github ticket.

    b) If a ticket is closed but the build still fails on Hydra, then we'll
    trigger some kind of warning so that a human investigates the issue.

    c) If a build keeps failing for, say 2 weeks, then we'll add a comment to
    the ticket threatening to drop the package from Nixpkgs. If the build keeps
    failing, then we'll actually drop it from Nixpkgs after, say 3 weeks, and
    close the issue.
