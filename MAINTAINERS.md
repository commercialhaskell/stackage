This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

The idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. Specifically, we aim for:

* All packages are buildable and testable from Hackage. We recommend [the Stack Travis script](https://docs.haskellstack.org/en/stable/travis_ci/), which ensures a package is not accidentally incomplete.
* All packages are compatible with the newest versions of all dependencies (You can find restrictive upper bounds by visiting http://packdeps.haskellers.com/feed?needle=PACKAGENAME).
* All packages in a snapshot are compatible with the versions of libraries that ship with the GHC used in the snapshot ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Packages in Stackage are not patched: all package changes occur upstream in Hackage.

## Adding a package

Anyone can add any package to Stackage but you may only add packages under your own name. It's highly encouraged that the actual package maintainer is also the Stackage maintainer, if that is not the case you should drop the package maintainer a note first.

To add your package you can edit [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) directly on github or fork the project. There's a section called `packages` where you would add yourself and your packages:

    "My Name <myemail@example.com> @mygithubuser":
        - package1
        - package2
        - package3

If your library depends on a C library, please add it to the `debian-bootstrap.sh` script.

After doing that commit with a message like "add foo-bar" and send a pull request.

The continuous integration job will do some checks to see if your package's dependencies are up-to-date.

The CI job notably doesn't compile packages, run tests, build documentation, or find missing C libraries.
If you want to be proactive or if CI fails, you can make sure that your package builds against the latest nightly:

```
# Build from the tarball on Hackage to check for missing files
$ stack unpack yourpackage && cd yourpackage-*
# Generate a pristine stack.yaml, adding any missing extra-deps
$ rm -f stack.yaml && stack init --resolver nightly --solver
# Build, generate docs, test, and build benchmarks
$ stack build --resolver nightly --haddock --test --bench --no-run-benchmarks
```

This approach works well, but has two limitations you should be aware
of:

* It won't notify you of restrictive upper bounds in your package if
  Stackage has the same upper bounds. For that reason, we recommend
  using [Packdeps](http://packdeps.haskellers.com/) (see "Following
  dependency upgrades" below).
* If the latest Stackage Nightly is missing some of the latest
  packages, your build above may succeed whereas the Travis job may
  fail. Again: Packdeps will help you detect this situation.

Alternatively, you can build with `cabal`. Note that this may end up
using older dependency versions:

```
$ ghc --version # Should be the same as the latest nightly, it's in the title of https://www.stackage.org/nightly
$ cabal update
$ cabal get PACKAGE
$ cd PACKAGE-*
$ cabal sandbox init # Should give "Creating a new sandbox" and not "Using an existing sandbox".
$ cabal install --enable-tests --enable-benchmarks --dry-run | grep latest # Should give no results
$ cabal install --enable-tests --enable-benchmarks --allow-newer
$ cabal test
$ cabal haddock
```

## Github and Notifications

Stackage uses Github notifications to reduce overhead of contacting individual
package maintainers through various channels. As a package maintainer, you will
receive notifications for a number of reasons, package build failures of
different sorts, blockages and bounds issues, etc.

Please note, Github does some throttling on the number people that can be
notified within one issue. This means that on issues created with a large
number of packages affected, maintainers may not receive a notification. This
is not ideal, but Stackage is largely a manual process and done on a best
effort basis.

## Uploading a new package version

When a new version of a package in Stackage is uploaded to Hackage, we automatically try to include it in Stackage. That can result in a number of possible failures. If there is a failure we temporarily introduce an upper bound, and open a GitHub issue ticket to resolve the issue.

If the new version doesn't compile then the package author should upload a fixed version.

If a package's test suite is failing, the first job is to investigate why. If this is due to a bad interaction with versions of other packages in Stackage, then it is the responsibility of the maintainer to fix the test suite. In some situations, it is acceptable to not run the test suite.

## Following dependency upgrades

If a new version of a dependency is released your package may fail to
build or tests may fail. In most cases we will add an upper bound on
the dependency so that your package remains in nightly. We'll also
create a github issue pinging you with the bounds issues or give build
logs showing failures. It's then up to you to modify your package.

We recommend that you also follow the dependencies of your packages on
[Packdeps](http://packdeps.haskellers.com/) (typically using the RSS
feeds) as well as that often gives you notice ahead of stackage
issues. There are cases where we will not notice a new release of a
package because of other upper bounds that are in place.

If a package is not updated in time, it may be temporarily removed
from Stackage by the curator team. We strive to notify you when this
happens. If it does you are always welcome to file another pull
request to add it back.

We typically use fairly long windows before disabling packages, but it
is decided on a case-by-case basis.

* If restrictive version bounds are the only problem, we will give
  maintainers at least a week to respond.
* If there are real breaking changes, the curator team will retain
  more discretion on how long a window to give before dropping
  packages.
* We usually drop all upper bounds and disable packages when we create
  a new Long Term Support (LTS) major version.
* There are rare cases where an upper bound or build failure are hard
  to deal with so then we may disable

**NOTE** Previously we had stricter time limits, but we decided to
remove that: Anyone is free to add a package to Stackage regardless of
responsiveness guarantees. However, as stated above, we may elect to
temporarily remove a package if it is not updated in a timely manner.

## Delays

Maintainers are humans, humans get sick/have babies/go on
holiday. Sometimes a dependency upgrade is extra time consuming.

Consider finding a co-maintainer with access to SCM and Hackage to
help you out.

We appreciate if you notify us of any expected delays in the Github
issues, some kind soul might decide to help out.

## Upgrading to a new GHC version

The Stackage curation team tries to move Stackage Nightly to new
versions of GHC quickly as they become available, while keeping LTS
Haskell on a regular release schedule. For package maintainers, the
most important impacts of a new GHC release are:

* We will typically do a sweep through the Stackage upper bounds and
  remove blocking packages. We prefer to do it this way rather than
  ask other maintainers to backport fixes.
    * We will definitely do this at a GHC major version release, and
      may do so at a minor version release
* Packages that are incompatible with the newest GHC version will be
  temporarily disabled

If your package ends up being temporarily disabled from Stackage
Nightly, please simply send a pull request to add it back once it and
its dependencies are compatible with the newest GHC version.

Note that it is _not_ a goal of LTS Haskell to track the latest
version of GHC. If you want the latest and greatest, Stackage Nightly
is your best bet. In particular, LTS Haskell will often&mdash;but not
always&mdash;avoid upgrading to the first point release of GHC
releases (e.g., 8.2.1) to allow further testing and to get the
benefits of the first bugfix release (e.g., 8.2.2).

## Adding a package to an LTS snapshot

The steps above affect the Stackage Nightly builds, but do not directly affect
LTS Haskell builds. When we build a new LTS Haskell major version (anything
ending in `.0`), the package set is taken from Stackage Nightly. Therefore, by
following the above steps, you can get your package into the next major LTS
Haskell release.

If you would like to get your package added to an existing LTS Haskell major
release (e.g., if `lts-8.9` is out, you would want your package to appear in
`lts-8.10`), please do the following in addition to the steps above:

* Check that your package can be built with that LTS major version (e.g. `stack build --test --bench --haddock --resolver lts-8.10`)
* Open up a new issue on the [lts-haskell repo](https://github.com/fpco/lts-haskell/issues/new)
  * Specify the LTS major versions you would like your packages to go into (e.g. lts-8)
  * Provide a list of packages you would like added
    * If relevant, mention any upper bounds that are needed on those packages
* Be patient! The LTS releases are less frequent than Nightly. The
  Stackage curators will try to get to your issue as soon as possible,
  but it may take some time.
* We gradually stop maintainng old LTS major versions, so your
  request may take longer or be declined if it's for an old LTS.
