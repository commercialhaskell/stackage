## Introduction

Stackage provides a series of snapshots: a set of tuples of packages and exact versions that is free of incompatibilities. It relies on two parties: Firstly repeated automated testing ensures that incompatibilities with newly released packages are detected, and secondly maintainers update their packages to keep them up to date with their dependencies.

Membership in any stackage snapshot is not reflected in a package's content; the stackage repository contains the metadata that determines membership. The repository relies on the following social contract in the hopes of enabling a stable set of packages, optimally never dropping any packages from newer snapshots.

# Agreement

1. In the following, the term "package maintainer" refers to the person responsible for Stackage membership of said package. Such a person need not be the hackage maintainer of the package, although it certainly makes sense for the stackage-side maintainer to have write access should the need for updating a package arise. Please consider contacting the hackage maintainer(s) if you wish adding a package to Stackage.

2. The following conditions apply to any package included in the (next) snapshot:

    * It must be compatible with the latest versions of all of its dependencies;
    * It must be compatible with the versions of libraries (aka `bootlibs`) shipping with the GHC connected to the snapshot (see the note below);
    * It must compile and test successfully with the usual automated building infrastructure (e.g. by the hackage buildbot);
    * (It probably should also be compatible with the latest published snapshot.)

3. The package maintainer is expected to ensure that a package continues to meet the above requirements by updating/fixing a package when dependencies are updated. The recommended timeframe for such fixes are:

    * **Up to one week** if restrictive version bounds are the only problem;
    * **Between a week and a month**, depending on the work required, if the cause of the issue is a breaking change in a dependency.

4. Failing to meet these requirements

    Packages that fail to meet above requirements can be removed from future snapshots, if the package maintainer fails to respect the above timeframes (e.g. we rather remove random package B than have it prevent updating of wildly used package A). This is not ideal for other users, but is not the end of the world either - existing snapshots are not affected, and packages can always be added back again later.

    Maintainers are encouraged to seek for co-maintainers if they are too busy for keeping the package up to date.


# Notes

- To ensure that package are complete and compile/test successfully we recommend [the Stack Travis script](http://docs.haskellstack.org/en/stable/GUIDE.html#travis-with-caching).

- To test that pvp-compliant restrictive upper bounds are up to date to the dependencies' latest versions you can find visit `http://packdeps.haskellers.com/feed?needle=PACKAGENAME`.

- When a new version is released for a bootlib (`binary`, `containers`, `process`, etc.), the latest version becomes distinct from the version distributed as part of a snapshot's GHC release. Packages should support both these versions. See [more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds) for further explanation.

- It is highly recommended that all package maintainers follow the dependencies of their packages on [Packdeps](http://packdeps.haskellers.com/), typically using the RSS feeds.


# Processes

## Adding a package

To add your package, first fork this repository.
In the [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) file, there's a section called `packages`.
To add a set of packages, you would add:

    "My Name myemail@example.com @mygithubuser":
        - package1
        - package2
        - package3

After doing that, send a pull request (with a commit message like "add foo-bar"). We do not require new submissions to be tested against the rest of Stackage before the pull request (though it is a good idea to do so if you can with `stack --resolver nightly exec stackage-curator check` and `stack --resolver nightly build`), provided you meet the dependency version requirements above. If your library depends on a C library, add a note to your pull request with the Ubuntu library name, or even better edit the `debian-bootstrap.sh` script directly

If you want to make sure that the package builds against the newest versions of all dependecies you can do this:
```
$ cabal update
$ ghc --version # Should give v8.0.1
$ cabal get PACKAGE-VERSION # e.g. aeson-0.11.2.1
$ cd PACKAGE-VERSION
$ cabal sandbox init # Should give "Creating a new sandbox" and not "Using an existing sandbox".
$ cabal install --enable-tests --enable-benchmarks --dry-run | grep latest # Should give no results
$ cabal install --enable-tests --enable-benchmarks --allow-newer
$ cabal test
```

**NB** Please use commit messages like "add foo-bar" or "add johndev's packages"
(`build-constraints.yaml` is the most frequently changed file in this git repo
so commit messages like "update build-constraints.yaml" are not helpful).

**NB2** There can be a delay of up to an hour before package versions
newly uploaded to Hackage appear to our build server. If you just
uploaded a package to Hackage that you're trying to get included, we
recommend waiting an hour before opening the PR. You can verify this
by making sure the latest version is listed at 
https://github.com/commercialhaskell/all-cabal-metadata/tree/master/packages/.


## Uploading a new version for an included package

When a new version of a package is uploaded to Hackage, we automatically try to include it in Stackage (unless the new version is considered experimental). That can result in a number of possible failures. If there is a failure we temporarily introduce an upper bound, and raise GitHub issue tickets to resolve the issue.

If the new version doesn't compile then the package author should quickly (within 1 week) upload a fixed version.

If a package's test suite is failing, the first job is to investigate why. If this is due to a bad interaction with versions of other packages in Stackage, then it is the responsibility of the maintainer to fix the test suite. In some situations, it is acceptable to not run the test suite.


## Upgrading to a new GHC version

The Stackage curation team tries to move Stackage Nightly to new versions of GHC quickly as they become available, while keeping LTS Haskell on a regular release schedule. For package maintainers, the most important impacts of a new GHC release are:

* We will typically do a sweep through the Stackage upper bounds and aggressively remove packages that block them. This is because, in most cases, we will need to move to the newest versions of a package to get support for the latest GHC, and asking package maintainers to backport their fixes is an undue burden
    * We will definitely do this at a GHC major version release, and may do so at a minor version release
* Packages that are incompatible with the newest GHC version will be temporarily blocked

If your package ends up being temporarily removed from Stackage Nightly, please simply send a pull request to add it back once it and its dependencies are compatible with the newest GHC version.


## Adding a package to an LTS snapshot

The steps above affect the Stackage Nightly builds, but do not directly affect
LTS Haskell builds. When we build a new LTS Haskell major version (anything
ending in `.0`), the package set is taken from Stackage Nightly. Therefore, by
following the above steps, you can get your package into the next major LTS
Haskell release.

If you would like to get your package added to an existing LTS Haskell major
release (e.g., if `lts-3.21` is out, you would want your package to appear in
`lts-3.22`), please do the following in addition to the steps above:

* Open up a new issue on the [lts-haskell repo](https://github.com/fpco/lts-haskell/issues/new)
* Specify the LTS major version you would like your package to go into (e.g., lts-3)
* Provide a list of packages you would like added, and if relevant, any upper bounds on those packages
* Be patient! The LTS releases are by their nature more conservative than nightly, and therefore adding new packages is a more manual process. The Stackage curators will try to get to your issue quickly, but there may be some delay.
