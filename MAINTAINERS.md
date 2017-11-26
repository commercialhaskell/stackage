This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

The idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. Specifically, we aim for:

* All packages are buildable and testable from Hackage. We recommend [the Stack Travis script](https://docs.haskellstack.org/en/stable/travis_ci/), which ensures a package is not accidentally incomplete.
* All packages are compatible with the newest versions of all dependencies (You can find restrictive upper bounds by visiting http://packdeps.haskellers.com/feed?needle=PACKAGENAME).
* All packages in a snapshot are compatible with the versions of libraries that ship with the GHC used in the snapshot ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Packages in Stackage are not patched: all package changes occur upstream in Hackage.

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

## Adding a package

Anyone can add any package to Stackage but you may only add packages under your own name. It's highly encouraged that the actual package maintainer is also the Stackage maintainer, if that is not the case you should drop the package maintainer a note first.

To add your package, first fork this repository.
In the [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) file, there's a section called `packages`.
To add a set of packages, you would add:

    "My Name <myemail@example.com> @mygithubuser":
        - package1
        - package2
        - package3

Note that the `master` branch is used for Stackage Nightly (not the `nightly` branch, which is used for the nightly docker builder imagine).

After doing that, send a pull request (with a commit message like "add foo-bar"). We do not require new submissions to be tested against the rest of Stackage before the pull request (though it is a good idea to do so if you can with `stack --resolver nightly exec stackage-curator check` and `stack --resolver nightly build`), provided you meet the dependency version requirements above. If your library depends on a C library, add a note to your pull request with the Ubuntu library name, or even better edit the `debian-bootstrap.sh` script directly

If you want to make sure that the package builds against the newest versions of all dependencies you can do this:
```
$ cabal update
$ ghc --version # Should give v8.0.2
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

## Uploading a new package version

When a new version of a package in Stackage is uploaded to Hackage, we automatically try to include it in Stackage (unless the new version is considered experimental). That can result in a number of possible failures. If there is a failure we temporarily introduce an upper bound, and open a GitHub issue ticket to resolve the issue.

If the new version doesn't compile then the package author should upload a fixed version.

If a package's test suite is failing, the first job is to investigate why. If this is due to a bad interaction with versions of other packages in Stackage, then it is the responsibility of the maintainer to fix the test suite. In some situations, it is acceptable to not run the test suite.

## Following dependency upgrades

If a new version of a dependency is released, and that stops your package compiling/passing the tests, then it is your responsibility to modify your package. It is recommended that all package maintainers follow the dependencies of their packages on [Packdeps](http://packdeps.haskellers.com/), typically using the RSS feeds.

If a package is not modified in a timely manner, it may be temporarily
removed from Stackage by the curator team, at which point it is your
responsibility to add it back in via a new pull request. We typically
use fairly long windows on this, but at a minimum:

* If restrictive version bounds are the only problem, we will give at
  least a week to respond.
* If there are real breaking changes, the curator team will retain
  more discretion on how long a window to give before dropping
  packages. Historically, this has usually waited until the cutting of
  a new Long Term Support (LTS) major version.

**NOTE** Previously, this maintainer agreement put a time limit on
maintainers, requiring a certain level of responsiveness for
modifications to be made. We have explicitly removed that: anyone is
free to add a package to Stackage regardless of responsiveness
guarantees. However, as stated above, we may elect to temporarily
remove a package if it is not updated in a timely manner.

## Failing to meet the time limits

Maintainers are humans, humans get sick/have babies/go on holiday. If you have regular problems meeting the limits, find a co-maintainer. If you have a one-off problem, respond to the GitHub tickets saying so, and some kind soul might pick up the slack.

The soft time limits are intended to prevent people being inconvenienced because of problems in other packages. Where such inconvenience happens, we will drop the offending packages from Stackage. While upper bounds are sometimes a temporary solution, they are against the ethos of Stackage, so will not be kept for longer periods.

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
release (e.g., if `lts-8.9` is out, you would want your package to appear in
`lts-8.10`), please do the following in addition to the steps above:

* Open up a new issue on the [lts-haskell repo](https://github.com/fpco/lts-haskell/issues/new)
* Specify the LTS major version you would like your package to go into (e.g., lts-8)
* Provide a list of packages you would like added, and if relevant, any upper bounds on those packages
* Be patient! The LTS releases are less frequent than Nightly and by their nature more conservative, and therefore adding new packages is a more manual process. The Stackage curators will try to get to your issue as soon as possible, but it may take some time.
* To add a package to more than one LTS version please file separate tickets for each major LTS release, since they will typically be built and added at different times.
