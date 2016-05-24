This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

The idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. Specifically, we aim for:

* All packages are buildable and testable from Hackage. We recommend [the Stack Travis script](http://docs.haskellstack.org/en/stable/GUIDE.html#travis-with-caching), which ensures a package is not accidentally incomplete.
* All packages are compatible with the newest versions of all dependencies.
* All packages in a snapshot are compatible with the versions of libraries that ship with the GHC used in the snapshot ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

## Adding a package

Anyone can add a package to Stackage, but it's highly encouraged that the actual package maintainer is also the Stackage maintainer. If that is not the case, you should drop the package maintainer a note first.

To add your package, first fork this repository.
In the [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) file, there's a section called `packages`.
To add a set of packages, you would add:

    "My Name myemail@example.com @mygithubuser":
        - package1
        - package2
        - package3

After doing that, send a pull request (with a commit message like "add foo-bar"). We do not require new submissions to be tested against the rest of Stackage before the pull request (though it is a good idea to do so if you can with `stack --resolver nightly exec stackage-curator check` and `stack --resolver nightly build`), provided you meet the dependency version requirements above. If your library depends on a C library, add a note to your pull request with the Ubuntu library name, or even better edit the `debian-bootstrap.sh` script directly

**NB** Please use commit messages like "add foo-bar" or "add johndev's packages"
(`build-constraints.yaml` is the most frequently changed file in this git repo
so commit messages like "update build-constraints.yaml" are not helpful).

**NB2** There can be a delay of up to an hour before package versions newly
uploaded to Hackage appear to our build server. If you just uploaded a package
to Hackage that you're trying to get included, we recommend waiting an hour
before opening the PR.


## Uploading a new package

When a new version of a package is uploaded to Hackage, we automatically try to include it in Stackage (unless the new version is considered experimental). That can result in a number of possible failures. If there is a failure we temporarily introduce an upper bound, and raise GitHub issue tickets to resolve the issue.

If the new version doesn't compile then the package author should quickly (within 1 week) upload a fixed version.

If a package's test suite is failing, the first job is to investigate why. If this is due to a bad interaction with versions of other packages in Stackage, then it is the responsibility of the maintainer to fix the test suite. In some situations, it is acceptable to not run the test suite.


## Following dependency upgrades

If a new version of a dependency is released, and that stops your package compiling/passing the tests, then it is your responsibility to modify your package. It is highly recommended that all package maintainers follow the dependencies of their packages on [Packdeps](http://packdeps.haskellers.com/), typically using the RSS feeds.

**If restrictive version bounds are the only problem** then you must quickly (within 1 week) upload a new version with relaxed version bounds. Note that unlike the PVP, Stackage does not require upper bounds.

**If the new dependency causes breaking changes** then all package authors should quickly assess the likely impact on their package (within 1 week) and then produce a new compatible version. The expected timeline for new versions varies between 1 week and 1 month, depending on the significance of the change, and thus the work required to produce those new versions.


## Failing to meet the time limits

Maintainers are humans, humans get sick/have babies/go on holiday. If you have regular problems meeting the limits, find a co-maintainer. If you have a one-off problem, respond to the GitHub tickets saying so, and some kind soul might pick up the slack.

The time limits are intended to stop people being inconvenienced because of problems in other packages. Where such inconvenience happens, we will drop the offending packages from Stackage. While upper bounds are sometimes a temporary solution, they are against the ethos of Stackage, so will not be kept for long.

## Upgrading to a new GHC version

The Stackage curation team tries to move Stackage Nightly to new versions of GHC quickly as they become available, while keeping LTS Haskell on a regular release schedule. For package maintainers, the most important impacts of a new GHC release are:

* We will typically do a sweep through the Stackage upper bounds and aggressively remove packages that block them. This is because, in most cases, we will need to move to the newest versions of a package to get support for the latest GHC, and asking package maintainers to backport their fixes is an undo burden
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
