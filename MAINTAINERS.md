This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

The idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. Specifically, we aim for:

* All packages are buildable and testable from Hackage. We recommend [the Stack Travis script](http://docs.haskellstack.org/en/stable/GUIDE.html#travis-with-caching), which ensures a package is not accidentally incomplete.
* All packages are compatible with the newest versions of all dependencies.
* All packages are compatible with the versions of libraries that ship with GHC ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

## Adding a package

Anyone can add a package to Stackage, but it's highly encouraged that the actual package maintainer is also the Stackage maintainer. If that is not the case, you should drop the package maintainer a note first.

To add your package, first fork this repository.
In the [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) file, there's a section called `packages`.
To add a set of packages, you would add:

    "My Name myemail@example.com @mygithubuser":
        - package1
        - package2
        - package3

After doing that, send a pull request. We do not require new submissions to be tested against the rest of Stackage before the pull request, provided you meet the dependency version requirements above. If your library depends on a C library, add a note to your pull request with the Ubuntu library name, or even better edit the debian-bootstrap script directly


Please use commit messages like "add foo-bar" or "add johndev's packages"
(`build-constraints.yaml` is the most frequently changed file in this git repo
so commit messages like "update build-constraints.yaml" are not helpful).


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
