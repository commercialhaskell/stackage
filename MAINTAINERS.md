This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

This Wiki page is meant to foster discussion of what such a social contract would look like, and ultimately come up with a solution that we can mostly agree upon.

## Adding a package to Stackage

Getting your package included in Stackage is simple:

* Create a fork on Github.
* Add your packages to [build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml), following the format of other contributors.
* Commit and send a pull request.

Previously, we recommended trying to build the Stackage set yourself before sending a pull request. That's no longer necessary. Just send a pull request, and if there's a problem, the maintainer (currently: Michael Snoyman) will let you know.

Unlike the Haskell Platform, Stackage does not place quality requirements on packages. In theory, any package that compiles will be accepted to the Stackage package set. However, the maintainer must agree to follow the social contract spelled out in this document.

Note: while it's highly encouraged that the actual package maintainer is also the Stackage maintainer, this is not necessary. You do not even need the package maintainer's approval to submit to Stackage, though it's a good idea to drop him/her a note.

## Upper bounds

The main idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. As a result, upper bounds are considered a detriment. This plays out in two ways:

1.  By default, when a package is added to Stackage, it has no upper bound attached. Newer versions will be automatically included, assuming the newer versions compile and pass all tests. The only exceptions to this rule should be:

    1. A new package is still considered experimental, and therefore not suitable for inclusion in a stable set of packages.
    2. A package is a dependency of a large number of other packages and introduces a major breaking change. In this case, we'll temporarily impose an upper bound to give maintainers an upgrade grace period (see below).

2.  There's long been a debate about whether the PVP's requirement of putting upper bounds in a package is a good thing. We won't resolve that debate here, but Stackage *does* change the situation a bit. Since a package included in Stackage will be constantly compiled against newer versions of dependencies, breakages will be found quickly. Therefore, the motivation for upper bounds is lessened.
    * Michael Snoyman: Personally, I lean towards dropping upper bounds most of the time.

If you decide to put upper bounds on your packages in general in strict accordance with the PVP, your package may often times cause a Stackage build failure. In many cases, this can be resolved with a simple dependency version bump in your cabal file.

* Michael Snoyman: For these kinds of breakages, given the frequency with which they occur and the simplicity of solving them, I believe package maintainers should have 4 days to upload a new version to Hackage.

It is highly recommended for all package maintainers to follow the dependencies of their packages on [Packdeps](http://packdeps.haskellers.com/). The site provides RSS feeds to simplify this process.

## Broken packages/broken tests

It's bound to happen that broken packages will be uploaded to Hackage. In my experience, the most common situation is that a new version works for the developer on his/her OS/GHC version combination, but fails on other systems. Stackage is a great way to discover such a breakage.

When Stackage reports such a breakage, I see the following possible resolutions to the problem:

1. Author uploads a fix.
2. We put a version constraint in Stackage to use the previous known good version.
3. Stackage itself applies a patch (dependent on [issue #5](https://github.com/fpco/stackage/issues/5)).
4. The package is dropped.

* Michael Snoyman: My recommendation would be that we give the maintainer 24 hours to respond to the failure report. If there's no response in 24 hours, we put in a temporary version constraint. If this version constraint begins to cause problems for other packages in Stackage, and the maintainer is still non-responsive, we drop the package.

If a package's test suite is failing, the first job is to investigate why. If this is due to a bad interaction with versions of other packages in Stackage, then it is the responsibility of the maintainer to fix the test suite. In some situations, it is acceptable to simply not run the test suite.

One example is that, at the time of writing, the HTTP package test suite is not run. This is because the version of HTTP included with the Haskell Platform requires an older version of Warp. Since we cannot upgrade HTTP without breaking Haskell Platform compliance, we are required to simply omit the test suite.

## Major breaking changes

Many times, when a new major version of a library is released, the resulting breakage is trivial to address. Some examples of this were the transformers 0.2 to 0.3 migration, or text 0.10 to 0.11.

* Michael Snoyman: In such cases, I believe maintainers should be given a one week grace period to update their packages, during which time the dependency is given an upper bound.

Other times, breakage is much more significant. Obviously, the distinction between these two cases in entirely subjective, and must be left in the hands of the Stackage organizers.

* Michael Snoyman: In the major breaking changes case, I believe a period of one month should be given for upgrades.

## Automation

Ultimately, I hope we have automated build machines and a centralized server to grab build reports. Until then, Stackage will work much more informally.
