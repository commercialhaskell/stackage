This project is built around the concept of maintainers taking responsibility for making their packages work with the rest of the stable ecosystem, usually meaning the newest version of all dependencies. This is a social contract, and is not reflected in the codebase in any way.

The idea behind Stackage is that, if all packages work with the newest versions of dependencies, we avoid dependency hell. Specifically, we aim for:

* All packages are buildable and testable from Hackage. We recommend [the Stack Travis script](https://docs.haskellstack.org/en/stable/travis_ci/), which ensures a package is not accidentally incomplete.
* All packages are compatible with the newest versions of all dependencies (You can find restrictive upper bounds by visiting http://packdeps.haskellers.com/feed?needle=PACKAGENAME).
* All packages in a snapshot are compatible with the versions of libraries that ship with the GHC used in the snapshot ([more information on lenient lower bounds](https://tech.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Packages in Stackage are not patched: all package changes occur upstream in Hackage.

## Adding a package

Anyone can add any package to Stackage but you should talk to the upstream maintainer before putting another person's package under your own name.
It's generally better the actual package maintainer is also the Stackage maintainer, if that is not the case you should write the package maintainer a note first, eg by opening an upstream issue or sending them an email.

To add your package you can edit [`build-constraints.yaml`](https://github.com/commercialhaskell/stackage/blob/master/build-constraints.yaml) directly on github or fork the project. There's a section called `packages` where you would add yourself and your packages:

    "My Name <myemail@example.com> @mygithubuser":
        - package1
        - package2
        - package3

(If you are adding yourself for the first time, you can add yourself anywhere under the `packages:` section, it does not have to be at the end: this actually helps to avoid merge conflicts between new contributions.)

Any dependencies of your packages that are not already part of
stackage also need to be added explicitly (When this happens you will
see `not present` errors in the CI log). As mentioned above: If you
don't maintain this package yourself it is preferable that the actual
maintainer is also the stackage maintainer, but you are allowed to add
it under your own name.

If your package depends on a C library, please add it to `docker/02-apt-get-install.sh` or `docker/03-custom-install.sh`.

After doing that, commit with a message like "add foo-bar" and send a pull request.

The continuous integration job will do some checks to see if your package's dependencies are up-to-date.

The CI job notably doesn't compile packages, run tests, build documentation, or find missing C libraries.
If you want to be proactive or if CI fails, you can make sure that your package builds against the latest nightly.
See the [verify-package](https://github.com/commercialhaskell/stackage/blob/master/verify-package) script in this repository.

This approach works well, but has two limitations you should be aware
of:

* It won't notify you of restrictive upper bounds in your package if
  Stackage has the same upper bounds. For that reason, we recommend
  using [Packdeps](http://packdeps.haskellers.com/) (see "Following
  dependency upgrades" below). You can also run `cabal outdated`.
* If the latest Stackage Nightly is missing some of the latest
  packages, your build above may succeed whereas the Travis job may
  fail. Again: Packdeps will help you detect this situation.

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

## Moving or retiring maintainership of a Stackage package

If you no longer wish to be listed as maintainer of a package in Stackage,
you can open a pull request to move it to a new maintainer or
to either the "Grandfathered dependencies" or the "Abandoned packages" sections in `build-constraints.yaml`.
Unless there is a compelling technical reason to remove the package,
this is better than just dropping it from the distribution.

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
* We typically drop upper bounds and disable conflicting packages in Nightly after we create
  a new Long Term Support (LTS) major version.
* There are rare cases where an upper bound or build failure are hard
  to deal with so then we may disable

**NOTE** Previously we had stricter time limits, but we decided to
remove that: Anyone is free to add a package to Stackage regardless of
responsiveness guarantees. However, as stated above, we may elect to
temporarily remove a package if it is not updated in a timely manner.

## Understanding stackage issues

### Bounds issues

These are the most common.

Using https://github.com/commercialhaskell/stackage/issues/6217 as an example.

Our convention is to title a PR after the package that is being held
back from the nightly snapshot (here: aeson).

The issue body is templated and looks like this:

```
aeson-2.0.0.0 ([changelog](http://hackage.haskell.org/package/aeson-2.0.0.0/changelog)) (MAINTAINER) is out of bounds for:
- [ ] Agda-2.6.2 (>=1.1.2.0 && < 1.6). MAINTAINER. Used by: library
- [ ] HsYAML-aeson-0.2.0.0 (>=1.4.0.0 && < 1.6). MAINTAINER. Used by: library
- [ ] IPv6Addr-2.0.2 (>=0.8.0.2 && < 1.6). MAINTAINER. Used by: library
[...]
```

This (usually - see "Other issues") means that there was a new release of the package in the
header (aeson). This is expected and the aeson maintainer is
not expected to act. They are pinged to notify them that the
latest version of their package will not be part of the nightly
snapshots until the issue closed.

The list of packages with checkboxes denote the packages that prevent
us from using the latest version of aeson, the first version number is
the current version of that package in nightly (e.g. Agda 2.6.2). The
constraint in parenthesis `(>=1.1.2.0 && < 1.6)` is Agda's current
bound on `aeson`. To check this box we expect there to be an update
of Agda to support aeson 2.0.0.0.

Once all boxes are checked we should be able to close the issue and
upgrade aeson.

### Other issues

Other common types of issues are
* Packages with failing tests, haddocks, or benchmarks (note that we only *compile* benchmarks)
  + Maintainers may choose to update these parts of their package, or
    to exclude them from the stackage build (e.g. adding a package to
    `skipped-tests`)
* New releases of packages that depend on packages that are not a part
  of stackage. This is denoted as `not present`
  + Maintainers are encouraged to ask the maintainer of these packages
    to join stackage, or to be the stackage contact person themselves.
* A new release of a package with stricter upper bounds than its previous version
  + This will look like a normal bounds issue, and should be treated
    the same way by maintainers. Curators usually resolve this by
    adding an upper bound to that package instead of its dependency.

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
releases (e.g., 8.x.1) to allow further testing and to get the
benefits of the first bugfix release (e.g., 8.x.2).

## Adding a package to an LTS snapshot

The steps above affect the Stackage Nightly builds, but do not directly affect
LTS Haskell builds. When we build a new LTS Haskell major version (anything
ending in `.0`), the package set is taken from Stackage Nightly. Therefore, by
following the above steps, you can get your package into the next major LTS
Haskell release.

If you would like to get your package added to the current LTS Haskell
major release, please do the following in addition to the steps for Nightly described earlier:

* Check that your package can be built with the current LTS version (e.g. `stack build --test --bench --haddock --resolver lts`)
* Fork [lts-haskell repo](https://github.com/commercialhaskell/lts-haskell/) if you haven't already
* Open up a pull request on the [lts-haskell repo](https://github.com/commercialhaskell/lts-haskell/compare) for the appropriate `build-constraints/lts-*-build-constraints.yaml`
* Be patient! The LTS releases are less frequent than Nightly. The
  Stackage curators will try to get to your issue as soon as possible,
  but it may take some time.

## LTS package guarantees and exceptions

In general, we try to stick to some rules when it comes to the
packages included in LTS minor bumps. In particular:

* If a package exists in LTS-X.Y, it should also exist in LTS-X.(Y+1)
* We should not include a major version bump of a package between
  LTS-X.Y and LTS-X.(Y+1)

However, there are some cases where exceptions may be made, based
purely on Stackage Curator discretion. The most common examples are:

*   If a package does not follow the PVP in its version number policy,
    applying the standard version bump rules would not necessarily
    makes sense. As an example, suppose package `foo` decides to
    follow SemVer instead of the PVP. By our standard rules of version
    bumps, a change from `foo-1.2.0` to `foo-1.3.0` would be
    considered a major version bump, and disallowed in an LTS minor
    version bump. However, if a package is following SemVer, this
    would not be a breaking change, and curators may elect to include
    it. Note though that curators and their tooling will not know your
    package is following SemVer, so in this case you would have to open
    an issue on the [lts-haskell repo](https://github.com/commercialhaskell/lts-haskell/issues/new).

*   If a package has overly restrictive version bounds on a
    dependency, in particular constraining a minor version
    unnecessarily, we may drop that package instead of artificially
    holding back the dependency. As an example: suppose `LTS-20.1`
    includes `foo-1.2.0` and `bar-1`. `bar-1` has a dependency `foo >=
    1.2.0 && < 1.2.1`, which is overly constrained on the minor
    version number according to the PVP. Then `foo-1.2.1` is
    released. The Stackage Curator team would have two choices:

    * Reject `foo-1.2.1` from `LTS-20.2`, since that is what `bar-1`
      requires.
    * Drop `bar-1` from `LTS-20.2`, and allow `foo-1.2.1`.

    Decisions will need to be taken on a case-by-case basis, and may
    depend on such issues as whether an important bugfix or security
    update in included. The curator team may also try to notify the
    author of `bar` to try and get a patched version released.
