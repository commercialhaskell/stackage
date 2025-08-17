stackage
========
[![check](https://github.com/commercialhaskell/stackage/actions/workflows/check.yml/badge.svg)](https://github.com/commercialhaskell/stackage/actions/workflows/check.yml)

Stable sets of Haskell Packages from Hackage

_This repository is for package authors and maintainers to get their packages into Stackage._

If you simply want to use Stackage as an end user, please follow the instructions in the  [stack documentation](https://docs.haskellstack.org/en/stable/) or via <https://www.stackage.org/>.

We highly recommend using the Haskell [stack](https://github.com/commercialhaskell/stack) tool for doing builds, which
includes built-in Stackage support.

Add your package
----------------
- To add your package in Stackage Nightly: edit the [build-constraints file](https://github.com/commercialhaskell/stackage/blob/master/build-constraints.yaml) and open a PR.
- To add you package to Stackage LTS: you need to open a pull request for changing [lts-haskell](https://github.com/commercialhaskell/lts-haskell/tree/master/build-constraints) build-constraints.

We welcome all packages, provided:

* The package author/maintainer agrees to the [maintainers agreement](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md).
* The package is buildable and testable from Hackage. We recommend setting up CI, which ensures a package is not accidentally incomplete, etc.
* The package is compatible with the newest versions of its dependencies.
* The package is compatible with the versions of libraries that ship with GHC ([more information on lenient lower bounds](https://tech.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Full details on how to add and test a package can be found in the [maintainers agreement](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md#adding-a-package).

__NOTE__: There is an approximate 30 minute delay between a package uploading
to Hackage and being available to the Github workflow action to check upper
bounds. If a pull request is marked as failed due to using an older version,
please close and reopen the PR to retrigger a CI build.

Other repos
-----------

The Stackage project consists of multiple repositories. This repository
contains the metadata on packages to be included in future builds and some
project information. In addition, we have the following repositories:

* [stackage-server](https://github.com/commercialhaskell/stackage-server)
* [curator](https://github.com/commercialhaskell/curator) ![Build Status](https://github.com/commercialhaskell/curator/workflows/Runtime%20image/badge.svg)
* [lts-haskell](https://github.com/commercialhaskell/lts-haskell)
* [stackage-snapshots](https://github.com/commercialhaskell/stackage-snapshots/)

Curious how it all fits together? See the [Stackage data
flow](https://github.com/commercialhaskell/stackage/blob/master/DATA-FLOW.md) (slightly outdated)

Build the package set
---------------------

Generally only the stackage build server run by the stackage curator
team and people interested in incorporating stackage snapshots into an
OS distribution need to build the entire package set. If you're
interested in trying this yourself, please check out
[the curator guide](https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md),
though be aware that this is not a recommended practice and there
likely will be problems you will need to debug yourself.

## Processing

The following describes at a high level the series of steps for processing

### Nightlies

1. Get list of core packages
2. Get build constraints from list of maintained packages
3. Load up package index
4. Calculate build plan using newest versions of packages
5. Write out a YAML file with complete build plan
6. Verify that the build plan can be compiled
7. Perform the build

### LTS

1. Load up most recent build plan
2. Convert build plan into constraints for next build
3. Continue from step (3) above

Frequently Asked Questions
--------------------------

__Why is Stackage LTS still on an older version of GHC?__

Typically it takes some months from a new major ghc release before
the Haskell ecosystem supports it fully enough that we can push it
to a new stable Stackage major version release. There can also be
ghc regressions that hold up a LTS major release.

The lag for minor ghc releases should be less
but it still requires extra work and there is usually some delay - this also
allows for some community testing before updating LTS.

__Why does Stackage have an older version of a package than Hackage?__

There are a number of answers to this question:

* Simplest reason: how old of a Stackage snapshot are you using? Once a
  snapshot is created, it's frozen for all time. So if you use
  nightly-2016-01-01, by the time you get to 2018, it will be pretty dated.
* If you're using an LTS snapshot: we lock down major versions when
  first creating an LTS run, so subsequent minor versions will not get
  new versions necessary. For example, if LTS 6.0 has `foo` version
  1.2.3, and the author immediately thereafter releases a version
  1.3.0 and never releases another 1.2.\* version, you'll never get
  another update in the LTS 6 line
* Sometimes we have upper bounds in place because other packages have
  problems with newer versions of dependencies. Open up the
  [build-constraints file](https://github.com/commercialhaskell/stackage/blob/master/build-constraints.yaml)
  and search for "Stackage upper bounds"
* Wired-in packages - those that ship with GHC and cannot be upgraded,
  and packages depending on them - are fixed to GHC versions. Common
  examples of this are containers and transformers. There's a lot more
  information on this in
  [an FP Complete blog post](https://tech.fpcomplete.com/blog/2014/05/lenient-lower-bounds)

__How long do you maintain an LTS build?__

We only guarantee that we will maintain a single LTS major version at
a time, and that it will be maintained for at least three months. This
is the
[originally proposed support window](https://tech.fpcomplete.com/blog/2014/12/backporting-bug-fixes),
and hasn't changed since then.

That said, we do maintain the capability to keep multiple LTS runs
operational in parallel, and with LTS 6 and 7 in fact did so. We
aren't changing our guarantees yet on longevity of a release, but are
trying to push out the bounds a bit farther.

__What time are Stackage snapshots published?__

Stackage Nightly and LTS are not released at a fixed time of day, they get
pushed to stackage.org (and the metadata to the stackage-snapshots github repo)
when their builds finish on the Stackage build server and
the latest built haddocks have been synced over. This time varies
greatly depending on build times for package updates, bounds breakage,
problems with new packages being added and other build issues, etc. There are
days when a release does not happen. LTS releases tend to happen over the
weekend or early in the week.

__Where to get help regarding uploading packages?__

Please ask on the #stackage channel on the
[Haskell Foundation Slack](https://join.slack.com/t/haskell-foundation/shared_invite/zt-mjh76fw0-CEjg2NbyVE8rVQDvR~0F4A)
or open an issue or comment on the PR which uploads the package.
