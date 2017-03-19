stackage
========

[![Build Status](https://travis-ci.org/fpco/stackage.svg?branch=master)](https://travis-ci.org/fpco/stackage)

"Stable Hackage": creating a vetted set of packages from Hackage.
This repository is for package authors and maintainers to get their packages into Stackage.
If you simply want to use Stackage as an end user, please follow the instructions on [https://www.stackage.org/](https://www.stackage.org).

We strongly recommend using the Haskell tool stack for doing builds, which
includes built-in Stackage support: [stack](https://github.com/commercialhaskell/stack) [![Build Status](https://travis-ci.org/commercialhaskell/stack.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack).


Add your package
----------------

We welcome all packages, provided:

* The package author/maintainer agrees to the [maintainers agreement](https://github.com/fpco/stackage/blob/master/MAINTAINERS.md).
* The package is buildable and testable from Hackage. We recommend [the Stack Travis script](http://docs.haskellstack.org/en/stable/GUIDE.html#travis-with-caching), which ensures a package is not accidentally incomplete.
* The package is compatible with the newest versions of all dependencies (You can find restrictive upper bounds by visiting http://packdeps.haskellers.com/feed?needle=PACKAGENAME).
* The package is compatible with the versions of libraries that ship with GHC ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Full details on how to add and test a package can be found in the [maintainers agreement](https://github.com/fpco/stackage/blob/master/MAINTAINERS.md#adding-a-package).

__NOTE__: There is an approximate 30 minute delay between a package uploading
to Hackage and being available to the Travis build script to check upper
bounds. If a pull request is marked as failed due to using an older version,
please close and reopen the PR to retrigger a Travis build.

Other repos
-----------

The Stackage project consists of multiple repositories. This repository
contains the metadata on packages to be included in future builds and some
project information. In addition, we have the following repositories:

* [stackage-server](https://github.com/fpco/stackage-server) [![Build Status](https://travis-ci.org/fpco/stackage-server.svg?branch=master)](https://travis-ci.org/fpco/stackage-server)
* [stackage-curator](https://github.com/fpco/stackage-curator) [![Build Status](https://travis-ci.org/fpco/stackage-curator.svg?branch=master)](https://travis-ci.org/fpco/stackage-curator)
* [stackage-types](https://github.com/fpco/stackage-types) [![Build Status](https://travis-ci.org/fpco/stackage-types.svg?branch=master)](https://travis-ci.org/fpco/stackage-types)
* [lts-haskell](https://github.com/fpco/lts-haskell)
* [stackage-nightly](https://github.com/fpco/stackage-nightly)

We also support some add-on tools to cabal-install to make its usage with
Stackage both easier and more secure:

* [stackage-cli](https://github.com/fpco/stackage-cli) [![Build Status](https://travis-ci.org/fpco/stackage-cli.svg?branch=master)](https://travis-ci.org/fpco/stackage-cli)
* [stackage-update](https://github.com/fpco/stackage-update) [![Build Status](https://travis-ci.org/fpco/stackage-update.svg?branch=master)](https://travis-ci.org/fpco/stackage-update)
* [stackage-upload](https://github.com/fpco/stackage-upload) [![Build Status](https://travis-ci.org/fpco/stackage-upload.svg?branch=master)](https://travis-ci.org/fpco/stackage-upload)
* [stackage-install](https://github.com/fpco/stackage-install) [![Build Status](https://travis-ci.org/fpco/stackage-install.svg?branch=master)](https://travis-ci.org/fpco/stackage-install)
* [stackage-build-plan](https://github.com/fpco/stackage-build-plan) [![Build Status](https://travis-ci.org/fpco/stackage-build-plan.svg?branch=master)](https://travis-ci.org/fpco/stackage-build-plan)

Curious how it all fits together? See the [Stackage data
flow](https://github.com/fpco/stackage/blob/master/DATA-FLOW.md).


Build the package set
---------------------

Generally only the stackage build server run by the stackage curator
team and people intrested in incorporating stackage snapshots into an
OS distribution need to build the entire package set. If you're
interested in trying this yourself, please check out
[the curator guide](https://github.com/fpco/stackage/blob/master/CURATORS.md),
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
  [build-constraints file](https://github.com/fpco/stackage/blob/master/build-constraints.yaml)
  and search for "Stackage upper bounds"
* Wired-in packages - those that ship with GHC and cannot be upgraded,
  and packages depending on them - are fixed to GHC versions. Common
  examples of this are containers and transformers. There's a lot more
  information on this in
  [an FP Complete blog post](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)

__How long do you maintain an LTS build?__

We only guarantee that we will maintain a single LTS major version at
a time, and that it will be maintained for at least three months. This
is the
[originally proposed support window](https://www.fpcomplete.com/blog/2014/12/backporting-bug-fixes),
and hasn't changed since then.

That said, we do maintain the capability to keep multiple LTS runs
operational in parallel, and with LTS 6 and 7 in fact did so. We
aren't changing our guarantees yet on longevity of a release, but are
trying to push out the bounds a bit farther.
