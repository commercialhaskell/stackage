stackage
========

[![Build Status](https://travis-ci.org/fpco/stackage.svg?branch=master)](https://travis-ci.org/fpco/stackage)

"Stable Hackage": creating a vetted set of packages from Hackage.
This repository is for package authors and maintainers to get their packages into Stackage.
If you simply want to use Stackage as an end user, please follow the instructions on [https://www.stackage.org/](https://www.stackage.org).

We strongly recommend using the Haskell tool `stack` for doing builds, which
has Stackage support built-in: [stack](https://github.com/commercialhaskell/stack) [![Build Status](https://travis-ci.org/commercialhaskell/stack.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack).


Add your package
----------------

We welcome all packages, given that

* they are compatible with current snapshots (roughly speaking: work with the latest released versions of GHC and of all the package's dependencies);
* there is a maintainer willing to promise to keep it that way.

See the [maintainers agreement](MAINTAINERS.md) for more details.

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
flow](DATA-FLOW.md).


Build the package set
---------------------

Generally only the stackage build server run by the stackage curator
team and people intrested in incorporating stackage snapshots into an
OS distribution need to build the entire package set. If you're
interested in trying this yourself, please check out
[the curator guide](CURATORS.md),
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
