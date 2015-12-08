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
* The package is compatible with the newest versions of all dependencies.
* The package is compatible with the versions of libraries that ship with GHC ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

Full details on how to add a package can be found in the [maintainers agreement](https://github.com/fpco/stackage/blob/master/MAINTAINERS.md#adding-a-package).

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


Build the package set
---------------------

Generally, building the package set should be done only by the Jenkins machine
or by the official maintainers, as the process does require quite a bit of
setup on the local machine. That said, you'll likely be able to get a stable
build by running:

    cabal update
    cabal install stackage
    stackage nightly

### Docker

Note: This method has been disabled for now, but may be enabled again in the future.

If you'd like to check a build plan, or perform an entire build, without
specially configuring your system, Docker may be a good approach. To check if
some modifications to `build-constraints.yaml` are valid, try the following:

1. Create a local clone of the `stackage` repo
2. Make modifications to your local `build-constraints.yaml`
3. Inside the `stackage` working directory, run the following:

   ```
   $ docker run -it --rm -v $(pwd):/stackage -w /stackage snoyberg/stackage /bin/bash -c 'cabal update && stackage check'
   ```

Similarly, if you'd like to perform an entire build, you can replace the last step with:

```
$ docker run -it --rm -v $(pwd):/stackage -w /stackage snoyberg/stackage /bin/bash -c 'cabal update && stackage nightly --skip-upload'
```

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
