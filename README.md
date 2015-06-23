stackage
========

[![Build Status](https://travis-ci.org/fpco/stackage.svg?branch=master)](https://travis-ci.org/fpco/stackage)

"Stable Hackage," tools for creating a vetted set of packages from Hackage.

__NOTE__ This repository is for package authors to get their code into
Stackage. If you simply want to use Stackage as an end user, please follow the
instructions on [http://www.stackage.org/](http://www.stackage.org).

The Stackage project consists of multiple repositories. This repository
contains the metadata on packages to be included in future builds and some
project information. In addition, we have the following repositories:

* [stackage-server](https://github.com/fpco/stackage-server) [![Build Status](https://travis-ci.org/fpco/stackage-server.svg?branch=master)](https://travis-ci.org/fpco/stackage-server)
* [stackage-curator](https://github.com/fpco/stackage-curator) [![Build Status](https://travis-ci.org/fpco/stackage-curator.svg?branch=master)](https://travis-ci.org/fpco/stackage-curator)
* [stackage-types](https://github.com/fpco/stackage-types) [![Build Status](https://travis-ci.org/fpco/stackage-types.svg?branch=master)](https://travis-ci.org/fpco/stackage-types)
* [lts-haskell](https://github.com/fpco/lts-haskell)
* [stackage-nightly](https://github.com/fpco/stackage-nightly)

We strongly recommend using the Haskell tool stack for doing builds, which
includes built-in Stackage support:

* [stack](https://github.com/commercialhaskell/stack) [![Build Status](https://travis-ci.org/commercialhaskell/stack.svg?branch=master)](https://travis-ci.org/commercialhaskell/stack)

We also support some add-on tools to cabal-install to make its usage with
Stackage both easier and more secure:

* [stackage-cli](https://github.com/fpco/stackage-cli) [![Build Status](https://travis-ci.org/fpco/stackage-cli.svg?branch=master)](https://travis-ci.org/fpco/stackage-cli)
* [stackage-update](https://github.com/fpco/stackage-update) [![Build Status](https://travis-ci.org/fpco/stackage-update.svg?branch=master)](https://travis-ci.org/fpco/stackage-update)
* [stackage-upload](https://github.com/fpco/stackage-upload) [![Build Status](https://travis-ci.org/fpco/stackage-upload.svg?branch=master)](https://travis-ci.org/fpco/stackage-upload)
* [stackage-install](https://github.com/fpco/stackage-install) [![Build Status](https://travis-ci.org/fpco/stackage-install.svg?branch=master)](https://travis-ci.org/fpco/stackage-install)
* [stackage-build-plan](https://github.com/fpco/stackage-build-plan) [![Build Status](https://travis-ci.org/fpco/stackage-build-plan.svg?branch=master)](https://travis-ci.org/fpco/stackage-build-plan)

Get your package included
-------------------------

In order to get your package included in the set of stable packages, you should
send a pull request against this repository. In the [`build-constraints.yaml`](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) file,
there's a section called `packages`. In general, to add a set of
packages, you would add:

    "My Name myemail@example.com @mygithubuser":
        - package1
        - package2
        - package3

You can follow the examples of the other sets of packages in that function.
Once you've done this, you can send a pull request to get your package
included.

__NOTE__: In order to ease the process of adding new packages, we no longer
require new submissions to be tested on your own system before sending a pull
request. If you believe your package works with the newest versions of all
dependencies, you may send a pull request without testing first.

You should also read the [maintainers
agreement](https://github.com/fpco/stackage/wiki/Maintainers-Agreement).

Package Author Guidelines
-------------------------

There are some basic rules to get your package to play nice with Stackage. Here
are some quick guidelines to hopefully make this easier:

* Make sure that your code is buildability and testable from Hackage. Often
  times, authors test their builds locally, but the tarball that gets uploaded
  to Hackage is missing some necessary files. The best way to do this is to
  set up a Travis job to do it for you. We recommend the
  [multi-ghc-travis](https://github.com/hvr/multi-ghc-travis) approach.
* Make your code compatible with the newest versions of all dependencies.
* Make your code compatible with the versions of libraries that ship with GHC ([more information on lenient lower bounds](https://www.fpcomplete.com/blog/2014/05/lenient-lower-bounds)).

There are certainly many other tips that could be added here. If you think of
any, please send a pull request!

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
