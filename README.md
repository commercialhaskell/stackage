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

## Code explanation

We start off with *constraints*. Constraints state things like "package X has a
given version range," who the maintainer is for a package, the description of
the system/compiler being used, etc. `BuildConstraints` describes the build as
a whole, whereas `PackageConstraints` describes the constraints on an
individual package.

There are two primary ways of getting a `BuildConstraints`.
`defaultBuildConstraints` inspects the first GHC in the PATH environment variable to
determine GHC version, core packages, core tools, etc. It then uses the
`Stackage.Config` module to extract information on additional packages to be
installed. The secondary approach is in `Stackage2.UpdateBuildPlan`, which will be
discussed later.

`BuildConstraints` does not specify a build completely. That is given by a
`BuildPlan`, which is similarly broken down into `BuildPlan` and `PackagePlan`.
In order to get a `BuildPlan`, we need two pieces of information: the
`BuildConstraints`, and a package index. The package index (usually downloaded
from Hackage) is a collection of all of the cabal files available.

By applying a `BuildConstraints` to a package index (via `newBuildPlan`), we
get a proposed `BuildPlan`. There is no guarantee that this `BuildPlan` is
valid. To validate it, we use `checkBuildPlan`. A `BuildPlan` is an instance of
both `ToJSON` and `FromJSON`, and therefore can be serialized to a file for
later use.

When dealing with LTS Haskell, we want to be able to take a `BuildPlan`, and
update to a newer `BuildPlan` that keeps all packages at the same major
version.  `updateBuildConstraints` turns a `BuildPlan` into a new
`BuildConstraints` with that restriction, and `updateBuildPlan` applies
`newBuildPlan` to that result. As mentioned previously: this is *not* a
validated result, and therefore `checkBuildPlan` must be used.

A `BuildPlan` can be acted on. This is done to check that all packages compile
together, run relevant test suites, test Haddock documentation is correct, and
produce as artifacts both a self-contained GHC binary package database and a
set of Haddock documentation. (Not yet implemented.)

A `BuildPlan` may be converted into a bundle to be uploaded to Stackage Server.
(Not yet implemented.)
