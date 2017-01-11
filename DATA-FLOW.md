# The Stackage data flow

The Stackage project is really built on top of a number of different
subcomponents. This page covers how they fit together. The Stackage data flow
diagram gives a good bird's-eye view:

![Stackage data flow diagram](https://cloud.githubusercontent.com/assets/49415/14490986/cad5274e-017e-11e6-85cc-a4d815175c61.png)

## Inputs

There are three inputs into the data flow:

* [Hackage](http://hackage.haskell.org/) is the upstream repository of all
  available open source Haskell packages that are part of our ecosystem.
  Hackage provides both cabal file metadata (via the 00-index.tar file) and
  tarballs of the individual packages.

* [build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml)
  is the primary Stackage input file. This is where package maintainers can add
  packages to the Stackage package set. This also defines upper bounds, skipped
  tests, and a few other pieces of metadata.

* [stackage-content](https://github.com/fpco/stackage-content) is a Github
  repository containing static file content served from stackage.org

## Travis

For [various
reasons](https://www.fpcomplete.com/blog/2015/05/distributing-packages-without-sysadmin),
we leverage Travis CI for running some processes. In particular:

* [all-cabal-files](https://github.com/commercialhaskell/all-cabal-files/blob/hackage/.travis.yml)
  clones all cabal files from Hackage's 00-index.tar file into a Git repository
  without any modification

* [all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes/blob/hackage/.travis.yml)
  is mostly the same, but also includes cryptographic hashes of the package
  tarballs for more secure download (as leveraged by
  [Stack](http://haskellstack.com). It is powered by [all-cabal-hashes-tool](https://github.com/commercialhaskell/all-cabal-hashes-tool)

* [all-cabal-packages](https://github.com/commercialhaskell/all-cabal-packages)
  uses [hackage-mirror](http://github.com/fpco/hackage-mirror) to populate the
  hackage.fpcomplete.com mirror of Hackage, which provides S3-backed high
  availability hosting of all package tarballs

* [all-cabal-metadata](https://github.com/commercialhaskell/all-cabal-metadata)
  uses
  [all-cabal-metadata-tool](https://github.com/commercialhaskell/all-cabal-metadata-tool)
  to query extra metadata from Hackage about packages and put them into YAML
  files. As we'll see later, this avoids the need to make a lot of costly calls
  to Hackage APIs

Travis does not currently provide a means of running jobs on a regular basis.
Therefore, we have a simple cron job on the Stackage build server that triggers
each of the above builds every 30 minutes.

## stackage-curator

The heart of running Stackage builds is the
[stackage-curator](https://github.com/fpco/stackage-curator) tool. We run this
on a daily basis on the Stackage build server for Stackage Nightly, and on a
weekly basis for LTS Haskell. The build process is [highly
automated](https://github.com/fpco/stackage/blob/master/automated/build.sh) and
leverages Docker quite a bit.

stackage-curator needs to know about the most recent versions of all packages,
their tarball contents, and some metadata, all of which it gets from the
Travis-generated sources mentioned in the previous section. In addition, it
needs to know about build constraints, which can come from one of two places:

* When doing an LTS Haskell minor version bump (e.g., building lts-5.13), it
  grabs the previous version (e.g., lts-5.12) and converts the previous package
  set into constraints. For example, if lts-5.12 contains the package foo-5.6.7,
  this will be converted into the constraint `foo >= 5.6.7 && < 5.7`.
* When doing a Stackage Nightly build or LTS Haskell major version bump (e.g.,
  building lts-6.0), it grabs the latest version of the build-constraints.yaml
  file.

By combining these constraints with the current package data, stackage-curator
can generate a build plan and check it. (As an aside, this build plan
generation and checking also occurs every time you make a pull request to the
stackage repo.) If there are version bounds problems, one of the [Stackage
curators](https://github.com/fpco/stackage/blob/master/CURATORS.md) will open
up a Github issue and will add upper bounds, temporarily block a package, or
some other corrective action.

Once a valid build plan is found, stackage-curator will build all packages,
build docs, and run test suites. Assuming that all succeeds, it generates some
artifacts:

* Uploads the build plan as a YAML file to either
  [stackage-nightly](https://github.com/fpco/stackage-nightly) or
  [lts-haskell](https://github.com/fpco/lts-haskell)
* Uploads the generated Haddock docs and a package index (containing all used
  .cabal files) to haddock.stackage.org.

## stackage-server-cron

On the Stackage build server, we run the [stackage-server-cron
executable](https://github.com/fpco/stackage-server/blob/master/app/stackage-server-cron.hs)
regularly, which generates:

* A [SQLite
  database](https://github.com/fpco/stackage-server/blob/master/Stackage/Database.hs)
  containing information on snapshots, the packages they contain, Hackage
  metadata about packages, and a bit more. This database is uploaded to S3.
* A Hoogle database for each snapshot, which is also uploaded to S3

## stackage-server

The [software running stackage.org](https://github.com/fpco/stackage-server) is
a relatively simple Yesod web application. It pulls data from the
stackage-content repo, the SQLite database, the Hoogle databases, and the build
plans for Stackage Nightly and LTS Haskell. It doesn't generate anything
important of its own except for a user interface.

## Stack

[Stack](http://haskellstack.com) takes advantage of many of the pieces listed above as well:

* It by default uses the all-cabal-hashes repo for getting package metadata,
  and downloads package contents from the hackage.fpcomplete.com mirror (using
  the hashes in the repo for verification)
* There are some metadata files in stackage-content which contain information
  on, for example, where to download GHC tarballs from to make `stack setup`
  work
* Stack downloads the raw build plans for Stackage Nightly and LTS Haskell from
  the Github repo and uses them when deciding which packages to build for a
  given stack.yaml file
