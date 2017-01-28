This is a collection of instructions covering the processes that the Stackage curators - the
guys who maintain the Stackage project itself - should be doing on a regular basis.
Originally this was handled largely by Michael Snoyman,
but now we are a team of 4 people handling requests weekly in rotation.
Curation activities are mostly automated, and do not take up a significant amount of time.

## Workflow overview

This section sketches out at a high level how the entire Stackage build/curation
process works:

* [build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml) specifies packages to be included in Stackage
* [stackage-curator](http://www.stackage.org/package/stackage-curator) combines build-constraints.yaml with the current state of Hackage to create a build plan for a Stackage Nightly
* stackage-curator can check that build plan to ensure all version bounds are consistent
    * The [Travis job](https://github.com/fpco/stackage/blob/master/.travis.yml) performs these two steps to provide immediate feedback on pull requests
* Docker Hub [builds](https://github.com/fpco/stackage/blob/master/Dockerfile) a [Docker image](https://registry.hub.docker.com/u/snoyberg/stackage/) for running builds
* The stackage-build server (described below) is able to run automated builds using the [build.sh script](https://github.com/fpco/stackage/blob/master/automated/build.sh)
* When a new Nightly build is completed, it is uploaded to [the nightly repo](https://github.com/fpco/stackage-nightly)
* Once a week, we run an LTS minor bump. Instead of using build-constraints.yaml, that job takes the previous LTS release, turns it into constraints, and then bumps the version numbers to the latest on Hackage, in accordance with the version bounds in the build plan. This plans are uploaded to [the LTS repo](https://github.com/fpco/lts-haskell)
* Cutting a new LTS major release is essentially just a Stackage Nightly that gets rebuilt and uploaded as an LTS

## Pull requests

The typical story on pull requests is: If Travis accepts it and the
author only added packages under his/her own name, merge it.  If the
build later fails (see "Adding Debian packages for required system tools or libraries"),
then block the package until it's fixed.

If benchmarks, haddocks, or test suites fails at this point we
typically also block the package until these issues are fixed. This in
order to add packages with a clean slate.

Optionally we can check if packdeps says the package is up to date.
Visit http://packdeps.haskellers.com/feed?needle=<package-name>

Builds may fail because of unrelated bounds changes. If this happens,
first add any version bounds to get master into a passing state (see
"Fixing bounds issues"), then re-run the travis build.

A common issue is that authors submit newly uploaded packages, it can
take up to an hour before this has synced across the stack
infrastructure. You can usually compare the versions of the package in
https://github.com/commercialhaskell/all-cabal-metadata/tree/master/packages/
to what's on hackage to see if this is the case. Wait an hour and
re-run the pull request.

Tests also commonly fail due to missing test files, and sometimes due
to doctest limitations. You can point the maintainer to
https://github.com/bergmark/blog/blob/master/2016/package-faq.md

## Fixing bounds issues

The most common activity you'll deal with in Stackage curation is a version
bound issue, usually a restrictive upper bound. You fix this by opening an
issue on the Stackage repo about the problem, and modifying the
build-constraints.yaml file to work around it in one of the ways below. Be sure
to refer to the issue for workarounds added to that file.

### Temporary upper bounds

Most common technique, just prevent a new version of a library from
being included immediately. This also applies to when only benchmarks
and tests are affected.

* Copy the stackage-curator output and create a new issue, see e.g
https://github.com/fpco/stackage/issues/2108

* Add a new entry under the "stackage upper bounds" section of `build-constraints.yaml`. For the above example it would be

```yaml
    "Stackage upper bounds":
        # https://github.com/fpco/stackage/issues/2108
        - pipes < 4.3.0
```

* Commit (message e.g. "Upper bound for #2108")
* Optionally: Verify with `stackage-curator check` locally
* Push
* Verify that everything works on the build server (you can restart the build or wait for it to to run again)

Sometimes releases for different packages are tightly coupled. Then it
can make sense to combine them into one issue, as in
https://github.com/fpco/stackage/issues/2143.

If a dependency that is not explicitly in stackage is causing test or
benchmark failures you can skip or expect them to fail (see "Skipping
tests and benchmarks" and "Expecting test/benchmark/haddock
failures"). Bonus points for reporting this upstream to that packages'
maintainer.

### Lifting upper bounds

You can try this when you notice that a package has been updated. You
can also periodically try to lift bounds (I think it's good to do this
at the start of your week /@bergmark)

If not all packages have been updated check if any of them are missing
from the original issue and if so add a new comment mentioning them. A
new package may appear if its dependencies were part of this issue but
have been updated since the last time we checked. We want to give
these new packages ample time to be upgraded.

If stackage-curator is happy commit the change ("Remove upper bounds
and close #X"). After doing this the next nightly build may fail
because some packages didn't have an upper bound in place, but
compilation failed. In this case revert the previous commit so any
disabled packages are enabled again, re-open the issue, and add a new
comment with the failing packages. This is to give all maintainers
enough time to upgrade for this case as well.

### Amending upper bounds

With the `pipes` example above there was later a new release of
`pipes-safe` that required the **newer** version of `pipes`. You can
add that package to the same upper bounds section,
(e.g. https://github.com/fpco/stackage/commit/6429b1eb14db3f2a0779813ef2927085fa4ad673)
as we want to lift them simultaneously.

### Skipping tests and benchmarks

Sometimes tests and benchmark dependencies are forgotten or not cared
for. To disable compilation for them add them to `skipped-tests` or
`skipped-benchmarks`. If a package is added to these sections they
won't be compiled, and their dependencies won't be taken into account.

There are sub sections under these headers that is used to group types
of failures together, and also to document what type of failures
exist.

### Expecting test/benchmark/haddock failures

The difference from the `skipped` sections is that items listed here
are compiled and their dependencies are taken into account. These
sections also have sub sections with groups and descriptions.

One big category of test suites in this section are those requiring
running services. We don't want to run those, but we do want to check
dependencies and compile them.

If there are no version bounds that would fix the issue or if you
can't figure it out, file it
(e.g. https://github.com/fpco/stackage/issues/2133) to ask the
maintainer for help.

### Waiting for new releases

Sometimes there is a failure reported on a (now possibly closed) issue
on an external tracker. If an issue gets resolved but there is no
hackage release yet we'd like to get notified when it's uploaded.

Add the package with its current version to the
`tell-me-when-its-released` section. This will cause the build to stop
when the new version is out.

### Excluding packages

In an extreme case of a non-responsive maintainer, you can remove the
package entirely from Stackage. We try to avoid that whenever
possible.

This typically happens when we move to a new major GHC release or when
there are only a few packages waiting for updates on an upper bounds
issue.

Comment out the offending packages from the "packages" section and add
a comment saying why it was disabled:

```
        # - swagger # bounds: aeson 1.0
```

If this causes reverse dependencies to be disabled we should notify
the maintainers of those packages.


## Updating the content of the Docker image used for building

### Adding Debian packages for required system tools or libraries
Additional (non-Haskell) system libraries or tools should be added to `stackage/debian-bootstrap.sh`.
Committing the changes to a branch should trigger a DockerHub. Normally only the `nightly` branch needs to be updated
since new packages are not added to the current lts release.

Use [Ubuntu Package content search](http://packages.ubuntu.com/) to determine which package provides particular dev files (it defaults to xenial which is the version used to build Nightly).

Note that we generally don't install/run services needed for testsuites in the docker images - packages with tests requiring some system service can be added to `expected-test-failures`.
It's good to inform the maintainer of any disabled tests (commenting in the PR is sufficient).

If a new package fails to build because of missing system libraries we often ask the maintainer to help figure out what to install.

### Upgrading GHC version
The Dockerfile contains information on which GHC versions should be used. You
can modify it and push it to Github to trigger a DockerHub build. The nightly
branch is used for nightlies. For LTSes, we use the ltsX branch, where X is the
major version number (e.g., lts3 for lts-3.\*).

Note that when starting a new LTS major release, you'll need to modify Docker
Hub to create a new Docker tag for the relevant branch name.

### Getting the new image to the build server
Once a new Docker image is available, you'll need to pull it onto the stackage-build server (see
below). Instead of pulling an unbounded number of images, I typically just
delete all of the old images and let the new ones get downloaded:

```
docker rm $(docker ps -a -q)
docker rmi $(docker images -q)
```

but `docker pull snoyberg/stackage:nightly` can also be run instead just to update the nightly image say.

For a new GHC version you should also delete the cache directories on the stackage-build server to
force all packages to be rebuilt. See: [issue#746](https://github.com/fpco/stackage/issues/746). Eg:
```
rm -r nightly/work/builds/nightly/
```
This should also be done when moving the Nightly docker image to a new version of Ubuntu.

## stackage-build server

You'll need to get your SSH public key added to the machine. ~/.ssh/config info:

```
Host stackage-build
    User curators
    Hostname build.stackage.org
```

### Running the build script

We currently run the builds manually so make it easy to see when there are
bounds issues that need to be corrected. Automated this would be even better,
we're just not there yet.

```
# Run a nightly build
/opt/stackage-build/stackage/automated/run-nightly.sh

# Run an LTS minor bump
/opt/stackage-build/stackage/automated/build.sh lts-2.17

# Run an LTS major bump
/opt/stackage-build/stackage/automated/build.sh lts-3.0
```

Recommended: run these from inside a `tmux` session. If you get version bound
problems on nightly or LTS major, you need to fix build-constraints.yaml (see
info above). For an LTS minor bump, you'll typically want to use the
`CONSTRAINTS` environment variable, e.g.:

```
CONSTRAINTS='--constraint "conduit < 1.4.5" --constraint "criterion < 1.2.3"' /opt/stackage-build/stackage/automated/build.sh lts-2.17
```

Valid arguments to include in this environment variable:

* `--constraint` to modify an upper or lower bound
* `--add-package` to add a brand new package
* `--expect-test-failure` to expect tests to fail
* `--expect-haddock-failure` to expect haddocks to fail

If a build fails for bounds reasons, see all of the advice above. If the code
itself doesn't build, or tests fail, open up an issue and then either put in a
version bound to avoid that version or something else. It's difficult to give
universal advice on how to solve things, since each situation is unique. Let's
develop this advice over time. For now: if you're not sure, ask for guidance.

__`NOPLAN=1`__ If you wish to rerun a build without recalculating a
build plan, you can set the environment variable `NOPLAN=1`. This is
useful for such cases as an intermittent test failure, out of memory
condition, or manually tweaking the plan file. This is the default for
LTS builds.

### Timing

A looping script on the build server keeps trying to build nightly
with `sleep 30m` interleaved. It only publishes the nightly once per
day. This way new package versions or build failures can be caught
early and hopefully the nightlies will be timely.

LTS minor bumps typically are run on Sundays.

### Website sync debugging (and other out of disk space errors)

* You can detect the problem by running `df`. If you see that `/` is out of space, we have a problem
* (outdated) There are many temp files inside `/home/ubuntu/stackage-server-cron` that can be cleared out occasionally
* (outdated) You can then manually run `/home/ubuntu/stackage-server-cron.sh`, or wait for the cron job to do it

### Wiping the cache

Sometimes the cache can get corrupted which might manifest as `can't load .so/.DLL`.
You can wipe the nightly cache and rebuild everything by doing
`rm -rf /var/stackage/stackage/automated/nightly`.
Replace nightly with `lts7` to wipe the LTS 7 cache.

### Force a single package rebuild

You can force a single package to rebuild by deleting its "previous result"
file, e.g.:

```
$ rm /var/stackage/stackage/automated/nightly/work/builds/nightly/prevres/Build/cryptohash-0.11.9
```

## Local curator setup

We do not run the full stackage build locally as that might take too
much time. However, some steps on the other hand are much faster to do
yourself, e.g. verifying constraints without building anything.

To get started, install `stackage-curator` via Git, or [the Linux binary]:

```
$ git clone git@github.com:fpco/stackage-curator.git
$ cd stackage-curator && stack install
```

It is a good idea to upgrade `stackage-curator` at the start of your week.
Then, clone the stackage repo, get the latest packages and run dependency
resolution:

```
$ git clone git@github.com:fpco/stackage.git
$ stack update && stackage-curator check
```

This can be used to make sure all version bounds are in place, including for
test suites and benchmarks, to check whether bounds can be lifted, and to get
[tell-me-when-its-released] notifications.

`stackage-curator` does not build anything, so you wont see any compilation
errors for builds, tests and benchmarks.

[the Linux binary]: https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
[tell-me-when-its-released]: https://github.com/fpco/stackage/blob/master/CURATORS.md#waiting-for-new-releases

## Adding new curators

1. Add public ssh key to `~/.ssh/authorized_keys` on build server
2. Add to fpco/stackage project.

## Dealing with a new GHC release

As mentioned in the [GHC upgrade note], the major impact of a new GHC release
is on the packages that are causing upper bounds to be put in place. In order
to minimise out-of-date breakage and allow maintainers to have a solid chance
of getting their packages into the newest LTS, we try to do the following:

Make an early announcement (in the form of a blog post, typically) of the new
GHC release on the nightly build and the planned deadline for the new LTS release.
Make it clear, that in the time coming up to this, we hope package maintainers
will upgrade their packages to allow for the new GHC release.

We prefer to prune packages causing upper bounds constraints **after** the LTS
release to allow the maximum amount of packages to get into the newest LTS.

After the first LTS release, the package pruning process may begin in the
nightly build in order to move forward with getting the latest versions of
packages compatible with the new GHC release.

[GHC upgrade note]: https://github.com/fpco/stackage/blob/master/MAINTAINERS.md#upgrading-to-a-new-ghc-version
