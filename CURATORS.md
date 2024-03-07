This is a collection of instructions covering the processes that the Stackage curators - the
team who maintain the Stackage project itself - should be doing on a regular basis.
Curation activities are mostly automated, and do not take up a significant amount of time.
The following is the current list of curators, in alphabetical order:

* Adam Bergmark (@bergmark)
* Alexey Zabelin (@alexeyzab)
* Andreas Ländle (@alaendle)
* Chris Dornan (@cdornan)
* Dan Burton (@danburton)
* Jens Petersen (@juhp)
* Joe Kachmar (@jkachmar)
* Mihai Maruseac (@mihaimaruseac)

## Workflow overview

This section sketches out at a high level how the entire Stackage build/curation
process works:

* [build-constraints.yaml](https://github.com/commercialhaskell/stackage/blob/master/build-constraints.yaml) specifies packages to be included in Stackage
* [curator](https://github.com/commercialhaskell/curator) combines build-constraints.yaml with the current state of Hackage to create a build plan for a Stackage Nightly
* `curator` can check that build plan to ensure all version bounds are consistent
    * The [Travis job](https://github.com/commercialhaskell/stackage/blob/master/.travis.yml) performs these two steps to provide immediate feedback on pull requests
* Docker builds [builds](https://github.com/commercialhaskell/stackage/actions/workflows/image.yml)
* The stackage-build server (described below) is able to run automated builds using the [build.sh script](https://github.com/commercialhaskell/stackage/blob/master/automated/build.sh)
* When a new (nightly or LTS) build is completed, it is uploaded to [stackage-snapshots](https://github.com/commercialhaskell/stackage-snapshots)
* Once a week, we run an LTS minor bump. Instead of using build-constraints.yaml, that job takes the previous LTS release, turns it into `^>=` constraints, and then bumps the version numbers to the latest on Hackage, in accordance with the generated constraint.
* Cutting a new LTS major release is essentially just a Stackage Nightly that gets rebuilt and uploaded as an LTS

## Pull requests

The typical story on pull requests is: If Travis accepts it and the
author only added packages under his/her own name, merge it.  If the
build later fails (see [Adding Debian packages]), then block the
package until it's fixed.

[Adding Debian packages]: https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md#adding-debian-packages-for-required-system-tools-or-libraries

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

* Copy the `curator` output and create a new issue, see e.g
https://github.com/commercialhaskell/stackage/issues/2108

* Add a new entry under the "stackage upper bounds" section of `build-constraints.yaml`. For the above example it would be

```yaml
    "Stackage upper bounds":
        # https://github.com/commercialhaskell/stackage/issues/2108
        - pipes < 4.3.0
```

* Commit (message e.g. "Upper bound for #2108")
* Optionally: Verify with `./check` locally
* Push
* Verify that everything works on the build server (you can restart the build or wait for it to to run again)

Sometimes releases for different packages are tightly coupled. Then it
can make sense to combine them into one issue, as in
https://github.com/commercialhaskell/stackage/issues/2143.

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

If `curator` is happy commit the change ("Remove upper bounds
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
(e.g. https://github.com/commercialhaskell/stackage/commit/6429b1eb14db3f2a0779813ef2927085fa4ad673)
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
are compiled and their dependencies are taken into account
(but they are allowed to fail to build).
These sections also have subsections with groups and descriptions.

One big category of test suites in this section are those requiring
running services. We don't want to run those, but we do want to check
dependencies and compile them.

If there are no version bounds that would fix the issue or if you
can't figure it out, file it
(e.g. https://github.com/commercialhaskell/stackage/issues/2133) to ask the
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

If a package needs to be disabled due to build failures: Add a `< 0`
bound to the package to exclude it, and add a comment stating why it
was disabled: `- swagger < 0 # compile failure againts aeson 1.0`

If a package needs to be disabled due to bounds issues, see the "Large
scale enabling/disabling of packages" section below.

If this causes reverse dependencies to be disabled we should notify
the maintainers of those packages.


## Updating the content of the Docker image used for building

### Adding Debian packages for required system tools or libraries
Additional (non-Haskell) system libraries or tools should be added to `docker/02-apt-get-install.sh` or `docker/03-custom-install.sh`.
After you've committed those changes, merging them into the `nightly` branch should
trigger an image build. Simply run:

```bash
    $ git checkout nightly
    $ git merge master
    $ git push
```

This will [trigger a build](https://github.com/commercialhaskell/stackage/actions/workflows/image.yml).

Use [Ubuntu Package content search](http://packages.ubuntu.com/) to determine which package provides particular dev files (it defaults to xenial which is the version used to build Nightly).

Note that we generally don't install/run services needed for testsuites in the docker images - packages with tests requiring some system service can be added to `expected-test-failures`.
It's good to inform the maintainer of any disabled tests (commenting in the PR is sufficient).

If a new package fails to build because of missing system libraries we often ask the maintainer to help figure out what to install.


### Upgrading GHC version
The nightly branch is used for nightlies. For LTSes, we use the ltsX branch,
where X is the major version number (e.g., lts20 for lts-20.\*).)

Note that when starting a new LTS major release, you'll need to modify `.github/workflows/image.yml` to add a new lts branch.

Ensure that the [global-hints.yaml
file](https://github.com/commercialhaskell/stackage-content/blob/master/stack/global-hints.yaml)
is updated with information on the latest GHC release by cloning that
repo and running `./update-global-hints.hs ghc-X.Y.Z`.

If enountering an error like the following, this means that the [Stack metadata](https://github.com/commercialhaskell/stackage-content)
has not yet been updated, so wait some time until this happens:

```
This probably means a GHC bindist has not yet been added for OS key 'linux64', 'linux64-ncurses6', 'linux64-tinfo6'.
Supported versions: ...
update-global-hints.hs: Received ExitFailure 1 when running
```

Also required to build an LTS minor bump with a ghc version change: modify <https://github.com/commercialhaskell/lts-haskell/tree/master/build-constraints> and update the ghc-version.  Then run `automated/build.sh lts-$THIS_LTS_MINOR_BUMP` to build the LTS.

### Getting the new image to the build server
Once a new Docker image is available, you'll need to pull it onto the stackage-build server (see
below). Instead of pulling an unbounded number of images, I typically just
delete all of the old images and let the new ones get downloaded:

```
docker rm $(docker ps -a -q)
docker rmi $(docker images -q)
```

but `docker pull ghcr.io/commercialhaskell/stackage/build:nightly` can also be run instead just to update the nightly image say.

For a new GHC version you should also delete the ~~cache~~ .stack-work snapshot install directories on the stackage-build server to
~~force all packages to be rebuilt~~ clear up some space. See: [issue#746](https://github.com/commercialhaskell/stackage/issues/746). Eg:

```
# for example
SNAP_SERIES=nightly # or lts16
OLD_GHCVER=8.10.1
rm -r work/$SNAP_SERIES/unpack-dir/.stack-work/install/x86_64-linux-tinfo6/*/$OLD_GHCVER/
```
This should also be done when moving the Nightly docker image to a new version of Ubuntu.

If you're impatient and would like to build the Docker image on the
build server instead of waiting for Docker Hub, you can run the
following command (replacing `BRANCH=nightly` if the image for a different branch is desired):

```
BRANCH=nightly
DIR=$(mktemp -d)
(cd $DIR \
  && git clone https://github.com/commercialhaskell/stackage \
  && cd stackage \
  && git checkout $BRANCH \
  && docker build --tag commercialhaskell/stackage:$BRANCH .)
rm -rf $DIR
```

Note that we do a clean clone of the `stackage` repo instead of using
the existing checkout because of how `docker build` works: it will
send the entire local directory contents as context to the Docker
daemon, which in the case of the build tree is a _lot_ of content. (We
can discuss the wisdom&mdash;or lack thereof&mdash;of Docker's
approach separately.)

## stackage-build server

You'll need to get your SSH public key added to the machine. ~/.ssh/config info:

```
Host stackage-build
    User curators
    Hostname stackage-builder.haskell.org
```

### Running the build script

We currently run the builds manually so make it easy to see when there are
bounds issues that need to be corrected. Automated this would be even better,
we're just not there yet.

```
# Run a nightly build
/var/stackage/stackage/automated/run-nightly.sh

# Run an LTS minor bump
/var/stackage/stackage/automated/build.sh lts-15.1

# Run an LTS major bump
/var/stackage/stackage/automated/build.sh lts-16.0
```

Recommended: run these from inside a `tmux` session. If you get version bound
problems on nightly or LTS major, you need to fix build-constraints.yaml (see
info above).

### Building LTS minor releases
Before running the build, please make sure that the Dockerfile in `automated/dockerfiles/lts-X.Y` is up to date, where X is the major version that you're building and Y is the latest minor version of X for which a Dockerfile exists.
  * If any changes need to be made, (eg, new GHC version), copy `automated/lts-X.Y/Dockerfile` to `automated/lts-X.Z/Dockerfile`, where Z is the minor version you're building, and include the new changes.
  * If you are building the first release of a new LTS major version, create a new `lts-X.0/Dockerfile` based on the previous LTS's, and adjust the variables at the top to match the requirements of the snapshot.  Ensure that `STACK_VERSION` is the latest release of Stack, and `BOOTSTRAP_COMMIT` is the commit ID of this repo containing the version of the `docker/*.sh` used to build the snapshot.  Also ensure the FROM image's Ubuntu version matches that used in the [root Dockerfile](Dockerfile) used to build this snapshot.

For an LTS minor bump, you'll typically want to update <https://github.com/commercialhaskell/lts-haskell/tree/master/build-constraints> as needed:

* constraint a package by appending an upperbound
* add new packages
* enable/disable test, benchmark, haddock when needed

Then run `./build.sh lts-X.Z` to generate an updated snapshot.

If a build fails for bounds reasons, see all of the advice above. If the code
itself doesn't build, or tests fail, open up an issue and then either put in a
version bound to avoid that version or something else. It's difficult to give
universal advice on how to solve things, since each situation is unique. Let's
develop this advice over time. For now: if you're not sure, ask for guidance.

Note LTS builds will use the last Hackage data.
You may need to `run-nightly.sh` to get a newer package, but this should be less common for lts.

### Timing

A looping script on the build server keeps trying to build nightly
with `sleep 30m` interleaved. It only publishes the nightly once per
day. This way new package versions or build failures can be caught
early and hopefully the nightlies will be timely.

LTS minor bumps are typically run on weekends. It can be a good idea
to start the build on friday or saturday to have enough time to
resolve any issues before the next curator shift the coming monday.

### Diskspace errors (and website sync debugging)

* You can detect the problem by running `df`. If you see that `/` is out of space, we have a problem.
* If you see that `/var/stackage/` is out of space, you can:
  * run `./etc/diskspace/remove-old-stack-work-libs.hs [nightly|lts-XX]`
  * If that is insufficient then remove all the old builds under the previous ghc/Cabal version:
    * `rm -r /var/stackage/stackage/automated/work/[nightly|lts-XX]/unpack-dir/unpacked/*/.stack-work/dist/x86_64-linux/Cabal-X.Y.0.0/`

  optionally:
  * `rm -r /var/stackage/stackage/automated/work/lts*/unpack-dir/unpacked/`
  * `rm -r /var/stackage/stackage/automated/work/nightly/unpack-dir/unpacked/`

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

### Restarting docker

If docker hangs with e.g. `docker: Error response from daemon:
connection error: desc = "transport: dial unix
/var/run/docker/containerd/docker-containerd .sock: connect:
connection refused".` you can restart it with `sudo systemctl restart
docker.service`.

## Local curator setup

We do not run the full stackage build locally as that might take too
much time. However, some steps on the other hand are much faster to do
yourself, e.g. verifying constraints without building anything.

To get started, install `curator` via Git:

```
$ git clone git@github.com:commercialhaskell/curator.git
$ cd curator && stack install curator
```

It is a good idea to upgrade `curator` at the start of your week.
Then, clone the stackage repo, get the latest packages and run dependency
resolution:

```
$ git clone git@github.com:commercialhaskell/stackage.git
$ cd stackage
$ ./check
```

This can be used to make sure all version bounds are in place, including for
test suites and benchmarks, to check whether bounds can be lifted, and to get
[tell-me-when-its-released] notifications.

`curator` does not build anything, so you wont see any compilation
errors for builds, tests and benchmarks.

[tell-me-when-its-released]: https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md#waiting-for-new-releases

### Large scale enabling/disabling of packages

`etc/commenter` is a binary that automates `build-constraints.yaml` workflows.

#### Setup
This is currently a rust program, You can install the rust toolchain
by using [rustup](https://rustup.rs/).

#### Example usage

After disabling a few packages you get this curator output:

```
ConfigFile (GHC 9 bounds issues, @maintainer) (not present) depended on by:
- [ ] xdg-desktop-entry-0.1.1.1 (-any). @maintainer. Used by: library


pipes-misc (GHC 9 bounds issues, @maintainer) (not present) depended on by:
- [ ] pipes-fluid-0.6.0.1 (>=0.5). @handles. Used by: test-suite


testing-feat (GHC 9 bounds issues, Grandfathered dependencies) (not present) depended on by:
- [ ] dual-tree-0.2.3.0 (-any). Grandfathered dependencies. @handles. Used by: test-suite
```

Now run:
```
./check 2>&1 >/dev/null | ./commenter add
```

You will get this output:
```
[INFO] ...
LIBS + EXES

        - xdg-desktop-entry < 0 # tried xdg-desktop-entry-0.1.1.1, but its *library* requires the disabled package: ConfigFile

TESTS

    - dual-tree # tried dual-tree-0.2.3.0, but its *test-suite* requires the disabled package: testing-feat
    - pipes-fluid # tried pipes-fluid-0.6.0.1, but its *test-suite* requires the disabled package: pipes-misc

Adding 1 libs, 2 tests, 0 benches to build-constraints.yaml
```

These bounds are added to build-constraints.yaml automatically.

Re-run this command until no more packages are disabled.

#### Re-enabling

We can periodically remove all packages under the bounds sections and then re-run the disabling flow above until we get a clean plan. This will automatically pick up packages that have been fixed.

```
./commenter clear
./check 2>&1 >/dev/null | ./commenter add
```

Repeat the second command until no updates are made to build-constraints.yaml (or use `commenter add-loop` instead).

#### Checking for new releases

Run `stack update` before doing this.

`./commenter outdated` looks through all bounds issues and library
compilation failures and compares the marked version with the latest hackage release. Example output is

```
Fin mismatch, manual: 0.2.8.0, hackage: 0.2.9.0
aeson mismatch, auto: 1.5.6.0, hackage: 2.0.2.0
```

where "manual" means the bound was added manually by a curator,
perhaps due to a compilation failure so we could try re-enabling the
package. "auto" means it's part of the sections generated by
`./commenter`, to update that run the `Re-enabling` step as documented
above.

`outdated` only finds packages that are in the auto generated
sections, or that are of the form `- package < 0 # $version`.

#### Notes

* Please make sure to separate bounds issues from compilation failures/test run failures, as we cannot verify that a package builds or that tests pass without running the build!

#### Diffing snapshots / Inspecting changes

To diff existing snapshots, or to evaluate changes before they end up
in a snapshot you can run:

```
./commenter diff-snapshot <old-snapshot.yaml> <new-snapshot.yaml>
```

Existing snapshots can be retrieved from https://github.com/commercialhaskell/stackage-snapshots. Preliminary snapshots can be generated by running relevant parts of `automated/build.sh`, at the time of writing:

```
TARGET=nightly-2021-01-14 \ # the date doesn't matter
  curator update && \
  curator constraints --target $TARGET && \
  curator snapshot-incomplete --target $TARGET && \
  curator snapshot
```

#### Pinging maintainers after disabling packages

After lifting a bound We often have to disable additional packages due
to compilation failures. `affected` figures out which packages have
been disabled and which maintainers are affected. Note that this does
not handle disabled test suites and benchmarks as the snapshots don't
contain this information.

```
./commenter affected <old-snapshot.yaml> <new-snapshot.yaml>
```

E.g.:
```
$ commenter affected ../stackage-snapshots/nightly/2022/1/2.yaml ../stackage-snapshots/nightly/2022/2/7.yaml
```
```
alg-0.2.13.1: Matthew Farkas-Dyck <strake888@gmail.com> @strake
butter-0.1.0.6: Matthew Ahrens <matt.p.ahrens@gmail.com> @mpahrens
category-0.2.5.0: Matthew Farkas-Dyck <strake888@gmail.com> @strake
constraint-0.1.4.0: Matthew Farkas-Dyck <strake888@gmail.com> @strake
dl-fedora-0.9.2: Jens Petersen <juhpetersen@gmail.com> @juhp
foldable1-0.1.0.0: Matthew Farkas-Dyck <strake888@gmail.com> @strake
gitlab-haskell-0.3.2.0: Rob Stewart <robstewart57@gmail.com> @robstewart57
hslua-module-doclayout-1.0.0: Albert Krewinkel <albert+stackage@zeitkraut.de> @tarleb
util-0.1.17.1: Matthew Farkas-Dyck <strake888@gmail.com> @strake
wai-middleware-auth-0.2.5.1: Alexey Kuleshevich <lehins@yandex.ru> @lehins
yesod-csp-0.2.5.0: Bob Long <robertjflong@gmail.com> @bobjflong
```

#### Finding disabled packages with lots of dependents

`./commenter disabled` prints the number of transitive dependents a disabled package has. Low hanging fruit to get a lot of packages included again.

Example output:
```
[...]
stringable is disabled with 10 dependents
llvm-hs is disabled with 12 dependents
th-data-compat is disabled with 12 dependents
amazonka-core is disabled with 96 dependents
gogol-core is disabled with 96 dependents
```

## Adding new curators

1. Add public ssh key to `~/.ssh/authorized_keys` on build server
2. Add to commercialhaskell/stackage project.

## Dealing with a new GHC major release

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

[GHC upgrade note]: https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md#upgrading-to-a-new-ghc-version

## New LTS major bump

Every 3-6 months, we make a new major release of LTS. The procedure we follow for this is:

1. Write a blog post on stackage.org announcing the intent to cut a major
   release. Give an estimated date two weeks in the future from the publication
   date of the post.
2. Spread the blog post on social media and mailing lists as much as possible.
3. Expect maintainers to send significant requests for added packages and
   relaxed upper bounds. There will likely be some hard decisions to be made
   regarding relaxing a bound versus keeping more packages. All of these changes
   occur on master and affect nightly.
4. Once the estimated date hits, push a new `ltsXX` and wait for the docker image build.
5. Run the build procedure for the new LTS release.
6. After the LTS build completes, more aggressively prune upper bounds from
   `build-constraints.yaml`.
7. Once both (5) and (6) are done, publish a new blog post on stackage.org
   announcing the new LTS and Nightly, with links to the change pages on
   stackage.org. Include a reminder that requests for packages to be added to LTS
   may be made on commercialhaskell/lts-haskell.
