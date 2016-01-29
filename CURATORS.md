This is a collection of instructions covering the processes Stackage curators - the
guys who maintain the Stackage project itself - should be doing on a regular basis.
Originally this was handled by Michael Snoyman,
but now we are a team of 4 people handling requests weekly in rotation.
Curation activities are mostly automated, and do not take up a significant amount of time.

## Workflow

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

The typical story on pull requests is: if Travis accepts it, and the author
only added packages under his/her own name, merge it. If the build later fails
(see below), then block the package until it's fixed.

## Fixing bounds issues

The most common activity you'll deal with in Stackage curation is a version
bound issue, usually a restrictive upper bound. You fix this by opening an
issue on the Stackage repo about the problem, and modifying the
build-constraints.yaml file to work around it in one of the ways below. Be sure
to refer to the issue for workarounds added to that file.

* __Temporary upper bounds__ Most common technique, just prevent a new version of a library from being included immediately
* __Skipping tests and benchmarks__ If the upper bound is only in a test suite or benchmark, you can add the relevant package to skipped-tests or skipped-benchmarks. For example, if conduit had an upper bound on criterion for a benchmark, you could added conduit as a skipped benchmark.
* __Excluding packages__ In an extreme case of a non-responsive maintainer, you can remove the package entirely from Stackage. We try to avoid that whenever possible

## Updating the content of the Docker image used for building

### Adding Debian packages for required system tools or libraries
Additional (non-Haskell) system libraries or tools should be added to `stackage/debian-bootstrap.sh`.
Committing the changes should trigger a DockerHub. Normally only the nightly branch needs to be updated
since new packages are not added to the current lts release.

Use [Ubuntu Package content search](http://packages.ubuntu.com/) to determine which package provides particular dev files (it defaults to trusty which is the same version as the server).

### Upgrading GHC version
The Dockerfile contains information on which GHC versions should be used. You
can modify it and push it to Github to trigger a DockerHub build. The master
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
force all packages to be rebuilt. See: [issue#746](https://github.com/fpco/stackage/issues/746).

## stackage-build server

You'll need to get your SSH public key added to the machine. ~/.ssh/config info:

```
Host stackage-build
    User ubuntu
    Hostname ec2-52-5-20-252.compute-1.amazonaws.com
```

We currently run the builds manually so make it easy to see when there are
bounds issues that need to be corrected. Automated this would be even better,
we're just not there yet.

```
# Run a nightly build
/opt/stackage-build/stackage/automated/build.sh nightly-2015-07-08

# Run an LTS minor bump
/opt/stackage-build/stackage/automated/build.sh lts-2.17

# Run an LTS major bump
/opt/stackage-build/stackage/automated/build.sh lts-3.0
```

Recommended: run these from inside a `screen` session. If you get version bound
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
develop this advice over time. For now: if you're not sure, ask Michael for
guidance.

### Timing
A cronjob on the build server keeps trying to build nightly unless it has already succeeded.

(Nightly builds should be run once a day. A common technique I use is, after a
build succeeds, write something like `sleep 20h;
/opt/stackage-build/stackage/automated/build.sh nightly-2015-01-02`.)

LTS minor bumps typically are run on Sundays.

### Website sync debugging (and other out of disk space errors)

* You can detect the problem by running `df`. If you see that `/` is out of space, we have a problem
* There are many temp files inside `/home/ubuntu/stackage-server-cron` that can be cleared out occasionally
* You can then manually run `/home/ubuntu/stackage-server-cron.sh`, or wait for the cron job to do it
