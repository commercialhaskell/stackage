## Processing

High level series of steps for processing

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
the system/compiler being used, etc. BuildConstraints discuss the build as a
whole, whereas PackageConstraints discuss the constraints on an individual
package.

There are two primary ways of getting a BuildConstraints.
defaultBuildConstraints inspects the GHC in the PATH environment variable to
determine GHC version, core packages, core tools, etc. It then uses the
Stackage.Config module to extract information on additional packages to be
installed. The secondary approach is in UpdateBuildPlan, which will be
discussed later.

BuildConstraints do not specify a build completely. That is given by a
BuildPlan, which is similarly broken down into BuildPlan and PackagePlan. In
order to get a BuildPlan, we need two pieces of information: the
BuildConstraints, and a package index. The package index (usually downloaded
from Hackage) is a collection of all of the cabal files available.

By applying a BuildConstraints to a package index (via newBuildPlan), we get a
proposed BuildPlan. There is no guarantee that this BuildPlan is valid. To
validate it, we use checkBuildPlan. A BuildPlan is an instance of both ToJSON
and FromJSON, and therefore can be serialized to a file for later use.

When dealing with LTS Haskell, we want to be able to take a BuildPlan, and
update to a newer BuildPlan that keeps all packages in the same major version.
updateBuildConstraints turns a BuildPlan into a new BuildConstraints with that
restriction, and updateBuildPlan applies newBuildPlan to that result. As
mentioned previously: this is *not* a validated result, and therefore
checkBuildPlan must be used.

A BuildPlan can actually be acted on. This is done to check that all packages
compile together, run relevant test suites, test Haddock documentation is
correct, and produce as artifacts both a self-contained GHC binary package
database and a set of Haddock documentation. (Not yet implemented.)

A BuildPlan may be converted into a bundle to be uploaded to Stackage Server.
(Not yet implemented.)
