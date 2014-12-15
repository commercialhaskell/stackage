stackage
========

"Stable Hackage," tools for creating a vetted set of packages from Hackage.

__NOTE__ This repository is for package authors to get their code into
Stackage. If you simply want to use Stackage as an end user, please follow the
instructions on [http://www.stackage.org/](http://www.stackage.org).

A note about the codebase: the goal is to minimize dependencies and have
the maximum range of supported compiler versions. Therefore, we avoid
anything "complicated." For example, instead of using the text package,
we use Strings everywhere.

Get your package included
-------------------------

In order to get your package included in the set of stable packages, you should
send a pull request against this repository. In the `build-constraints.yaml` file,
there's a function called `defaultStablePackages`. In general, to add a set of
packages, you would add:

    mapM_ (add "your-email-address") $ words
        "package1 package2 package3"

You can follow the examples of the other sets of packages in that function.
Once you've done this, you can confirm that your newly added packages are
compatible with the rest of stackage by building the package set following the
instructions below.

__NOTE__: In order to ease the process of adding new packages, we no longer
require new submissions to be tested on your own system before sending a pull
request. If you believe your package works with the newest versions of all
dependencies, you may send a pull request without testing first. If you do so,
please be sure to state this in the pull request so that the Stackage
maintainers (e.g., Michael) will know to do basic sanity checking before
merging.

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

The code itself
---------------
