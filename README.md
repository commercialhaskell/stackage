stackage
========

"Stable Hackage," tools for creating a vetted set of packages from Hackage.

A note about the codebase: the goal is to minimize dependencies and have
the maximum range of supported compiler versions. Therefore, we avoid
anything "complicated." For example, instead of using the text package,
we use Strings everywhere.

Get your package included
-------------------------

In order to get your package included in the set of stable packages, you should
send a pull request against this repository. In the `Stackage.Config` module,
there's a function called `defaultStablePackages`. In general, to add a set of
packages, you would add:

    mapM_ (add "your-email-address") $ words
        "package1 package2 package3"

You can follow the examples of the other sets of packages in that function.
Once you've done this, you should confirm that your newly added packages are
compatible with the rest of stackage by building the package set.

You should also read the [maintainers
agreement](https://github.com/fpco/stackage/wiki/Maintainers-Agreement).

Build the package set
---------------------

As this project is just starting, we don't really have a solid set of steps. In
general, the following set of commands should be good for getting started:

    cabal update
    git clone https://github.com/fpco/stackage
    cd stackage
    cabal sandbox init # requires cabal-install 1.18
    cabal install --only-dependencies
    ./dist/build/stackage/stackage select
    ./dist/build/stackage/stackage check
    ./dist/build/stackage/stackage build # takes a *long* time
    ./dist/build/stackage/stackage test # also takes a *long* time

Notes
-----

Make sure to have Cabal-1.16 installed in either your global or user database,
regardless of any sandboxing, as custom build types require it to be present.
You must build with cabal-install 1.16, due to several important bug fixes.

Using a non-Haskell Platform versions of GHC
--------------------------------------------

By default, Stackage bases itself off of the Haskell Platform for determining
which packages are core packages, and locks down package versions to match the
Haskell Platform selections. This works fine when you are compiling with the
same version of GHC as the Haskell Platform was built on. If you're using a
different version of GHC, you'll probably need to use the following options for
the `select` call:

    --no-platform --use-global-db

The former says to disregard Haskell Platform package versions, and the latter
says to determine which packages are core packages based on their presence in
the global package database.
