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

Build the package set
---------------------

As this project is just starting, we don't really have a solid set of steps. In
general, the following set of commands should be good for getting started:

    cabal update
    cabal install cabal-dev
    git clone https://github.com/fpco/stackage
    cd stackage
    git submodule update --init # get the Haskell Platform files
    runghc app/stackage.hs build # takes a *long* time

Notes
-----

Make sure to have Cabal-1.16 installed in either your global or user database,
regardless of any sandboxing, as custom build types require it to be present.
You must build with cabal-install 1.16, due to several important bug fixes.
