stackage
========

"Stable Hackage," tools for creating a vetted set of packages from Hackage.

A note about the codebase: the goal is to minimize dependencies and have
the maximum range of supported compiler versions. Therefore, we avoid
anything "complicated." For example, instead of using the text package,
we use Strings everywhere.

Getting Started
---------------

As this project is just starting, we don't really have a solid set of steps. In
general, the following set of commands should be good for getting started:

    cabal update
    cabal install cabal-dev
    git clone https://github.com/snoyberg/stackage
    cd stackage
    git submodule update --init # get the Haskell Platform files
    runghc app/stackage.hs build # takes a *long* time
    runghc app/stackage.hs init # modifies your ~/.cabal/config file
