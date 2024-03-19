Checklist:
- [ ] Meaningful commit message, eg `add my-cool-package` (please don't mention `build-constraints.yml`)
- [ ] At least 30 minutes have passed since uploading to Hackage
- [ ] If applicable, required system libraries are added to [02-apt-get-install.sh](https://github.com/commercialhaskell/stackage/blob/master/docker/02-apt-get-install.sh) or [03-custom-install.sh](https://github.com/commercialhaskell/stackage/blob/master/docker/03-custom-install.sh)
- [ ] (recommended) Package have been verified to work with the latest nightly snapshot, e.g by running the [verify-package script](https://github.com/commercialhaskell/stackage/blob/master/verify-package)
- [ ] (optional) Package is compatible with the latest version of all dependencies (Run `cabal update && cabal outdated`)

The script runs virtually the following commands in a clean directory:

      stack unpack $package-$version # `-$version` is optional
      cd $package-$version
      rm -f stack.yaml && stack init --resolver nightly --ignore-subdirs
      stack build --resolver nightly --haddock --test --bench --no-run-benchmarks
