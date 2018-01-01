Checklist:
- [ ] Meaningful commit message - please not `Update build-constraints.yml`
- [ ] Some time passed since Hackage upload
- [ ] On your own machine, in a new directory, you have succesfully run the following set of commands (replace `$package` with the name of the package that is submitted, `$version` is the version of the package you want to get into Stackage):

      stack unpack $package
      cd $package-$version
      stack init --resolver nightly
      stack build --resolver nightly --haddock --test --bench --no-run-benchmarks
