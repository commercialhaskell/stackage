Checklist:
- [ ] Meaningful commit message, eg `add my-cool-package` (please not mention `build-constraints.yml`)
- [ ] At least 30 minutes have passed since uploading to Hackage
- [ ] On your own machine, you have successfully run the following command (find verify-package in the root of this repo):

      ./verify-package $package # or $package-$version

The script runs virtually the following commands in a clean directory:

      stack unpack $package-$version # `-$version` is optional
      cd $package-$version
      rm -f stack.yaml && stack init --resolver nightly --ignore-subdirs
      stack build --resolver nightly --haddock --test --bench --no-run-benchmarks
