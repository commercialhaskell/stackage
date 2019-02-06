# Building Stackage with Nix

The script `build.sh` is supposed to be run in a directory where `curator unpack`
was run - it assumes existence of `unpack-dir` directory with a Stackage
snapshot `stack.yaml` and unpacked packages in it.
