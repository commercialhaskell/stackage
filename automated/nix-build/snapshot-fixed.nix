{ pkgs ? import (import ./fetch-nixpkgs.nix) {}
, compiler ? pkgs.haskell.packages.ghc863
}:
let
  snapshot = import ./snapshot.nix { pkgs = pkgs; compiler = compiler; };
in
  snapshot.override (with pkgs.haskell.lib; {
    overrides = self: super: {
      # disabling packages from non-hackage-packages.nix
      futhark = null;
      multi-ghc-travis = null;
      vaultenv = null;
      # native deps
      check-email = super.check-email.override { resolv = null; };
      flac = super.flac.override { FLAC = pkgs.flac; };
      freenect = super.freenect.override { freenect_sync = null; libfreenect = null; };
      haskell-gi = super.haskell-gi.override { gobject-introspection = pkgs.gnome3.gobjectIntrospection; };
      HDBC-mysql = super.HDBC-mysql.override { mysqlclient = pkgs.mysql;};
      libffi = super.libffi.override { ffi = null; };
      # fixes
      blas-ffi = overrideCabal super.blas-ffi (
        old: { libraryPkgconfigDepends = old.libraryPkgconfigDepends ++ [ pkgs.openblasCompat ]; }
      );
      odbc = overrideCabal super.odbc (
        old: { librarySystemDepends = old.librarySystemDepends ++ [ pkgs.freetds ]; }
      );
      html-entities = null; # https://github.com/nikita-volkov/html-entities/issues/8
      category = null; # conflict in Prelude.hs
      constraint = null; # because of category
      lame = null; # fails with 'Total ticks: 451489'
      proto-lens-protobuf-types = overrideCabal super.proto-lens-protobuf-types (
      old: { libraryToolDepends = old.libraryToolDepends ++ [ pkgs.protobuf ]; }
      );
      # libraft = null; # xxx/postgres/entries.sql: openFile: does not exist (No such file or directory) in libraft-0.2.0.0
      xmlbf-xeno = null; # because of html-entities
      xmonad = null; # xmonad requires extra patches for Nix
      xmonad-contrib = null;
      xmonad-extras = null;
      hlibgit2 = disableHardening super.hlibgit2 [ "format" ];
    };
  })
