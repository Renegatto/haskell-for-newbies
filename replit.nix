{ pkgs }: {
    deps = [
        pkgs.haskellPackages.ghcid.bin
        pkgs.haskellPackages.cabal-install
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            # Put your dependencies here!
            pkgs.random
        ]))
        pkgs.haskell-language-server
    ];
}