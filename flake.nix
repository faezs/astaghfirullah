{
  description = "Astaghfirullah";
    inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixops-plugged.url = "github:lukebfox/nixops-plugged";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  outputs = { self, nixpkgs, flake-utils, nixops-plugged, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "astaghfirullah";
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
            allowBroken = true;
          };
        };
        haskellPackages = pkgs.haskellPackages;
        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        inherit (pkgs.lib.trivial) pipe flip;
        inherit (pkgs.lib.lists) optionals;

        haskPackages = haskellPackages.override {
          overrides = self: super: rec {
            servant = self.callHackage "servant" "0.18.3" {};
            servant-server = self.callHackage "servant-server" "0.18.3" {};
            lucid-htmx = jailbreakUnbreak haskellPackages.lucid-htmx;
            servant-htmx = jailbreakUnbreak haskellPackages.servant-htmx;
            streamly-process = pkgs.haskell.lib.dontCheck (jailbreakUnbreak super.streamly-process);
          };
        };
        shellDeps = with haskPackages; [
          haskell-language-server
          ghcid
          cabal-install
          nixops-plugged.defaultPackage.${system}
        ];
        project = returnShellEnv:
          haskPackages.developPackage {
            inherit returnShellEnv;
            name = packageName;
            root = ./.;
            withHoogle = returnShellEnv;
            modifier = drv:
              let inherit (pkgs.haskell.lib) addBuildTools;
              in
              pipe drv
                [
                  # Transform the Haskell derivation (`drv`) here.
                  (flip addBuildTools
                    (optionals returnShellEnv shellDeps))
                ];
          };
      in
      {
        packages.${packageName} = project false;

        defaultPackage = self.packages.${system}.${packageName};
        devShells = {
          default = project true;
        };
        devShell = self.devShells.${system}.default;

        # nixopsConfigurations.default = {
        #   network.storage.legacy.databasefile = "./deployments.nixops";
        #   network.description = domain;
        #   network.enableRollback = true;
        #   defaults.nixpkgs.pkgs = pkgsFor "x86_64-linux";
        #   defaults._module.args = {
        #     inherit domain;
        #   };
        # };
      }
    );
}
