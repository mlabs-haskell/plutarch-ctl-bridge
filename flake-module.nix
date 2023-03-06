{ self
, config
, lib
, flake-parts-lib
, ...
}:
let
  inherit
    (flake-parts-lib)
    mkPerSystemOption
    ;
  inherit
    (lib)
    types
    mkOption
    ;
in
{
  options = {
    perSystem =
      mkPerSystemOption
        ({ config
         , self'
         , inputs'
         , pkgs
         , system
         , ...
         }:
          let
            ctlBridgeSubmodule = types.submodule ({ name, ... }: {
              options = {
                template = mkOption {
                  type = types.path;
                  description = "Path to the template directory to create `src` in";
                };

                packageJson = lib.mkOption {
                  description = ''
                    Path to the project's package.json file.
                  '';
                  type = types.path;
                };

                packageLock = lib.mkOption {
                  description = ''
                    Path to the project's package-lock.json file.
                  '';
                  type = types.path;
                };

                package = mkOption {
                  type = types.package;
                  description = "Package to use for the bridge";
                };

                binName = mkOption {
                  type = types.str;
                  description = "The name of the binary from `package`.";
                };

                outName = mkOption {
                  type = types.str;
                  description = ''
                    The name of the output that will be created by the binary.
                  '';
                };
              };
            });
          in
          {
            options.ctlBridge = lib.mkOption {
              type = types.attrsOf ctlBridgeSubmodule;
              description = ''
                A submodule for each CTL bridge.
              '';
            };
            config = {
              packages =
                let
                  mkPackage = name: conf: {
                    name = "ctl-bridge-${name}";
                    value = pkgs.stdenvNoCC.mkDerivation {
                      name = "ctl-bridge-${name}";
                      src = conf.template;

                      nativeBuildInputs = [
                        conf.package
                        pkgs.fd
                        pkgs.nodePackages.purs-tidy
                      ];

                      buildPhase = ''
                        runHook preBuild
                        ${conf.binName}
                        rm -f src
                        mv ${conf.outName} src
                        purs-tidy format-in-place $(fd -epurs)
                        runHook postBuild
                      '';

                      installPhase = ''
                        runHook preInstall
                        cp -r . $out
                        runHook postInstall
                      '';
                    };
                  };
                in
                builtins.listToAttrs
                  (lib.attrsets.mapAttrsToList mkPackage config.ctlBridge);

              # offchain =
              #   let
              #     mkOffchain = name: conf: {
              #       name = "offchain-${name}";
              #       value = {
              #         src = self'.packages."ctl-bridge-${name}";
              #         inherit (conf) packageJson packageLock;

              #         spagoOverride = {
              #           "liqwid-ctl-extra" = self.inputs.liqwid-ctl-extra;
              #         };

              #         runtime = {
              #           enableCtlServer = false;
              #           exposeConfig = false;
              #         };

              #         ignoredWarningCodes = [
              #           "ImplicitImport"
              #           "UserDefinedWarning"
              #           "WildcardInferredType"
              #         ];

              #         enableFormatCheck = true;
              #       };
              #     };
              #   in
              #   builtins.listToAttrs
              #     (lib.attrsets.mapAttrsToList mkOffchain config.ctlBridge);
            };
          });
  };
}
