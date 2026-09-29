{
  inputs = {
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    nixpkgs.follows = "typelevel-nix/nixpkgs";
    flake-utils.follows = "typelevel-nix/flake-utils";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, typelevel-nix, sops-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlays.default ];
        };

        # SOPS secrets
        secretValues = {
          FONTAWESOME_NPM_AUTH_TOKEN = "SOPS_ENCRYPTED_FONTAWESOME_NPM_AUTH_TOKEN";
          GPP_SLACK_WEBHOOK_URL = "SOPS_ENCRYPTED_GPP_SLACK_WEBHOOK_URL";
          SSO_SERVICE_JWT = "SOPS_ENCRYPTED_SSO_SERVICE_JWT";
        };
      in
      {
        devShell = pkgs.devshell.mkShell {
          imports = [ typelevel-nix.typelevelShell ];
          packages = [
            pkgs.typescript-language-server
            pkgs.vscode-langservers-extracted
            pkgs.prettier
            pkgs.typescript
            pkgs.graphqurl
            pkgs.hasura-cli
            pkgs.pnpm_12
            pkgs.sops
            pkgs.age
            pkgs.yq
            pkgs.direnv
            pkgs.websocat
            pkgs.github-cli
          ];
          typelevelShell = {
            nodejs.enable = true;
            jdk.package = pkgs.jdk25;
          };
          env = [
            {
              name = "NODE_OPTIONS";
              value = "--max-old-space-size=8192";
            }
            {
              name = "FONTAWESOME_NPM_AUTH_TOKEN";
              value = secretValues.FONTAWESOME_NPM_AUTH_TOKEN;
            }
            {
              name = "GPP_SLACK_WEBHOOK_URL";
              value = secretValues.GPP_SLACK_WEBHOOK_URL;
            }
            {
              name = "SSO_SERVICE_JWT";
              value = secretValues.SSO_SERVICE_JWT;
            }
            {
              "name" = "SITE";
              "value" = "development";
            }
          ];
        };
      }

    );
}
