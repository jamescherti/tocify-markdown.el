{ pkgs, ... }:

let tocify-markdown-emacs = pkgs.emacsWithPackages (epkgs:
      (with epkgs.melpaStablePackages; [
        markdown-mode s dash pkgs.tocify-markdown
      ])
    );
in pkgs.stdenv.mkDerivation {
  name = "tocify-markdown";
  buildInputs = with pkgs; [
    tocify-markdown-emacs
    pkgs.gitAndTools.hub
    cask
  ];
  src = null;
}
