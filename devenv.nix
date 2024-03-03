{ pkgs, ... }:

{
  packages = with pkgs; [ rustPlatform.bindgenHook cmake clang z3  pkg-config ];

  env.LD_LIBRARY_PATH = "${pkgs.z3.lib}/lib";
}
