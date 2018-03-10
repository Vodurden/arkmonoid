An Arkanoid clone written in Haskell

# Pre-requisites:

- [nix](https://nixos.org/nix/download.html)
- nix-unstable channel: `sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable`

# Build/Run:

    > nix-shell          # Drops you into a shell with all the correct dependencies. Similar to NVM/rbenv but better
    > cabal configure    # Prepare the program for compliation. Only needs to be run once
    > cabal build        # Compile the program. Executable is available under `dist/build/arkmonoid`
    > cabal run tool     # Run the game
