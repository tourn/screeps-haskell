# screeps-haskell

This shall become a starting point for creating an AI for the [Screeps](https://screeps.com/) game in Haskell. The Javascript Bindings are currently very rudimentary and don't consider multi-room support.

To run the upload script you will need a screeps account, which you currently only get by purchasing the game. Theoretically you could use the free demo mode if you manage to paste the big output js (see below)

**ZuriHac**: I'll try to get a setup where we all can use my own account.

The build process rather clumsily runs `stack` inside a Docker container with a prebuilt ghcjs (since that thing takes ages to build). I haven't got the `stack docker` integration to work properly. I probably also should package node in the image for the upload script (or even better, convert the upload script to haskell in the first place)

## How to run it

* Have a *.env* file with USERNAME and PASSWORD of your screeps account

* **FIRST TIME**: run `make setup` (which runs stack setup in a docker container)

* Run `make build`

* Go to https://screeps.com/a/#!/sim/tutorial/4 and click away the dialogues,

* Upload the script: 

  * Either use `make upload` to build and upload the script, select the *haskell* branch
  
  * Or paste the contents of *.stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/screeps-exe/screeps-exe.jsexe/all.js* into the main module. Theoretically this should work, although my browser seems to refuse to paste the >1MB of code.

* Look at them go!


