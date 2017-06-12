# screeps-haskell

This shall become a starting point for creating an AI for the [Screeps](https://screeps.com/) game in Haskell. The Javascript Bindings are currently very rudimentary and don't consider multi-room support.

To run the upload script you will need a screeps account, which you currently only get by purchasing the game. Theoretically you could use the free demo mode if you manage to paste the big output js (see below)

**ZuriHac**: I'll try to get a setup where we all can use my own account.

The build process rather clumsily runs `stack` inside a Docker container with a prebuilt ghcjs (since that thing takes ages to build). I haven't got the `stack docker` integration to work properly. I probably also should package node in the image for the upload script (or even better, convert the upload script to haskell in the first place)

## Where do I start?

To make any sense of what's happening, you need a basic grasp of the game. Screeps has a [free tutorial](https://screeps.com/ptr/#!/sim/tutorial) that you can do in javascript.

Here's the very very basics:

* You have little guys called *creeps* for which you program the AI

* The very minimum of functionality is having creeps collecting *energy* from a *source* (the yellow blocks) and carrying it to the *room controller* (the octagonal thing), upgrading it, so your power increases.

* Creeps are eventually going to die, so you will have to *spawn* new creeps. For this, you have to put energy into the *Spawn* and its *Extensions* (the round things). Then you tell the spawn to create a new creep, consuming energy based on the kind of creep you spawn.


[src/Lib.hs](https://github.com/tourn/screeps-haskell/blob/master/src/Lib.hs) currently contains the main logic. Start looking at what's happening there.

## How to run it with Atom

Use this plugin which should handle everything for you: https://github.com/bigaru/screeps-haskell-atom/

## How to run it otherwise

* Have a *.env* file with USERNAME and PASSWORD of your screeps account. If you don't have an account, just create an empty .env file so the Makefile works

* With Docker

  * **FIRST TIME**: run `make setup` (which runs stack setup in a docker container)

  * Run `make build`
  
* Without Docker

  * **FIRST TIME**: Run `stack setup` (will compile GHCJS, which is going to take a while)
  
  * Run `stack build`


* Go to https://screeps.com/a/#!/sim/tutorial/4 and click away the dialogues,

* Upload the script: 

  * Either use `make upload` to build and upload the script, select the *haskell* branch
  
  * Or paste the contents of *.stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/screeps-exe/screeps-exe.jsexe/all.js* into the main module. Theoretically this should work, although my browser seems to refuse to paste the >1MB of code.

* Look at them go!


