include .env
export $(shell sed 's/=.*//' .env)

build:
	docker run --rm -v `pwd`:/opt/workspace -w /opt/workspace tourn/screeps:latest stack build --allow-different-user

watch:
	docker run -it --rm -v `pwd`:/opt/workspace -w /opt/workspace tourn/screeps:latest stack build --allow-different-user --file-watch

setup:
	docker run --rm -v `pwd`:/opt/workspace -w /opt/workspace tourn/screeps:latest stack setup --allow-different-user

upload: build
	node upload-https.js $$EMAIL $$PASSWORD

run: build
	node .stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/screeps-exe/screeps-exe.jsexe/all.js

