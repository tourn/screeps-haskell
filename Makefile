include .env
export $(shell sed 's/=.*//' .env)

upload: build
	node upload-https.js $$EMAIL $$PASSWORD

build:
	docker run --rm -v `pwd`:/opt/workspace -w /opt/workspace tourn/screeps:latest stack build

