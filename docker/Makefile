DOCKER_COMPOSE = docker-compose
all: build

build:
	$(DOCKER_COMPOSE) build

run:
	$(DOCKER_COMPOSE) run faial

push:
	$(DOCKER_COMPOSE) create
	$(DOCKER_COMPOSE) push

.PHONY: build run test all
