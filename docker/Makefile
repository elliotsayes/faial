DOCKER_COMPOSE = docker-compose
all: build

build: build-ci build-dev

build-base:
	$(DOCKER_COMPOSE) build base

build-ci: build-base
	$(DOCKER_COMPOSE) build ci

build-dev:
	$(DOCKER_COMPOSE) build dev

ci:
	$(DOCKER_COMPOSE) run ci

dev:
	$(DOCKER_COMPOSE) run dev

push:
	$(DOCKER_COMPOSE) up --no-start ci
	$(DOCKER_COMPOSE) push ci

.PHONY: build run test all build-ci build-dev build-base
