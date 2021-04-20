DOCKER_COMPOSE = docker-compose
all: build

build:
	$(DOCKER_COMPOSE) build ci

run:
	$(DOCKER_COMPOSE) run ci

push:
	$(DOCKER_COMPOSE) up --no-start ci
	$(DOCKER_COMPOSE) push ci

.PHONY: build run test all
