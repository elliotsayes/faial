DOCKER_COMPOSE = docker-compose
all: build

build:
	$(DOCKER_COMPOSE) build faial

run:
	$(DOCKER_COMPOSE) run faial

push:
	$(DOCKER_COMPOSE) up --no-start faial
	$(DOCKER_COMPOSE) push faial

.PHONY: build run test all
