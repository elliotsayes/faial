DOCKER_COMPOSE = docker-compose
all: build

build:
	$(DOCKER_COMPOSE) build dev

run:
	$(DOCKER_COMPOSE) run dev

push:
	$(DOCKER_COMPOSE) up --no-start dev
	$(DOCKER_COMPOSE) push dev

.PHONY: build run test all
