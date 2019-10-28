DOCKER = time docker
IMAGE = registry.gitlab.com/cogumbreiro/faial/faial-dev
VERSION = 1

all: build

build:
	$(DOCKER) build -f docker/Dockerfile -t $(IMAGE):$(VERSION) .

run:
	$(DOCKER) run -v $(PWD):/out --env VERSION=$(VERSION) -it --rm $(IMAGE):$(VERSION)

push:
	$(DOCKER) push $(IMAGE):$(VERSION)

.PHONY: build run test all
