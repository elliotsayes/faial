DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BIN = _build/default/bin
TEST = _build/test

all: main

clean:
	$(DUNE) clean
	rm -f faial-bin flores pico proto-to-cuda

cast:
	$(DUNE) build bin/test_c.exe

build-test:
	$(DUNE) build test

main:
	$(DUNE) build bin/main.exe
	cp $(BIN)/main.exe faial-bin

pico:
	$(DUNE) build bin/pico.exe
	cp $(BIN)/pico.exe pico

proto-to-cuda:
	$(DUNE) build bin/prototocuda.exe
	cp $(BIN)/prototocuda.exe proto-to-cuda

flores:
	$(DUNE) build bin/flores.exe
	cp $(BIN)/flores.exe flores

test: build-test
	$(DUNE) runtest

ui:
	(cd faial-ui/ && cargo b --release)

sys-test:
	@./run-tests.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin


.PHONY: all clean main cast build-test test ui pico proto-to-cuda flores sys-test gitlab gitlab-bin gitlab-test
