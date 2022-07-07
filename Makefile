DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BIN = _build/default/bin
TEST = _build/test

all: main

clean:
	$(DUNE) clean

c-ast:
	$(DUNE) build bin/c_ast.exe
	cp -f $(BIN)/c_ast.exe c-ast

data-dep:
	$(DUNE) build bin/data_dep.exe
	cp -f $(BIN)/data_dep.exe data-dep

build-test:
	$(DUNE) build test

main:
	$(DUNE) build bin/main.exe
	cp -f $(BIN)/main.exe faial-bin

pico:
	$(DUNE) build bin/pico.exe
	cp -f $(BIN)/pico.exe pico

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


.PHONY: all clean main build-test test ui pico sys-test gitlab gitlab-bin gitlab-test c-ast
