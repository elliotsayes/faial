DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BUILD = _build/default
BIN = $(BUILD)/bin
TEST = _build/test
all: c-ast pico ng data-dep

clean:
	$(DUNE) clean

c-ast:
	$(DUNE) build inference/bin/c_ast.exe
	cp -f $(BUILD)/inference/bin/c_ast.exe c-ast

data-dep:
	$(DUNE) build index_dep/data_dep.exe
	cp -f $(BUILD)/index_dep/data_dep.exe data-dep

build-test:
	$(DUNE) build test

ng:
	$(DUNE) build drf/bin/next_gen.exe
	cp -f $(BUILD)/drf/bin/next_gen.exe next-gen

#main:
#	$(DUNE) build drf/bin/main.exe
#	cp -f $(BUILD)/drf/bin/main.exe faial-bin

pico:
	$(DUNE) build bank_conflicts/pico.exe
	cp -f $(BUILD)/bank_conflicts/pico.exe pico

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


.PHONY: all clean main build-test test ui pico sys-test gitlab gitlab-bin gitlab-test c-ast data-dep
