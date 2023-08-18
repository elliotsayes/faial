DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BUILD = _build/default
BIN = $(BUILD)/bin
TEST = _build/test
all: c-ast faial-bc faial-drf data-dep faial-bc-dyn

clean:
	$(DUNE) clean

c-ast:
	$(DUNE) build inference/bin/c_ast.exe
	cp -f $(BUILD)/inference/bin/c_ast.exe c-ast

data-dep:
	$(DUNE) build index_dep/main.exe
	cp -f $(BUILD)/index_dep/main.exe data-dep

build-test:
	$(DUNE) build test

faial-drf:
	$(DUNE) build drf/bin/main.exe
	cp -f $(BUILD)/drf/bin/main.exe faial-drf

faial-bc-dyn:
	$(DUNE) build bank_conflicts/dyn.exe
	cp -f $(BUILD)/bank_conflicts/dyn.exe faial-bc-dyn

faial-bc:
	$(DUNE) build bank_conflicts/pico.exe
	cp -f $(BUILD)/bank_conflicts/pico.exe faial-bc

test: build-test
	$(DUNE) runtest
	(cd examples/data-dep; python3 run.py)

sys-test:
	@./run-tests.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin


.PHONY: all clean faial-bc faial-bc-dyn faial-drf build-test test sys-test gitlab gitlab-bin gitlab-test c-ast data-dep
