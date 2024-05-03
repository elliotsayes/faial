DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BUILD = _build/default
BIN = $(BUILD)/bin
TEST = _build/test
all: c-ast faial-bc faial-drf data-dep faial-bc-dyn faial-sync

clean:
	$(DUNE) clean

c-ast:
	$(DUNE) build inference/bin/c_ast.exe
	cp -f $(BUILD)/inference/bin/c_ast.exe c-ast

data-dep:
	$(DUNE) build approx/bin/main.exe
	cp -f $(BUILD)/approx/bin/main.exe data-dep

build-test:
	$(DUNE) build test

faial-drf:
	$(DUNE) build drf/bin/main.exe
	cp -f $(BUILD)/drf/bin/main.exe faial-drf

faial-bc-dyn:
	$(DUNE) build bank_conflicts/bin/dyn.exe
	cp -f $(BUILD)/bank_conflicts/bin/dyn.exe faial-bc-dyn

faial-bc:
	$(DUNE) build $(BUILD)/bank_conflicts/bin/main.exe
	cp -f $(BUILD)/bank_conflicts/bin/main.exe faial-bc

faial-sync:
	$(DUNE) build barrier_div/main.exe
	cp -f $(BUILD)/barrier_div/main.exe faial-sync

test: build-test
	$(DUNE) runtest

sys-test:
	python3 examples/data-dep/run.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin


.PHONY: all clean faial-bc faial-bc-dyn faial-drf build-test test sys-test gitlab gitlab-bin gitlab-test c-ast data-dep faial-sync
