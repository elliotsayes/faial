DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

BUILD = _build/default
BIN = $(BUILD)/bin
TEST = _build/test
all: c-ast \
	faial-bc \
	faial-drf \
	data-dep \
	faial-bc-dyn \
	faial-sync \
	faial-gen \
	faial-cost \
	faial-cost-diff \
	wgsl-ast

clean:
	$(DUNE) clean
	rm -f faial-bin gen_kernels pico faial-gen faial-cost-diff

wgsl-ast:
	$(DUNE) build inference/bin/w_ast.exe
	cp -f $(BUILD)/inference/bin/w_ast.exe wgsl-ast

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

faial-cost:
	$(DUNE) build $(BUILD)/total_cost/main.exe
	cp -f $(BUILD)/total_cost/main.exe faial-cost

faial-cost-diff:
	$(DUNE) build $(BUILD)/total_cost/diff.exe
	cp -f $(BUILD)/total_cost/diff.exe faial-cost-diff

faial-sync:
	$(DUNE) build barrier_div/main.exe
	cp -f $(BUILD)/barrier_div/main.exe faial-sync

faial-gen:
	$(DUNE) build codegen/corvo.exe
	cp -f $(BUILD)/codegen/corvo.exe faial-gen

gen_kernels:
	$(DUNE) build codegen/gen_kernels.exe
	cp -f $(BUILD)/codegen/gen_kernels.exe gen_kernels

test: build-test
	$(DUNE) runtest

sys-test:
	python3 examples/data-dep/run.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin


.PHONY: \
	all \
	clean \
	faial-bc \
	faial-bc-dyn \
	faial-drf \
	build-test \
	test \
	sys-test \
	gitlab \
	gitlab-bin \
	gitlab-test \
	c-ast \
	data-dep \
	faial-sync \
	faial-gen \
	faial-cost \
	wgsl-ast
