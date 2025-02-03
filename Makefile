DUNE = dune

GITLAB_CACHE = /tmp/gitlab-cache

DUNE_BUILD ?= _build
BUILD = $(DUNE_BUILD)/default
BIN = $(BUILD)/bin
TEST = $(DUNE_BUILD)/test
all: c-ast \
	faial-bc \
	faial-drf \
	data-dep \
	faial-sync \
	faial-gen \
	faial-cost \
	faial-cost-diff \
	faial-cost-dyn \
	wgsl-ast

clean:
	$(DUNE) clean
	rm -f faial-bin gen_kernels pico faial-gen faial-cost-diff

build:
	echo "building dune in $(DUNE_BUILD)"
	$(DUNE) build --build-dir $(DUNE_BUILD)

drf_api_example: build
	clang examples/drf_api/main.c -o $(BUILD)/drf_api_example $(BUILD)/drf/bin/drf_api.so

run_drf_api_example: drf_api_example
	cd $(BUILD) && ./drf_api_example

wgsl-ast: build
	cp -f $(BUILD)/inference/bin/w_ast.exe wgsl-ast

c-ast: build
	cp -f $(BUILD)/inference/bin/c_ast.exe c-ast

data-dep: build
	cp -f $(BUILD)/approx/bin/main.exe data-dep

build-test:
	$(DUNE) build test

faial-drf: build
	cp -f $(BUILD)/drf/bin/main.exe faial-drf

faial-cost-dyn: build
	cp -f $(BUILD)/total_cost/dyn.exe faial-cost-dyn

faial-bc: build
	cp -f $(BUILD)/bank_conflicts/bin/main.exe faial-bc

faial-cost: build
	cp -f $(BUILD)/total_cost/main.exe faial-cost

faial-cost-diff: build
	cp -f $(BUILD)/total_cost/diff.exe faial-cost-diff

faial-sync: build
	cp -f $(BUILD)/barrier_div/main.exe faial-sync

faial-gen: build
	cp -f $(BUILD)/codegen/corvo.exe faial-gen

gen_kernels: build
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
	build \
	clean \
	drf_api \
	faial-bc \
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
	faial-cost-dyn \
	faial-cost-diff \
	wgsl-ast
