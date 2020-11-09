OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -no-links
OCB = ocamlbuild $(OCB_FLAGS)

GITLAB_CACHE = /tmp/gitlab-cache

BUILD = _build/src

all: native byte

clean:
	$(OCB) -clean

native: build-tests
	$(OCB) main.native
	cp $(BUILD)/main.native faial-bin

build-tests:
#	$(OCB) test_loops.native
	$(OCB) test_common.native
	$(OCB) test_phasesplit.native
	$(OCB) test_streamutil.native

test: build-tests
#	$(BUILD)/test_loops.native
	$(BUILD)/test_streamutil.native
	$(BUILD)/test_common.native
	$(BUILD)/test_phasesplit.native

sys-test: build-tests
	@./run-tests.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

.PHONY: all clean byte native profile debug sanity test
