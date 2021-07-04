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

bank-conflicts:
	$(OCB) bankconflicts.native
	cp $(BUILD)/bankconflicts.native bank-conflicts

build-tests:
	$(OCB) test_imp.native
	$(OCB) test_common.native
	$(OCB) test_locsplit.native
	$(OCB) test_streamutil.native
	$(OCB) test_predicates.native

test: build-tests
	$(BUILD)/test_imp.native
	$(BUILD)/test_common.native
	$(BUILD)/test_streamutil.native
	$(BUILD)/test_locsplit.native
	$(BUILD)/test_predicates.native

ui:
	(cd faial-ui/ && cargo b --release)

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

.PHONY: all clean byte native profile debug sanity test ui bank-conflicts
