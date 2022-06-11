OCB_FLAGS   = -use-ocamlfind -use-menhir -I lib -no-links
OCB = ocamlbuild $(OCB_FLAGS)

GITLAB_CACHE = /tmp/gitlab-cache

BIN = _build/bin
TEST = _build/test

all: native

clean:
	$(OCB) -clean
	rm -f faial-bin bank-conflicts proto-to-cuda flores

cast:
	$(OCB) -I bin test_c.native

native: build-tests
	$(OCB) -I bin main.native
	cp $(BIN)/main.native faial-bin

bank-conflicts:
	$(OCB) -I bin -package z3 -tag thread bankconflicts.native
	cp $(BIN)/bankconflicts.native bank-conflicts

proto-to-cuda:
	$(OCB) -I bin -package toml -tag thread cgen.native prototocuda.native
	cp $(BIN)/prototocuda.native proto-to-cuda

flores:
	$(OCB) -I bin -package toml -tag thread cgen.native flores.native
	cp $(BIN)/flores.native flores

build-tests:
	$(OCB) -I test test_imp.native
	$(OCB) -I test test_common.native
	$(OCB) -I test test_locsplit.native
	$(OCB) -I test test_streamutil.native
	$(OCB) -I test test_predicates.native
	$(OCB) -I test test_parsejs.native

test: build-tests
	$(TEST)/test_imp.native
	$(TEST)/test_common.native
	$(TEST)/test_streamutil.native
	$(TEST)/test_locsplit.native
	$(TEST)/test_predicates.native
	$(TEST)/test_parsejs.native

ui:
	(cd faial-ui/ && cargo b --release)

sys-test: build-tests
	@./run-tests.py

gitlab-test:
	 gitlab-runner exec docker test --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab-bin:
	 gitlab-runner exec docker build-dist --cache-dir=${GITLAB_CACHE} --docker-cache-dir=${GITLAB_CACHE} --docker-volumes=${GITLAB_CACHE}

gitlab: gitlab-test gitlab-bin

profile:
	$(OCB) -I bin -tag profile main.native

debug:
	$(OCB) -I bin -tag debug main.byte

.PHONY: all clean byte native profile debug sanity test ui bank-conflicts proto-to-cuda flores
