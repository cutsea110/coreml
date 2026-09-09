PACKAGES := chap2 chap3 chap4 compiler

.PHONY: all build test clean $(PACKAGES)

.DEFAULT_GOAL := all

all: build

build:
	cabal build all

# `make test`            -> cabal test all
# `make test chap2 chap4` -> cabal test for just those packages
test:
ifneq ($(filter $(PACKAGES),$(MAKECMDGOALS)),)
	@for pkg in $(filter $(PACKAGES),$(MAKECMDGOALS)); do \
		cabal test $$pkg; \
	done
else
	cabal test all
endif

clean:
	cabal clean

# `make chap2` / `make compiler` etc. -> cabal build that package
# When combined with `test` (e.g. `make test chap2`) or `clean`, this is a
# no-op, since the `test`/`clean` recipes above already handle it.
$(PACKAGES):
ifeq ($(filter test clean,$(MAKECMDGOALS)),)
	cabal build $@
else
	@:
endif
