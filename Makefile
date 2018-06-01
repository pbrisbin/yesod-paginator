all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build \
	  --flag yesod-paginator:examples \
	  --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build \
	  --flag yesod-paginator:examples \
	  --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --flag yesod-paginator:examples \
	  --pedantic --test


.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: docs
docs:
	stack --work-dir .stack-work-docs build --haddock

.PHONY: check-nightly
check-nightly:
	stack setup --resolver nightly
	stack build --resolver nightly \
	  --flag yesod-paginator:examples \
	  --pedantic --test
