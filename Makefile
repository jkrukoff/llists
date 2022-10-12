.DEFAULT_GOAL := all
.SHELLFLAGS := -e -o pipefail -c
MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
PATH := bin:node_modules/.bin:$(PATH)
SHELL := bash

DEPS_TARGETS := package-lock.json poetry.lock rebar.lock
PRETTIER_GLOBS := *.json *.md doc/*.md *.yaml
REBAR_PROFILES := test chunk html markdown
REBAR ?= ${if ${wildcard ./rebar3}, ./rebar3, rebar3}
REQUIREMENTS := hadolint prettier markdownlint-cli2 npm pic2plot poetry rebar3 shellcheck shfmt
REQUIREMENT_TARGETS := $(addprefix require-,$(REQUIREMENTS))
SHFMT_ARGS := -i 2 -ci -sr

.PHONY: all
## Build everything.
all: $(DEPS_TARGETS)
	$(MAKE) check-requirements deps
	$(MAKE) check compile doc

.PHONY: check
ifdef DOCKER
## Run all checks and linting.
check: require-poetry
	poetry run docker-compose build
	poetry run docker-compose run check -j2 --output-sync check
else
check: check-rebar check-erlfmt check-shellcheck check-hadolint check-poetry check-npm check-hank check-markdownlint check-yamllint check-prettier check-shfmt
endif

.PHONY: check-erlfmt
## Check erlang file formatting.
check-erlfmt: require-rebar3
	$(REBAR) as test fmt --check

.PHONY: check-geas
## Check OTP version compatibility.
check-geas: require-rebar3
	$(REBAR) as test geas

.PHONY: check-hadolint
## Lint Dockerfile files.
check-hadolint: require-hadolint
	hadolint Dockerfile

.PHONY: check-hank
## Check for unused preprocessor code.
check-hank: require-rebar3
	$(REBAR) as test hank

.PHONY: check-markdownlint
## Lint Markdown files.
check-markdownlint: require-markdownlint-cli2
	markdownlint-cli2 CHANGELOG.md

.PHONY: check-npm
## Check for security issues in node.js dependencies.
check-npm: require-npm
	npm audit

.PHONY: check-poetry
## Lint Poetry configuration for python dependencies.
check-poetry: require-poetry
	poetry check

.PHONY: check-prettier
## Check JSON, Markdown and YAML file formatting.
check-prettier: require-prettier
	prettier --check $(PRETTIER_GLOBS)

.PHONY: check-rebar
## Run static analysis, unit tests, property tests and linting.
check-rebar: require-rebar3
	$(REBAR) dialyzer
	$(REBAR) as test do eunit, proper, lint

.PHONY: check-requirements
## Check that all external requirements are installed.
check-requirements: $(REQUIREMENT_TARGETS)

.PHONY: check-shellcheck
## Lint shell scripts.
check-shellcheck: require-shellcheck
	shellcheck bin/*.bash

.PHONY: check-shfmt
## Check shell script formatting.
check-shfmt: require-shfmt
	shfmt -d $(SHFMT_ARGS) bin/*.bash

.PHONY: check-yamllint
## Lint YAML files.
check-yamllint: require-poetry
	poetry run yamllint -s .

.PHONY: clean
## Delete intermediate files.
clean: require-rebar3
	$(REBAR) clean -a

.PHONY: compile
## Compile all profiles.
compile: require-rebar3
	$(REBAR) compile
	$(REBAR) as test compile

.PHONY: deps
## Install all local dependencies.
deps: deps-rebar3 deps-npm deps-poetry

.PHONY: deps-npm
## Install node.js based dependencies.
deps-npm: require-npm
	npm install

.PHONY: deps-poetry
## Install python based dependencies.
deps-poetry: require-poetry
	poetry install

.PHONY: deps-rebar3
## Install erlang based dependencies.
deps-rebar3: require-rebar3
	$(REBAR) do update, upgrade --all
	$(foreach profile,$(REBAR_PROFILES),$(REBAR) as $(profile) upgrade --all &&) true
# Skip upgrading plugins if this is the first ever build. This is an
# optimization for Dockerfile build times.
ifneq ($(wildcard _build/.),)
	$(REBAR) plugins upgrade --all
	$(foreach profile,$(REBAR_PROFILES),$(REBAR) as $(profile) plugins upgrade --all &&) true
endif

.PHONY: doc
## Build documentation.
doc: $(addsuffix .png,$(basename $(wildcard doc/*.pic)))
	$(REBAR) as chunk edoc
	$(REBAR) as html edoc
	$(REBAR) as markdown edoc
	$(foreach f, README.md doc/README.md, ed --verbose '$(f)' < bin/move-modules-section.ed &&) true
	# Assumes gnu sed for in place editing support.
	$(foreach f, README.md doc/README.md, sed -E --in-place --file bin/fix-external-links.sed '$(f)' &&) true
	ed --verbose README.md < bin/fix-image-path.ed
	$(MAKE) fmt-prettier

doc/%.png: require-pic2plot doc/%.pic
	pic2plot --font-size=0.010 --bitmap-size=4096x4096 --line-width=0.001 -Tpng $< > $@

.SILENT: help
.PHONY: help
## This help screen.
help:
	# Extracts help from the Makefile itself, printing help for any rule
	# which matches the defined regular expression and that has a double
	# hash (##) comment on the line above.
	printf "Available Targets:\n\n"
	awk '/^[a-zA-Z\-_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-18s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' ${MAKEFILE_LIST} | grep --color=auto '^[^:]*'

.PHONY: fmt
## Reformat all files.
fmt: fmt-erlfmt fmt-prettier fmt-shfmt

.PHONY: fmt-erlfmt
## Reformat all erlang files.
fmt-erlfmt: require-rebar3
	$(REBAR) as test fmt --write
	$(REBAR) as test fmt --write elvis.config

.PHONY: fmt-prettier
## Reformat JSON, Markdown and YAML files.
fmt-prettier: require-prettier
	prettier --write $(PRETTIER_GLOBS)

.PHONY: fmt-shfmt
## Reformat shell script files.
fmt-shfmt: require-shfmt
	shfmt -w $(SHFMT_ARGS) bin/*.bash

package-lock.json: require-npm package.json
	npm update

poetry.lock: require-poetry pyproject.toml
	poetry update

rebar.lock: require-rebar3 rebar.config
	$(REBAR) update
	$(REBAR) unlock --all
	$(REBAR) upgrade --all

rebar3:
	curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
	chmod 755 rebar3

.PHONY: $(REQUIREMENT_TARGETS)
$(REQUIREMENT_TARGETS): required=$(patsubst require-%,%,$@)
$(REQUIREMENT_TARGETS): require-%:
	@command -v $(required) > /dev/null || \
		(printf "%s%s%s\n" "$$(tput setaf 3)" '"$(required)" is required, please install.' "$$(tput sgr0)"; exit 1)
