.DEFAULT_GOAL := all
.SHELLFLAGS := -e -o pipefail -c
MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
PATH := node_modules/.bin:$(PATH)
SHELL := bash

DEPS_TARGETS := package-lock.json poetry.lock rebar.lock
PRETTIER_GLOBS := *.json *.md doc/*.md *.yaml
REBAR ?= ${if ${wildcard ./rebar3}, ./rebar3, rebar3}

.PHONY: all
## Build everything.
all: $(DEPS_TARGETS)
	$(MAKE) deps
	$(MAKE) check compile doc

.PHONY: check
## Run all checks and linting.
check: check-test check-erlfmt check-hadolint check-markdownlint check-yamllint check-prettier

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

.PHONY: check-test
## Run the unit test suite.
check-test: require-rebar3
	$(REBAR) as prod dialyzer
	$(REBAR) as test do eunit, proper, lint

.PHONY: check-markdownlint
## Lint Markdown files.
check-markdownlint: require-markdownlint-cli2
	markdownlint-cli2 CHANGELOG.md

.PHONY: check-prettier
## Check JSON, Markdown and YAML file formatting.
check-prettier: require-prettier
	prettier --check $(PRETTIER_GLOBS)

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
	rebar3 update
	rebar3 get-deps
	rebar3 as test get-deps
	rebar3 as markdown get-deps
	rebar3 plugins upgrade rebar3_hex
	for plugin in erlfmt geas_rebar3 rebar3_lint rebar3_proper; do\
		rebar3 as test plugins upgrade $${plugin}; \
	done

.PHONY: doc
## Build documentation.
doc: $(addsuffix .png,$(basename $(wildcard doc/*.pic)))
	$(REBAR) edoc
	$(REBAR) as markdown edoc
	for f in README.md doc/README.md; do \
		ed --verbose "$${f}" < bin/move-modules-section.ed; \
	done
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
fmt: fmt-erlfmt fmt-prettier

.PHONY: fmt-erlfmt
## Reformat all erlang files.
fmt-erlfmt: require-rebar3
	$(REBAR) as test fmt --write
	$(REBAR) as test fmt --write elvis.config

.PHONY: fmt-prettier
## Reformat JSON, Markdown and YAML files.
fmt-prettier: require-prettier
	prettier --write $(PRETTIER_GLOBS)

package-lock.json: require-npm package.json
	npm update

poetry.lock: require-poetry pyproject.toml
	poetry update

rebar.lock: require-rebar3 rebar.config
	$(REBAR) update
	$(REBAR) unlock
	$(REBAR) upgrade

rebar3:
	curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
	chmod 755 rebar3

REQUIREMENTS = $(addprefix require-,hadolint prettier markdownlint-cli2 npm pic2plot poetry rebar3)

.PHONY: $(REQUIREMENTS)
$(REQUIREMENTS): required=$(patsubst require-%,%,$@)
$(REQUIREMENTS): require-%:
	@command -v $(required) > /dev/null || \
		(printf "%s%s%s\n" "$$(tput setaf 3)" '"$(required)" is required, please install.' "$$(tput sgr0)"; exit 1)
