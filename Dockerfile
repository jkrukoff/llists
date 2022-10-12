FROM erlang:25
SHELL ["/bin/bash", "-euo", "pipefail", "-c"]
ENV \
	PATH="/opt/app/bin:/opt/poetry/bin:${PATH}" \
	ERL_AFLAGS="+pc unicode -enable-feature maybe_expr"

ARG HADOLINT_VERSION=2.10.0
ARG HADOLINT_CHECKSUM=4816c95243bedf15476d2225f487fc17465495fb2031e1a4797d82a26db83a1edb63e4fed084b80cef17d5eb67eb45508caadaf7cd0252fb061187113991a338
ARG SHFMT_VERSION=3.5.1
ARG SHFMT_CHECKSUM=e70595598cfd823a1c8957c03a71961e2448a7728d7af7740f7a3bbe2ece2f41d11a2c424d8e5644080a0100a92309504c7840808ba9ea5b7fef1852e1be6cf0

RUN \
	mkdir -p /opt/app && \
	mkdir -p /opt/poetry
WORKDIR /opt/app

COPY bin/download-dep.bash ./bin/
# Install build and test dependencies.
# hadolint: for Dockerfile linting.
# nodejs: for installing node.js test dependencies.
# plotutils: for generating documentation images.
# python3-pip: for installing python test dependencies.
# shellcheck: for shell script linting.
# shfmt: for shell script formatting.
#
# We do not use the packaged nodejs, as it's too old for npm to support. This
# could be fixed if the base image updated to a newer debian release.
#
# We ignore the error for not specifying the nodejs version to install, as the
# upstream repository only makes the latest version available. If we pin to a
# specific version we must upadate the pin every time upstream releases.
# hadolint ignore=DL3008
RUN \
	chmod 755 bin/download-dep.bash && \
	(curl -sSL https://deb.nodesource.com/setup_lts.x | bash -) && \
	apt-get update && \
	apt-get install -y --no-install-recommends 'nodejs' 'plotutils=2.6-11' 'python3-pip=20.3.4-4+deb11u1' 'python3-venv=3.9.2-3' 'shellcheck=0.7.1-1+deb11u1' && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	(curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/install-poetry.py | POETRY_HOME=/opt/poetry python3 -) && \
	poetry --version && \
	download-dep.bash hadolint "https://github.com/hadolint/hadolint/releases/download/v${HADOLINT_VERSION}/hadolint-Linux-x86_64" "${HADOLINT_CHECKSUM}" && \
	download-dep.bash shfmt "https://github.com/mvdan/sh/releases/download/v${SHFMT_VERSION}/shfmt_v${SHFMT_VERSION}_linux_amd64" "${SHFMT_CHECKSUM}"

# Copy in all external package manager configuration seperately and download
# all dependencies as own step in order to take better advantage of docker
# caching.
COPY Makefile rebar.config rebar.lock package.json package-lock.json pyproject.toml poetry.lock ./
RUN make --output-sync deps

COPY . ./
RUN make check-requirements compile

ENTRYPOINT ["make"]
