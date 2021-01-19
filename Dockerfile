FROM erlang:23
SHELL ["/bin/bash", "-euo", "pipefail", "-c"]
ENV PATH="~/.poetry/bin:${PATH}"

ARG HADOLINT_VERSION=1.19.0
ARG HADOLINT_CHECKSUM=c11736e8fe3d691106e7449efbff112fcb7a628f59ca9681660759d387509229f1c6e16e97ab141bfdd57eac6799387a7f63631fee328e96a6e710d8324e5f13
ARG SHFMT_VERSION=3.2.1
ARG SHFMT_CHECKSUM=5eaad6642ebf5506b5cba3b89a6746cd6262d427b3197dce7a66517dab753f9e626a3a98285afbc2d08a93b4a37156c88a8f56c3e97803450ac2cac34a8939b3

RUN mkdir -p /opt/app
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
# We create a python3 symlink to force poetry to run with python 3.x instead
# of python 2.x which it otherwise finds first. This might cause issues with
# any system utilities which use python, so we do so last.
#
# We ignore the error for not specifying the nodejs version to install, as the
# upstream repository only makes the latest version available. If we pin to a
# specific version we must upadate the pin every time upstream releases.
# hadolint ignore=DL3008
RUN \
	PATH="/opt/app/bin:${PATH}" && \
	(curl -sSL https://deb.nodesource.com/setup_lts.x | bash -) && \
	apt-get update && \
	apt-get install -y --no-install-recommends 'nodejs' 'plotutils=2.6-10' 'python3-pip=18.1-5' 'shellcheck=0.5.0-3' && \
	apt-get clean && \
	rm -rf /var/lib/apt/lists/* && \
	(curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3 -) && \
	download-dep.bash hadolint "https://github.com/hadolint/hadolint/releases/download/v${HADOLINT_VERSION}/hadolint-Linux-x86_64" "${HADOLINT_CHECKSUM}" && \
	download-dep.bash shfmt "https://github.com/mvdan/sh/releases/download/v${SHFMT_VERSION}/shfmt_v${SHFMT_VERSION}_linux_amd64" "${SHFMT_CHECKSUM}" && \
	ln -s /usr/bin/python3 /usr/local/bin/python

# Copy in all external package manager configuration seperately and download
# all dependencies as own step in order to take better advantage of docker
# caching.
COPY Makefile rebar.config rebar.lock package.json package-lock.json pyproject.toml poetry.lock ./
RUN make -j2 --output-sync deps

COPY . ./
RUN make compile

ENTRYPOINT ["make"]
