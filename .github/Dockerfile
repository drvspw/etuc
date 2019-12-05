# Dockerfile for eutc

FROM erlang:21.3.8.2
MAINTAINER Venkatakumar Srinivasan

LABEL version="1.0"
LABEL description="Image for Circle CI Builds"

USER root

ENV RUSTUP_HOME /opt/rust
ENV CARGO_HOME /opt/rust

# Install rust
RUN mkdir /opt/rust && \
    echo "export RUSTUP_HOME=/opt/rust" >> /etc/profile.d/rust.sh && \
    echo "CARGO_HOME=/opt/rust" >> /etc/profile.d/rust.sh && \
    curl https://sh.rustup.rs -sSf | sh -s -- -y --no-modify-path

RUN cd /usr/local/bin/ 	&& \
	for f in `ls /opt/rust/bin/*`; do echo "ln -s $f $(basename $f)" | /bin/bash; done

# Install base packages
RUN apt-get update  && \
	apt-get install -y --no-install-recommends \
	apt-utils                                  \
	gdebi  && \
	rm -rf /var/lib/apt/lists/*

# Cleanup
RUN apt-get update && \
	rm -rf /var/lib/apt/lists/*
