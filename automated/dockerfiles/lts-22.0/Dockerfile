FROM ubuntu:22.04

LABEL maintainer="manny@fpcomplete.com"

ARG GHC_VERSION=9.6.3
ARG LTS_SLUG=lts-22.0
ARG PID1_VERSION=0.1.2.0
ARG STACK_VERSION=2.11.1
ARG CUDA_VERSION=10.0
ARG JVM_PATH=/usr/lib/jvm/java-8-openjdk-amd64
ARG LLVM_PATH=/usr/lib/llvm-9
ARG BOOTSTRAP_COMMIT=56d27e1cfead1a37ff55942a9d2f14c0733459c9

ARG DEBIAN_FRONTEND=noninteractive
ARG VARIANT=build
ARG STACK_ROOT=/home/stackage/.stack

#
# Set encoding to UTF-8 and PATH to find GHC and cabal/stack-installed binaries.
#

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.local/bin:/usr/local/cuda-$CUDA_VERSION/bin:$STACK_ROOT/programs/x86_64-linux/ghc-$GHC_VERSION/bin:$PATH \
    CUDA_PATH=/usr/local/cuda-$CUDA_VERSION \
    CPATH=$JVM_PATH/include:$JVM_PATH/include/linux:$LLVM_PATH/include

#
# Install pre-requisites
#

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        wget netbase ca-certificates g++ gcc libc6-dev libffi-dev libgmp-dev \
        make xz-utils zlib1g-dev git gnupg libtinfo-dev jq && \
    rm -rf /var/lib/apt/lists/*

#
# Use Stackage's docker/*.sh scripts to install system libraries and
# tools required to build any Stackage package.
# Re-installs 'stack' *after* running docker/*.sh since that may have
# installed a different version.
# In the case of 'small' image, just install Stack and GHC.
#

RUN if [ "$VARIANT" != "small" ]; then \
        wget -qO- https://raw.githubusercontent.com/commercialhaskell/stackage/$BOOTSTRAP_COMMIT/docker/01-build-server.sh https://raw.githubusercontent.com/commercialhaskell/stackage/$BOOTSTRAP_COMMIT/docker/02-apt-get-install.sh https://raw.githubusercontent.com/commercialhaskell/stackage/$BOOTSTRAP_COMMIT/docker/03-custom-install.sh https://raw.githubusercontent.com/commercialhaskell/stackage/$BOOTSTRAP_COMMIT/docker/04-cleanup.sh | sed "s/^GHCVER=9.0.1$/GHCVER=$GHC_VERSION/" | GHCVER=$GHC_VERSION bash; \
    fi && \
    wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/bin '*/stack' && \
    if [ "$VARIANT" = "small" ]; then \
        stack setup --resolver ghc-$GHC_VERSION; \
    fi && \
    rm -rf /var/lib/apt/lists/* && \
    cd $STACK_ROOT && \
    find . -type f -not -path "./programs/x86_64-linux/ghc-$GHC_VERSION/*" -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

#
# Configure Stack to use the GHC installed in the Docker image rather than installing its own
#

RUN mkdir /etc/stack/ && \
    echo "system-ghc: true" >/etc/stack/config.yaml

#
# Use 'stack' to install basic Haskell tools like alex, happy, and cpphs. We
# remove most of the STACK_ROOT afterward to save space, but keep the 'share'
# files that some of these tools require.
#

RUN stack --resolver=$LTS_SLUG --local-bin-path=/usr/bin install \
        happy alex cpphs gtk2hs-buildtools hscolour hlint hindent && \
    cd $STACK_ROOT && \
    find . -type f -not -path './snapshots/*/share/*' -and -not -path "./programs/x86_64-linux/ghc-$GHC_VERSION/*" -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

#
# Install 'pid1' init daemon
#

RUN wget -O- "https://github.com/fpco/pid1/releases/download/v$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1

#
# Set up pid1 entrypoint and default command
#

ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["bash"]
