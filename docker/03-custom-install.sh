#!/usr/bin/env bash

set -exuo pipefail

export LANG=C.UTF-8
export DEBIAN_FRONTEND=noninteractive

# Add system dependencies not present in the ubuntu package registry here.
# Please add a comment stating which haskell packages needs it.

locale-gen en_US.UTF-8

# upstream ghc still defaults to ld.gold (though we should really switch to ld.bfd)
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# Install WirePlumber 0.5 for gi-wireplumber and taffybar.
# Ubuntu 24.04 only packages WirePlumber 0.4.
WIREPLUMBER_VER=0.5.14
WIREPLUMBER_TARBALL=wireplumber-${WIREPLUMBER_VER}.tar.gz
(
  cd /tmp \
    && curl -L -o ${WIREPLUMBER_TARBALL} https://gitlab.freedesktop.org/pipewire/wireplumber/-/archive/${WIREPLUMBER_VER}/${WIREPLUMBER_TARBALL} \
    && echo "e91f04cd8cec75d72b8a2aaa7e90b1ba0a5e2094b7a882fc3a29a484a48a87e9  ${WIREPLUMBER_TARBALL}" | sha256sum -c - \
    && tar -xzf ${WIREPLUMBER_TARBALL} \
    && cd wireplumber-${WIREPLUMBER_VER} \
    && meson setup build -Ddoc=disabled -Dsystem-lua=true \
    && meson compile -C build \
    && meson install -C build \
    && ldconfig \
    && cd /tmp \
    && rm -rf wireplumber-${WIREPLUMBER_VER} ${WIREPLUMBER_TARBALL}
)

# # Add JDK to system paths.
# echo "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server" > /etc/ld.so.conf.d/openjdk.conf \
#     && ldconfig

# # Install erlang/otp platform and its dependencies
# ERLANG_DEB_FILE="esl-erlang_21.1-1~ubuntu~bionic_amd64.deb"
# pushd /tmp \
#     && wget https://packages.erlang-solutions.com/erlang/debian/pool/${ERLANG_DEB_FILE} \
#     && (dpkg -i ${ERLANG_DEB_FILE}; apt-get install -yf) \
#     && rm ${ERLANG_DEB_FILE} \
#     && popd

# protoc, for proto-lens-combinators test suite
# Instructions from: https://google.github.io/proto-lens/installing-protoc.html
PROTOC_ZIP=protoc-28.0-linux-x86_64.zip
curl -OL https://github.com/google/protobuf/releases/download/v28.0/$PROTOC_ZIP
sudo unzip -o $PROTOC_ZIP -d /usr bin/protoc
rm -f $PROTOC_ZIP

# Install librdkafka (Apache Kafka C/C++ library)
wget -qO - https://packages.confluent.io/deb/5.2/archive.key | apt-key add -
add-apt-repository "deb https://packages.confluent.io/deb/5.2 stable main"
apt-get update && apt install -y librdkafka-dev

# Install z3, for grisette test suite
Z3_VER=4.13.4
(
  cd /usr/local \
    && wget https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VER}/z3-${Z3_VER}-x64-glibc-2.35.zip \
    && unzip z3-${Z3_VER}-x64-glibc-2.35.zip \
    && rm z3-${Z3_VER}-x64-glibc-2.35.zip \
    && ln -s /usr/local/z3-${Z3_VER}-x64-glibc-2.35/bin/z3 /usr/bin/z3
)

# Install libtorch
curl -OL https://download.pytorch.org/libtorch/cpu/libtorch-shared-with-deps-$LIBTORCH_VERSION%2Bcpu.zip
unzip libtorch-shared-with-deps-$LIBTORCH_VERSION%2Bcpu.zip -d /usr/local/
cp -rf /usr/local/libtorch/lib/* /usr/lib/
cp -rf /usr/local/libtorch/include/* /usr/include/
rm libtorch-shared-with-deps-$LIBTORCH_VERSION%2Bcpu.zip
rm -rf /usr/local/libtorch
