#!/usr/bin/env bash

set -exuo pipefail

export LANG=C.UTF-8
export DEBIAN_FRONTEND=noninteractive

# Add system dependencies not present in the ubuntu package registry here.
# Please add a comment stating which haskell packages needs it.

locale-gen en_US.UTF-8

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# install ocilib dependencies then build and install ocilib
cd /tmp \
    && wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb \
    && dpkg -i oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb \
    && rm -f oracle-instantclient12.1-basiclite_12.1.0.2.0-2_amd64.deb \
    && wget https://storage.googleapis.com/oracle.fpinsight.com/instantClient/oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb \
    && dpkg -i oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb \
    && rm -f oracle-instantclient12.1-devel_12.1.0.2.0-2_amd64.deb \
    && wget https://github.com/vrogier/ocilib/archive/v4.3.2.tar.gz \
    && tar xvf v4.3.2.tar.gz \
    && cd /tmp/ocilib-4.3.2 \
    && ./configure --with-oracle-import=linkage \
                   --with-oracle-charset=ansi \
                   --with-oracle-headers-path=/usr/include/oracle/12.1/client64 \
                   --with-oracle-lib-path=/usr/lib/oracle/12.1/client64/lib \
    && make \
    && make install \
    && cd \
    && rm -rf /tmp/ocilib-4.3.2 \
    && echo "/usr/local/lib" > /etc/ld.so.conf.d/usr-local.conf \
    && echo "/usr/lib/oracle/12.1/client64/lib" > /etc/ld.so.conf.d/oracle-client.conf \
    && ldconfig

# Add JDK to system paths.
echo "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/" > /etc/ld.so.conf.d/openjdk.conf \
    && ldconfig

# Install erlang/otp platform and its dependencies
ERLANG_DEB_FILE="esl-erlang_21.1-1~ubuntu~bionic_amd64.deb"
pushd /tmp \
    && wget https://packages.erlang-solutions.com/erlang/debian/pool/${ERLANG_DEB_FILE} \
    && (dpkg -i ${ERLANG_DEB_FILE}; apt-get install -yf) \
    && rm ${ERLANG_DEB_FILE} \
    && popd

# Install the TensorFlow C API.
curl https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.1.0.tar.gz > libtensorflow.tar.gz \
    && sudo tar zxf libtensorflow.tar.gz -C /usr \
    && rm libtensorflow.tar.gz \
    && ldconfig

export CLANG_PURE_LLVM_LIB_DIR=/usr/lib/llvm-9/lib;
export CLANG_PURE_LLVM_INCLUDE_DIR=/usr/lib/llvm-9/include;

# protoc, for proto-lens-combinators test suite
# Instructions from: https://google.github.io/proto-lens/installing-protoc.html
PROTOC_ZIP=protoc-3.3.0-linux-x86_64.zip
curl -OL https://github.com/google/protobuf/releases/download/v3.3.0/$PROTOC_ZIP
sudo unzip -o $PROTOC_ZIP -d /usr bin/protoc
rm -f $PROTOC_ZIP


echo /usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server > /etc/ld.so.conf.d/java.conf

# Install librdkafka (Apache Kafka C/C++ library)
wget -qO - https://packages.confluent.io/deb/5.2/archive.key | apt-key add -
add-apt-repository "deb https://packages.confluent.io/deb/5.2 stable main"
apt-get update && apt install -y librdkafka-dev

# Install z3, for grisette test suite
Z3_VER=4.12.4
(
  cd /usr/local \
    && wget https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VER}/z3-${Z3_VER}-x64-glibc-2.35.zip \
    && unzip z3-${Z3_VER}-x64-glibc-2.35.zip \
    && rm z3-${Z3_VER}-x64-glibc-2.35.zip \
    && ln -s /usr/local/z3-${Z3_VER}-x64-glibc-2.35/bin/z3 /usr/bin/z3
)

LIBJWT_VER=1.12.1
(
pushd /tmp \
    && wget https://github.com/benmcollins/libjwt/archive/v${LIBJWT_VER}.zip \
    && unzip v${LIBJWT_VER}.zip \
    && pushd libjwt-${LIBJWT_VER} \
       && autoreconf -fiv \
       && ./configure --disable-valgrind --disable-doxygen-doc --prefix /usr \
       && make \
       && sudo make install \
       && popd \
    && popd
)
