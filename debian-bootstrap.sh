#!/usr/bin/env bash

# Work in progress: create a list of commands necessary to get Stackage
# up-and-running on a freshly installed Debian-based system (including Ubuntu).

# Quick start:
# wget -O - https://raw.github.com/fpco/stackage/master/debian-bootstrap.sh | bash -ex

# NOTE: Requires that GHC and Cabal are installed and on your PATH. For
# instructions, see:
#    http://www.stackage.org/install

set -exu

mkdir /home/stackage -p

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y software-properties-common

add-apt-repository ppa:hvr/ghc -y
add-apt-repository -y ppa:marutter/rrutter
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
add-apt-repository -y --keyserver hkp://keyserver.ubuntu.com:80 'deb http://download.mono-project.com/repo/debian wheezy main'
add-apt-repository -y --keyserver hkp://keyserver.ubuntu.com:80 'deb http://download.mono-project.com/repo/debian wheezy-apache24-compat main'
add-apt-repository -y --keyserver hkp://keyserver.ubuntu.com:80 'deb http://download.mono-project.com/repo/debian wheezy-libjpeg62-compat main'

GHCVER=8.4.1

apt-get update
apt-get install -y \
    apt-transport-https \
    build-essential \
    cmake \
    curl \
    freeglut3-dev \
    freetds-dev \
    fsharp \
    g++ \
    gawk \
    ghc-$GHCVER \
    ghc-$GHCVER-dyn \
    ghc-$GHCVER-htmldocs \
    ghc-$GHCVER-prof \
    git \
    gnupg \
    gradle \
    hscolour \
    libadns1-dev \
    libaio1 \
    libalut-dev \
    libasound2-dev \
    libblas-dev \
    libbz2-dev \
    libcairo2-dev \
    libclang-3.9-dev \
    libcurl4-openssl-dev \
    libcwiid-dev \
    libdevil-dev \
    libedit-dev \
    libedit2 \
    libfftw3-dev \
    libflac-dev \
    libfreenect-dev \
    libgd2-xpm-dev \
    libgeoip-dev \
    libgirepository1.0-dev \
    libglfw3-dev \
    libglib2.0-dev \
    libglu1-mesa-dev \
    libgmp3-dev \
    libgnutls-dev \
    libgsasl7-dev \
    libgsl0-dev \
    libgtk-3-dev \
    libgtk2.0-dev \
    libgtksourceview-3.0-dev \
    libhidapi-dev \
    libi2c-dev \
    libicu-dev \
    libimlib2-dev \
    libjack-jackd2-dev \
    libjavascriptcoregtk-4.0-dev \
    libjudy-dev \
    liblapack-dev \
    libleveldb-dev \
    liblmdb-dev \
    liblzma-dev \
    libmagic-dev \
    libmagickcore-dev \
    libmagickwand-dev \
    libmarkdown2-dev \
    libmono-2.0-dev \
    libmp3lame-dev \
    libmpfr-dev \
    libmysqlclient-dev \
    libncurses-dev \
    libnfc-dev \
    liboath-dev \
    libnotify-dev \
    libopenal-dev \
    libpango1.0-dev \
    libpcap0.8-dev \
    libpq-dev \
    libre2-dev \
    libsdl1.2-dev \
    libsdl2-dev \
    libsdl2-gfx-dev \
    libsdl2-image-dev \
    libsdl2-mixer-dev \
    libsdl2-ttf-dev \
    libsnappy-dev \
    libsndfile1-dev \
    libsox-dev \
    libsqlite3-dev \
    libssl-dev \
    libsystemd-dev \
    libtagc0-dev \
    libtre-dev \
    libudev-dev \
    libusb-1.0-0-dev \
    libwebkitgtk-3.0-dev \
    libxau-dev \
    libxml2-dev \
    libxrandr-dev \
    libxss-dev \
    libyaml-dev \
    libzip-dev \
    libzstd-dev \
    libzmq3-dev \
    llvm-3.9 \
    locales \
    m4 \
    minisat \
    mono-mcs \
    nettle-dev \
    ninja-build \
    nodejs \
    npm \
    openjdk-8-jdk \
    python-mpltoolkits.basemap \
    python3-matplotlib \
    python3-numpy \
    python3-pip \
    r-base \
    r-base-dev \
    ruby-dev \
    sudo \
    unixodbc-dev \
    wget \
    xclip \
    z3 \
    zip \
    zlib1g-dev

# odbc
curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add -
curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list
apt-get update
ACCEPT_EULA=Y apt-get install msodbcsql17 -y

locale-gen en_US.UTF-8

curl -sSL https://get.haskellstack.org/ | sh

# Put documentation where we expect it
mv /opt/ghc/$GHCVER/share/doc/ghc-$GHCVER/ /opt/ghc/$GHCVER/share/doc/ghc

# llvm-5.0 for GHC (separate since it needs wget)
wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
    && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main" \
    && apt-get update \
    && apt-get install -y llvm-5.0

# llvm-6.0 for llvm-hs (separate since it needs wget)
wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
    && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" \
    && apt-get update \
    && apt-get install -y llvm-6.0

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# GHC requires a specific LLVM version on the system PATH for its LLVM backend.
# This version is tracked here:
# https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM/Installing
#
# GHC 8.4 requires LLVM 5.0 tools (specifically, llc-5.0 and opt-5.0).
update-alternatives --install "/usr/bin/llc" "llc" "/usr/bin/llc-5.0" 50
update-alternatives --install "/usr/bin/opt" "opt" "/usr/bin/opt-5.0" 50

# Made sure a "node" binary is in the path, as well as "nodejs".
# A historical naming collision on Debian means that the binary is called "nodejs",
# but some tools like tsc still expect "node" to exist.
ln -s /usr/bin/nodejs /usr/bin/node

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
ERLANG_VERSION="20.2.2"
ERLANG_DEB_FILE="esl-erlang_${ERLANG_VERSION}-1~debian~jessie_amd64.deb"
pushd /tmp \
    && wget https://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/${ERLANG_DEB_FILE} \
    && (dpkg -i ${ERLANG_DEB_FILE}; apt-get install -yf) \
    && rm ${ERLANG_DEB_FILE} \
    && popd

# Install version 3 of the protobuf compiler.  (The `protobuf-compiler` package only
# supports version 2.)
curl -OL https://github.com/google/protobuf/releases/download/v3.3.0/protoc-3.3.0-linux-x86_64.zip \
  && sudo unzip -o protoc-3.3.0-linux-x86_64.zip -d /usr bin/protoc \
  && rm -f protoc-3.3.0-linux-x84_64.zip

# Install the TensorFlow C API.
curl https://storage.googleapis.com/tensorflow/libtensorflow/libtensorflow-cpu-linux-x86_64-1.1.0.tar.gz > libtensorflow.tar.gz \
    && sudo tar zxf libtensorflow.tar.gz -C /usr \
    && rm libtensorflow.tar.gz \
    && ldconfig

# Install libsodium
curl https://download.libsodium.org/libsodium/releases/LATEST.tar.gz > libsodium.tar.gz \
	&& sudo tar xfz libsodium.tar.gz -C /tmp \
	&& rm libsodium.tar.gz \
	&& cd /tmp/libsodium-stable \
	&& ./configure \
	&& make install

# NOTE: also update Dockerfile when cuda version changes
# Install CUDA toolkit
# The current version can be found at: https://developer.nvidia.com/cuda-downloads
CUDA_PKG=8.0.61-1         # update this on new version
CUDA_VER=${CUDA_PKG:0:3}
CUDA_APT=${CUDA_VER/./-}

pushd /tmp \
    && wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu1604/x86_64/cuda-repo-ubuntu1604_${CUDA_PKG}_amd64.deb \
    && dpkg -i cuda-repo-ubuntu1604_${CUDA_PKG}_amd64.deb \
    && apt-get update -qq \
    && apt-get install -y cuda-drivers cuda-core-${CUDA_APT} cuda-cudart-dev-${CUDA_APT} cuda-cufft-dev-${CUDA_APT} cuda-cublas-dev-${CUDA_APT} cuda-cusparse-dev-${CUDA_APT} cuda-cusolver-dev-${CUDA_APT} \
    && rm cuda-repo-ubuntu1604_${CUDA_PKG}_amd64.deb \
    && export CUDA_PATH=/usr/local/cuda-${CUDA_VER} \
    && export LD_LIBRARY_PATH=${CUDA_PATH}/nvvm/lib64:${LD_LIBRARY_PATH+x} \
    && export LD_LIBRARY_PATH=${CUDA_PATH}/lib64:${LD_LIBRARY_PATH} \
    && export PATH=${CUDA_PATH}/bin:${PATH} \
    && popd

# non-free repo for mediabus-fdk-aac
apt-add-repository multiverse \
    && apt-get update \
    && apt-get install -y nvidia-cuda-dev
