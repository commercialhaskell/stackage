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

GHCVER=8.0.2

apt-get update
apt-get install -y \
    build-essential \
    ghc-$GHCVER \
    ghc-$GHCVER-dyn \
    ghc-$GHCVER-prof \
    ghc-$GHCVER-htmldocs \
    hscolour \
    sudo \
    curl \
    freeglut3-dev \
    fsharp \
    git \
    gradle \
    libadns1-dev \
    libaio1 \
    libalut-dev \
    libasound2-dev \
    libblas-dev \
    libbz2-dev \
    libcairo2-dev \
    libclang-3.7-dev \
    libcurl4-openssl-dev \
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
    libicu-dev \
    libimlib2-dev \
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
    libnotify-dev \
    libopenal-dev \
    libpango1.0-dev \
    libpcap0.8-dev \
    libpq-dev \
    libsdl2-dev \
    libsdl2-mixer-dev \
    libsdl2-image-dev \
    libsdl2-gfx-dev \
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
    libzmq3-dev \
    llvm-3.7 \
    locales \
    m4 \
    minisat \
    mono-mcs \
    nettle-dev \
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
    wget \
    xclip \
    z3 \
    zip \
    zlib1g-dev

locale-gen en_US.UTF-8

curl -sSL https://get.haskellstack.org/ | sh

# Put documentation where we expect it
mv /opt/ghc/$GHCVER/share/doc/ghc-$GHCVER/ /opt/ghc/$GHCVER/share/doc/ghc

# Buggy versions of ld.bfd fail to link some Haskell packages:
# https://sourceware.org/bugzilla/show_bug.cgi?id=17689. Gold is
# faster anyways and uses less RAM.
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10

# GHC requires a specific LLVM version on the system PATH for its LLVM backend.
# This version is tracked here:
# https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM/Installing
#
# GHC 8.0 requires LLVM 3.7 tools (specifically, llc-3.7 and opt-3.7).
update-alternatives --install "/usr/bin/llc" "llc" "/usr/bin/llc-3.7" 50
update-alternatives --install "/usr/bin/opt" "opt" "/usr/bin/opt-3.7" 50

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

# llvm-4.0 for llvm-hs (separate since it needs wget)
wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
    && add-apt-repository "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-4.0 main" \
    && apt-get update \
    && apt-get install -y llvm-4.0

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

## non-free repo for mediabus-fdk-aac
#apt-add-repository multiverse \
#    && apt-get update \
#    && apt-get install -y libfdk-aac-dev


################################################################################
# Install opencv.

OPENCV_VERSION="3.2.0"

apt-get install -y \
    cmake \
    pkg-config \
    libjpeg-dev \
    libtiff5-dev \
    libjasper-dev \
    libpng12-dev \
    libavcodec-dev \
    libavformat-dev \
    libswscale-dev \
    libxvidcore-dev \
    libx264-dev \
    libv4l-dev \
    liblapacke-dev \
    libgtk-3-dev \
    libopenblas-dev \
    libhdf5-dev \
    libtesseract-dev \
    libleptonica-dev \
    python3-dev \
    gfortran

# Make a new directory
rm -rf /tmp/opencv-build
mkdir /tmp/opencv-build
cd /tmp/opencv-build

# Download OpenCV
curl -L https://github.com/opencv/opencv/archive/${OPENCV_VERSION}.tar.gz | tar xz
curl -L https://github.com/opencv/opencv_contrib/archive/${OPENCV_VERSION}.tar.gz | tar xz

cd opencv-${OPENCV_VERSION}
mkdir build
cd build
cmake -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=/usr/local \
      -D OPENCV_EXTRA_MODULES_PATH=/tmp/opencv-build/opencv_contrib-${OPENCV_VERSION}/modules

make -j

make install

################################################################################
