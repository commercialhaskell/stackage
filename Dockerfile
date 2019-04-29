FROM fpco/pid1:18.04

ENV HOME /home/stackage
ENV LANG en_US.UTF-8
ENV GHCVER 8.6.5

# NOTE: also update debian-bootstrap.sh when cuda version changes
ENV PATH /home/stackage/.stack/programs/x86_64-linux/ghc-$GHCVER/bin:/usr/local/cuda-10.0/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV CUDA_PATH /usr/local/cuda-10.0

ADD debian-bootstrap.sh /tmp/debian-bootstrap.sh
RUN /tmp/debian-bootstrap.sh && rm /tmp/debian-bootstrap.sh

# Include file path
ENV CPATH /usr/lib/jvm/java-8-openjdk-amd64/include:/usr/lib/jvm/java-8-openjdk-amd64/include/linux:/usr/lib/llvm-3.7/include
