FROM ubuntu:24.04

ENV HOME /home/stackage
ENV LANG en_US.UTF-8

ADD docker/01-build-server.sh /tmp/01-build-server.sh
RUN /tmp/01-build-server.sh && rm /tmp/01-build-server.sh

ADD docker/02-apt-get-install.sh /tmp/02-apt-get-install.sh
RUN /tmp/02-apt-get-install.sh && rm /tmp/02-apt-get-install.sh

ADD docker/03-custom-install.sh /tmp/03-custom-install.sh
RUN /tmp/03-custom-install.sh && rm /tmp/03-custom-install.sh

ADD docker/04-cleanup.sh /tmp/04-cleanup.sh
RUN /tmp/04-cleanup.sh && rm /tmp/04-cleanup.sh

# Include file path
ENV CPATH /usr/lib/jvm/java-8-openjdk-amd64/include:/usr/lib/jvm/java-8-openjdk-amd64/include/linux:/usr/lib/llvm-3.7/include
