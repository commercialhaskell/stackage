FROM ubuntu:12.04

ENV HOME /home/stackage
ENV LANG en_US.UTF-8

RUN mkdir /home/stackage -p
RUN locale-gen en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y software-properties-common python-software-properties
RUN DEBIAN_FRONTEND=noninteractive add-apt-repository ppa:hvr/ghc -y

ADD debian-bootstrap.sh /tmp/debian-bootstrap.sh
RUN DEBIAN_FRONTEND=noninteractive bash /tmp/debian-bootstrap.sh
RUN rm /tmp/debian-bootstrap.sh

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y cabal-install-1.20 ghc-7.8.4

ENV PATH /opt/ghc/7.8.4/bin:/opt/cabal/1.20/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN cabal update
ADD . /tmp/stackage
RUN cd /tmp/stackage && cabal install . hscolour
RUN cp $HOME/.cabal/bin/* /usr/local/bin
RUN rm -rf $HOME/.cabal $HOME/.ghc /tmp/stackage

RUN cd /home/stackage && cabal update && stackage check
