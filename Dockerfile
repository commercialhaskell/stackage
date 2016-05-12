FROM ubuntu:14.04

ENV HOME /home/stackage
ENV LANG en_US.UTF-8
ENV PATH /opt/ghc/7.10.3/bin:/usr/sbin:/usr/bin:/sbin:/bin

ADD debian-bootstrap.sh /tmp/debian-bootstrap.sh
RUN /tmp/debian-bootstrap.sh && rm /tmp/debian-bootstrap.sh
