FROM fpco/pid1:16.04

ENV HOME /home/stackage
ENV LANG en_US.UTF-8

# NOTE: also update debian-bootstrap.sh when cuda version changes
ENV PATH /usr/local/cuda-8.0/bin:/opt/ghc/8.4.1/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV CUDA_PATH /usr/local/cuda-8.0
ENV LD_LIBRARY_PATH=/usr/local/cuda-8.0/lib64:/usr/local/cuda-8.0/nvvm/lib64

ADD debian-bootstrap.sh /tmp/debian-bootstrap.sh
RUN /tmp/debian-bootstrap.sh && rm /tmp/debian-bootstrap.sh
