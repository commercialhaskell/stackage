FROM fpco/stack-build:lts-14

# Prime the snapshot so rebuilds are faster
RUN mkdir -p /artifacts
RUN stack update
RUN stack build --no-install-ghc --resolver lts-13.28 --only-dependencies stack amazonka-s3

RUN git clone --depth 1 https://github.com/commercialhaskell/stack && \
    git clone --depth 1 https://github.com/commercialhaskell/curator

COPY stack.yaml .

RUN stack --no-install-ghc install --local-bin-path /artifacts
