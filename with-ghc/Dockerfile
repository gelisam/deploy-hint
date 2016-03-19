## Dockerfile for building a haskell program
FROM       haskell:7.10.3
MAINTAINER Samuel Gélineau <gelisam@gmail.com>

WORKDIR /root
COPY my-program.cabal /root/my-program.cabal
COPY Setup.hs         /root/Setup.hs
COPY                  src /root/src

RUN cabal update && \
    cabal sandbox init && \
    cabal install --only-dependencies
RUN cabal install && \
    mkdir my-program && \
    cp .cabal-sandbox/bin/my-program my-program/ && \
    mkdir my-program/libs && \
    cp /usr/lib/x86_64-linux-gnu/libgmp.so.10 my-program/libs/ && \
    tar czvf my-program.tar.gz my-program

CMD ["./my-program/my-program"]
