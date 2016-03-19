## Dockerfile for building a haskell program
FROM       haskell:7.10.3
MAINTAINER Samuel Gélineau <gelisam@gmail.com>

## ensure locale is set during build
ENV LANG            C.UTF-8

WORKDIR /root
COPY my-program.cabal /root/my-program.cabal
COPY Setup.hs         /root/Setup.hs
COPY                  src /root/src

RUN cabal sandbox init && \
    cabal install
RUN mkdir my-program && \
    cp .cabal-sandbox/bin/my-program my-program/my-program && \
    tar czvf my-program.tar.gz my-program

CMD ["./my-program/my-program"]
