## Dockerfile for running a haskell program
FROM       debian:jessie
MAINTAINER Samuel Gélineau <gelisam@gmail.com>

WORKDIR /root
COPY my-program.tar.gz /root/my-program.tar.gz

RUN tar xvf my-program.tar.gz

CMD ["./my-program/my-program"]
