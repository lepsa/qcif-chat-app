# Get all the deps built for both the exe and tests
FROM haskell:9 as build_base

WORKDIR /opt/website

ARG flags=-tls

RUN cabal update
COPY ./QCIF.cabal /opt/website/QCIF.cabal
RUN cabal build -f "$flags" --only-dependencies -j$(nproc)

COPY ./app /opt/website/app
COPY ./src /opt/website/src
COPY ./test /opt/website/test
COPY ./CHANGELOG.md /opt/website/CHANGELOG.md
COPY ./LICENSE /opt/website/LICENSE

# Actually build the main binary
FROM build_base as build

WORKDIR /opt/website

ARG flags=-tls

RUN cabal install -f "$flags" --installdir=. --install-method=copy exe:QCIF

# What actually runs, no haskell compiler stuff
FROM debian:buster as web

COPY ./static /opt/website/static
COPY --from=build /opt/website/QCIF /opt/website/QCIF

WORKDIR /opt/website

EXPOSE 8080

CMD ["/opt/website/QCIF"]

# Run tests
FROM build_base as test

WORKDIR /opt/website

ARG flags=-tls

CMD cabal run -f $flags  QCIF-test