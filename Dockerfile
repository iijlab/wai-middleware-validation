ARG BASE_IMAGE
FROM ${BASE_IMAGE}

WORKDIR /build
COPY stack.yaml wai-middleware-validation.cabal LICENSE README.md Setup.hs ./
RUN stack setup \
    && stack build --only-dependencies \
    && stack test --only-dependencies
COPY src ./src
COPY test ./test
