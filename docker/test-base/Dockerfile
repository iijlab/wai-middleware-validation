ARG GHC_VER
FROM haskell:${GHC_VER}

WORKDIR /build
COPY . ./
ARG RESOLVER
RUN cd dummy-project \
    && stack --resolver ${RESOLVER} setup \
    && stack build --only-dependencies \
    && stack test --only-dependencies
