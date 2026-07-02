# syntax=docker/dockerfile:1
#
# Overture build environment. The ATS2 toolchain is bootstrapped from
# the -int- distribution's generated C sources (CBOOT), so a C
# compiler is all it takes; the layer is cached across CI runs.

# ---------- stage 1: the ATS2 toolchain ----------
FROM debian:bookworm-slim AS ats2

RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential libgmp-dev curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

ARG ATS2_VERSION=0.4.2
WORKDIR /opt
RUN curl -fsSL -o ats2.tgz \
      "https://downloads.sourceforge.net/project/ats2-lang/ats2-lang/ats2-postiats-${ATS2_VERSION}/ATS2-Postiats-int-${ATS2_VERSION}.tgz" \
 && tar -xzf ats2.tgz \
 && rm ats2.tgz

WORKDIR /opt/ATS2-Postiats-int-${ATS2_VERSION}
RUN ./configure && make all

ENV PATSHOME=/opt/ATS2-Postiats-int-0.4.2
ENV PATH=/opt/ATS2-Postiats-int-0.4.2/bin:${PATH}

# ---------- stage 2: build the Overture compiler, run the tests ----------
FROM ats2 AS test

WORKDIR /overture
COPY . .
RUN make -C src
RUN tests/run_tests.sh

# ---------- stage 3: the compiler on PATH (default target) ----------
FROM ats2 AS overture

COPY --from=test /overture/src/overture /usr/local/bin/overture
WORKDIR /work
ENTRYPOINT ["overture"]
