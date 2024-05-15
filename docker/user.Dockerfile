FROM ubuntu:22.04

# https://serverfault.com/a/1016972
ARG DEBIAN_FRONTEND="noninteractive"
ENV TZ="America/New_York"
RUN apt-get update && \
    apt-get install -y \
        wget \
        bzip2 \
        unzip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


ARG Z3_VERSION=4.12.5
ARG Z3_ARCH=x64-glibc-2.35
RUN wget \
      https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VERSION}/z3-${Z3_VERSION}-${Z3_ARCH}.zip \
      -O /z3.zip && \
    unzip -j /z3.zip z3-${Z3_VERSION}-${Z3_ARCH}/bin/libz3.so -d /usr/lib/ && \
    rm -f /z3.zip

RUN cd /usr/local && \
    wget -nv --content-disposition \
      "https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle&inline=false" \
      -O c-to-json-bin.tar.bz2 && \
    tar xvf c-to-json-bin.tar.bz2 && \
    rm c-to-json-bin.tar.bz2 && \
    chmod a+xr /usr/local

RUN useradd -m faial
USER faial
WORKDIR /home/faial

COPY <<EOF saxpy.cu
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = a*x[i] + y[i];
}
EOF

COPY <<EOF saxpy-data-race.cu
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = a*x[i] + y[i+1];
}
EOF



