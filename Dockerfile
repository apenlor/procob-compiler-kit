# Base Image: Debian Stable Slim (Bookworm)
FROM debian:stable-slim

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
# - libaio1t64: Asynchronous I/O library (Oracle requirement)
# - build-essential: Necessary for compiling (gcc, make, etc.)
# - unzip: To unzip Oracle files
# - GnuCOBOL Build Deps: libgmp-dev, libdb-dev, libncurses-dev, libxml2-dev, libjson-c-dev
RUN apt-get update && apt-get install -y \
    libaio1t64 \
    build-essential \
    unzip \
    libgmp-dev \
    libdb-dev \
    libncurses-dev \
    libxml2-dev \
    libjson-c-dev \
    && rm -rf /var/lib/apt/lists/*

# --- GnuCOBOL 3.2 Installation (Source) ---
WORKDIR /tmp/gnucobol
COPY resources/gnucobol/gnucobol-3.2.tar.xz .

RUN tar -xf gnucobol-3.2.tar.xz && \
    cd gnucobol-3.2 && \
    ./configure && \
    make && \
    make install && \
    ldconfig && \
    cd /tmp && \
    rm -rf /tmp/gnucobol

WORKDIR /opt/oracle

# Transfer Oracle Instant Client archives
COPY resources/oracle/*.zip .

# 1. Unzip (they merge into a single folder instantclient_XX_XX)
# 2. Delete the zips
# 3. Standardize directory name to 'instantclient'
RUN unzip -o '*.zip' && \
    rm *.zip && \
    mv instantclient* instantclient

# Version-agnostic environment configuration
ENV ORACLE_HOME=/opt/oracle/instantclient
ENV LD_LIBRARY_PATH=$ORACLE_HOME:$LD_LIBRARY_PATH
ENV PATH=$ORACLE_HOME:$ORACLE_HOME/sdk:$PATH

WORKDIR /app

CMD ["/bin/bash"]