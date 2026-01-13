# Base Image: Ubuntu 24.04 LTS (Noble Numbat)
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
# - libaio1t64: Asynchronous I/O library (Oracle requirement)
# - build-essential: Necessary for compiling (gcc, make, etc.)
# - unzip: To unzip Oracle files
# - gnucobol: The COBOL compiler
RUN apt-get update && apt-get install -y \
    libaio1t64 \
    build-essential \
    unzip \
    gnucobol \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/oracle

# Transfer Oracle Instant Client archives
COPY oracle-downloads/*.zip .

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