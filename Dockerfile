# ==============================================================================
# BASE STAGE
# Contains common dependencies for both compiler and runner
# ==============================================================================
FROM debian:stable-slim AS base
ENV DEBIAN_FRONTEND=noninteractive

# Install minimal runtime dependencies required by Oracle and GnuCOBOL
RUN apt-get update && apt-get install -y \
    libaio1t64 \
    libgmp10 \
    libdb5.3 \
    libncursesw6 \
    libxml2 \
    libjson-c5 \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install Oracle Instant Client Basic (Runtime)
WORKDIR /opt/oracle
COPY resources/oracle/instantclient-basiclite-linuxx64.zip .
RUN unzip -o '*.zip' && \
    rm -- *.zip && \
    ln -s instantclient_* instantclient

# Configure runtime environment
ENV ORACLE_HOME=/opt/oracle/instantclient
ENV LD_LIBRARY_PATH=$ORACLE_HOME
ENV PATH=$ORACLE_HOME:$PATH

# ==============================================================================
# BUILDER STAGE
# Builds GnuCOBOL from source, creating a full development environment
# ==============================================================================
FROM base AS builder

# Install build-time dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libgmp-dev \
    libdb-dev \
    libncurses-dev \
    libxml2-dev \
    libjson-c-dev \
    && rm -rf /var/lib/apt/lists/*

# Compile GnuCOBOL from source
WORKDIR /tmp/gnucobol
COPY resources/gnucobol/gnucobol-3.2.tar.xz .
# Note: CFLAGS="-O0" is added to prevent GCC segfaults when running under Q-EMU emulation (Apple Silicon)
RUN tar -xf gnucobol-3.2.tar.xz && \
    cd gnucobol-3.2 && \
    ./configure CFLAGS="-O0" && \
    make && \
    make install && \
    ldconfig && \
    cd /tmp && \
    rm -rf /tmp/gnucobol

# ==============================================================================
# COMPILER STAGE (Final)
# The final image for the 'compiler' service. Includes all build tools.
# ==============================================================================
FROM builder AS compiler

# Create symlink for libaio, required by Oracle Precompiler (procob)
RUN ln -s /usr/lib/x86_64-linux-gnu/libaio.so.1t64 /usr/lib/libaio.so.1

# Install Oracle SDK and Precompiler development tools
WORKDIR /opt/oracle
COPY resources/oracle/instantclient-sdk-linuxx64.zip .
COPY resources/oracle/instantclient-precomp-linux.x64-23.26.0.0.0.zip .
RUN unzip -o '*.zip' && rm -- *.zip

# Add SDK to PATH for procob
ENV PATH=$ORACLE_HOME/sdk:$PATH

# Set final WORKDIR and CMD
WORKDIR /app
CMD ["/bin/bash"]


# ==============================================================================
# RUNNER STAGE (Final)
# The final, minimal image for the 'runner' service.
# ==============================================================================
FROM base AS runner

# Copy GnuCOBOL runtime binaries and libraries from the builder stage
COPY --from=builder /usr/local/lib/libcob* /usr/local/lib/
COPY --from=builder /usr/local/bin/cobcrun /usr/local/bin/
RUN ldconfig

# Set final WORKDIR and CMD
WORKDIR /app
CMD ["/bin/bash"]
