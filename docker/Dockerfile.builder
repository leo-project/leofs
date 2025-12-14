# =============================================================================
# LeoFS Builder Image
# =============================================================================
FROM ubuntu:24.04

# Timezone
ENV TZ=Asia/Tokyo

# Fix apt proxy/mirror issues
COPY docker/99fixbadproxy /etc/apt/apt.conf.d/

# Install build dependencies
RUN rm -rf /var/lib/apt/lists/* \
    && apt-get clean \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        autoconf \
        automake \
        libtool \
        cmake \
        git \
        curl \
        wget \
        ca-certificates \
        libncurses5-dev \
        libssl-dev \
        check \
        libsubunit-dev \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /usr/include/machine \
    && ln -s /usr/include/endian.h /usr/include/machine/endian.h

# Install kerl and build Erlang
ENV KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --without-wx --without-odbc"
RUN curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl \
    && chmod +x kerl \
    && mv kerl /usr/local/bin/

# Build and install Erlang/OTP 28
RUN kerl build 28.0 28.0 \
    && kerl install 28.0 /opt/erlang/28.0

# Install rebar3
RUN . /opt/erlang/28.0/activate \
    && curl -O https://s3.amazonaws.com/rebar3/rebar3 \
    && chmod +x rebar3 \
    && mv rebar3 /usr/local/bin/

# Clone and build LeoFS
WORKDIR /build
COPY . /build/

# Build libcutil without tests
RUN . /opt/erlang/28.0/activate \
    && make clean || true \
    && (cd apps/leo_manager && rebar3 compile) \
    && (cd apps/leo_storage && rebar3 compile) \
    && (cd apps/leo_gateway && rebar3 get-deps) \
    && (cd apps/leo_gateway/_build/default/lib/leo_mcerl/c_src \
        && git clone https://github.com/leo-project/libcutil.git \
        && cd libcutil && git checkout 0.5.1 \
        && mkdir -p build && cd build \
        && cmake -DBUILD_TESTING=OFF .. \
        && make cutil \
        && echo 'all: cutil' > Makefile \
        && echo 'cutil:' >> Makefile \
        && echo '	@echo "libcutil already built"' >> Makefile) \
    && (cd apps/leo_gateway/_build/default/lib/leo_dcerl/c_src \
        && git clone https://github.com/leo-project/libcutil.git \
        && cd libcutil && git checkout 0.5.1 \
        && mkdir -p build && cd build \
        && cmake -DBUILD_TESTING=OFF .. \
        && make cutil \
        && echo 'all: cutil' > Makefile \
        && echo 'cutil:' >> Makefile \
        && echo '	@echo "libcutil already built"' >> Makefile) \
    && (cd apps/leo_gateway && rebar3 compile) \
    && make release

# Output packages are in /build/package/
