# Legacy toolchain for the pinned rumprun revision, not a deployment image.
FROM ubuntu@sha256:152dc042452c496007f07ca9127571cb9c29697f42acbfad72324b2bb2e43c98

RUN apt-get -o Acquire::Retries=5 update \
    && DEBIAN_FRONTEND=noninteractive apt-get -o Acquire::Retries=5 install -y \
       --no-install-recommends build-essential gcc-x86-64-linux-gnu \
       g++-x86-64-linux-gnu zlib1g-dev qemu-system-x86 ca-certificates curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build
RUN curl -fL --retry 5 \
      https://codeload.github.com/rumpkernel/rumprun/tar.gz/b04d42225a12a6fae57a78a9c1cf23642e46cd00 \
      -o rumprun.tar.gz \
    && curl -fL --retry 5 \
      https://codeload.github.com/rumpkernel/buildrump.sh/tar.gz/9c9b022cb2115734935e50600c867a3bc230b32c \
      -o buildrump.tar.gz \
    && curl -fL --retry 5 \
      https://codeload.github.com/rumpkernel/src-netbsd/tar.gz/b8b951e911a2fc555848a2785a9998bc128530b6 \
      -o src-netbsd.tar.gz \
    && printf '%s\n' \
      '5f473de65b1a652a99fda1f851d6e0e31aa504fcb1f4af5ce104c016ea9a3594  rumprun.tar.gz' \
      'd79d0bedf9c61d58fc0972bb333f249fbe15039b95667e6492398f8fd427a5c1  buildrump.tar.gz' \
      '798764420885aa1d5d887ab50d5c2c974994dba4e12dfae8b2123149712558a5  src-netbsd.tar.gz' \
      | sha256sum -c - \
    && mkdir rumprun \
    && tar -xzf rumprun.tar.gz --strip-components=1 -C rumprun \
    && tar -xzf buildrump.tar.gz --strip-components=1 -C rumprun/buildrump.sh \
    && tar -xzf src-netbsd.tar.gz --strip-components=1 -C rumprun/src-netbsd

WORKDIR /build/rumprun
# Disable optional Git discovery. Sources are hash-verified archives.
ENV CC=x86_64-linux-gnu-gcc CXX=x86_64-linux-gnu-g++ HOST_CC=cc GIT=al-no-git
# GCC 7 diagnoses an old libunwind signed shift; leave the warning visible.
RUN ./build-rr.sh -j8 -d /opt/rumprun hw -- -F CPPFLAGS=-Wno-error=shift-negative-value

COPY runtime-probe.c /probe/runtime-probe.c
RUN cc -std=gnu11 -Wall -Wextra -Werror -O2 -pthread \
      /probe/runtime-probe.c -o /probe/linux-control \
    && /opt/rumprun/bin/x86_64-rumprun-netbsd-gcc \
      -std=gnu11 -Wall -Wextra -Werror -O2 -pthread \
      /probe/runtime-probe.c -o /probe/runtime-probe \
    && /opt/rumprun/bin/rumprun-bake hw_generic \
      /probe/runtime-probe.bin /probe/runtime-probe

COPY artifact-guest.c /probe/artifact-guest.c
RUN /opt/rumprun/bin/x86_64-rumprun-netbsd-gcc \
      -std=gnu11 -Wall -Wextra -Werror -O2 \
      /probe/artifact-guest.c -o /probe/artifact-guest \
    && /opt/rumprun/bin/rumprun-bake hw_generic \
      /probe/artifact-guest.bin /probe/artifact-guest

# No workspace mount, guest disk, NIC, monitor, or privileged device required.
USER 65534:65534
WORKDIR /probe
ENTRYPOINT ["timeout", "5", "qemu-system-x86_64", "-machine", "pc,accel=tcg", "-cpu", "qemu64", "-m", "256", "-net", "none", "-vga", "none", "-display", "none", "-serial", "stdio", "-monitor", "none", "-no-reboot", "-kernel", "/probe/runtime-probe.bin", "-append"]
CMD ["{\"cmdline\":\"probe basic\"}"]
