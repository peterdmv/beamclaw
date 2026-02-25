## ---- Stage 1: Build --------------------------------------------------------
FROM erlang:28-alpine AS builder

RUN apk add --no-cache wget git

# Download rebar3
RUN wget -q https://s3.amazonaws.com/rebar3/rebar3 -O /usr/local/bin/rebar3 \
 && chmod +x /usr/local/bin/rebar3

WORKDIR /build

# Fetch deps first so this layer is cached unless rebar.config / rebar.lock change.
COPY rebar.config rebar.lock ./
RUN rebar3 get-deps

# Build a self-contained OTP release using the docker profile (TUI disabled).
COPY apps/ apps/
COPY config/ config/
RUN rebar3 as docker release
RUN rebar3 escriptize

## ---- Stage 2: Minimal runtime ----------------------------------------------
FROM alpine:3.23

# Erlang runtime C-library dependencies only — no Erlang package needed
# because the OTP release from stage 1 bundles its own ERTS.
# docker-cli enables sandbox sibling containers when Docker socket is mounted.
RUN apk add --no-cache ncurses-libs openssl libstdc++ libgcc docker-cli

# Non-root user for principle-of-least-privilege
RUN addgroup -S beamclaw && adduser -S beamclaw -G beamclaw -h /home/beamclaw

# Copy the self-contained OTP release (includes ERTS, no other Erlang needed)
COPY --from=builder --chown=beamclaw:beamclaw \
     /build/_build/docker/rel/beamclaw /opt/beamclaw

# Copy the CLI escript (high-level commands: agent, skills, pair, doctor, etc.)
COPY --from=builder --chown=beamclaw:beamclaw \
     /build/_build/default/bin/beamclaw /opt/beamclaw/beamclaw-ctl

# Convenience wrapper so `beamclaw-ctl <cmd>` works directly
RUN printf '#!/bin/sh\nexec /opt/beamclaw/bin/beamclaw escript beamclaw-ctl "$@"\n' \
    > /usr/local/bin/beamclaw-ctl && chmod +x /usr/local/bin/beamclaw-ctl

USER beamclaw

# Pre-create data + bridge socket directories so Docker named volumes inherit
# beamclaw:beamclaw ownership instead of defaulting to root:root on first mount.
RUN mkdir -p /home/beamclaw/.beamclaw /tmp/beamclaw-bridges

WORKDIR /opt/beamclaw

# HTTP gateway (configurable via sys.docker.config)
EXPOSE 18800

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
    CMD wget -qO- http://127.0.0.1:18800/health || exit 1

# Run the node in foreground so Docker can capture stdout and manage lifecycle.
# Secrets are injected via -e flags at runtime — never baked into the image.
ENTRYPOINT ["/opt/beamclaw/bin/beamclaw", "foreground"]
