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

## ---- Stage: uv binary (no Alpine package available) ------------------------
FROM ghcr.io/astral-sh/uv:latest AS uv

## ---- Stage 2: Minimal runtime ----------------------------------------------
FROM alpine:3.23

# Erlang runtime C-library dependencies only — no Erlang package needed
# because the OTP release from stage 1 bundles its own ERTS.
# docker-cli enables sandbox sibling containers when Docker socket is mounted.
# bash/curl/jq: required by bc_tool_bash, bc_tool_curl, bc_tool_jq, finnhub skill.
# python3: required by uv for nano-banana-pro skill scripts.
RUN apk add --no-cache ncurses-libs openssl libstdc++ libgcc docker-cli su-exec \
    bash curl jq python3 rsvg-convert

# uv: Python package runner for nano-banana-pro skill (no Alpine package exists).
COPY --from=uv /uv /usr/local/bin/uv

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

COPY docker-entrypoint.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/docker-entrypoint.sh

# Pre-create directories with correct ownership for non-compose (plain docker run) usage.
# Bind mounts override these at runtime; the entrypoint fixes ownership in that case.
RUN mkdir -p /home/beamclaw/.beamclaw /tmp/beamclaw-bridges && \
    chown beamclaw:beamclaw /home/beamclaw/.beamclaw /tmp/beamclaw-bridges

WORKDIR /opt/beamclaw

# HTTP gateway (configurable via sys.docker.config)
EXPOSE 18800

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
    CMD wget -qO- http://127.0.0.1:18800/health || exit 1

# Start as root; entrypoint fixes bind-mount ownership then drops to beamclaw user.
# Secrets are injected via -e flags at runtime — never baked into the image.
ENTRYPOINT ["docker-entrypoint.sh"]
CMD ["foreground"]
