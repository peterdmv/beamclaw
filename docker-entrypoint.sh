#!/bin/sh
set -e

# Ensure Mnesia directory exists (first run with fresh volume)
mkdir -p /home/beamclaw/.beamclaw/mnesia

# Fix ownership of bind-mounted directories that Docker may create as root.
# This is the standard Docker entrypoint pattern (cf. PostgreSQL, Redis).
for dir in /tmp/beamclaw-bridges /home/beamclaw/.beamclaw /home/beamclaw/.beamclaw/mnesia; do
    if [ -d "$dir" ]; then
        chown -R beamclaw:beamclaw "$dir" 2>/dev/null || true
    fi
done

# Add beamclaw user to the Docker socket's group so sandbox containers can be
# spawned after privilege drop. This is the standard pattern used by Jenkins,
# GitLab Runner, etc. for sibling container access.
if [ -S /var/run/docker.sock ]; then
    DOCKER_SOCK_GID=$(stat -c '%g' /var/run/docker.sock)
    # Create a group with the socket's GID if it doesn't exist, then add beamclaw
    if ! getent group "$DOCKER_SOCK_GID" >/dev/null 2>&1; then
        addgroup -S -g "$DOCKER_SOCK_GID" docker_host 2>/dev/null || true
    fi
    DOCKER_GROUP=$(getent group "$DOCKER_SOCK_GID" | cut -d: -f1)
    addgroup beamclaw "$DOCKER_GROUP" 2>/dev/null || true
fi

# Ensure UTF-8 locale survives privilege drop via su-exec
export LANG="${LANG:-C.UTF-8}"
export LC_ALL="${LC_ALL:-C.UTF-8}"

# Drop to beamclaw user and exec the OTP release
exec su-exec beamclaw /opt/beamclaw/bin/beamclaw "$@"
