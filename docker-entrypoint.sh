#!/bin/sh
set -e

# Fix ownership of bind-mounted directories that Docker may create as root.
# This is the standard Docker entrypoint pattern (cf. PostgreSQL, Redis).
for dir in /tmp/beamclaw-bridges /home/beamclaw/.beamclaw; do
    if [ -d "$dir" ]; then
        chown beamclaw:beamclaw "$dir" 2>/dev/null || true
    fi
done

# Drop to beamclaw user and exec the OTP release
exec su-exec beamclaw /opt/beamclaw/bin/beamclaw "$@"
