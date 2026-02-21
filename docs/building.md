# Building BeamClaw

## Prerequisites

### Erlang/OTP 28

The recommended way to install Erlang/OTP 28 is via [asdf](https://asdf-vm.com/):

```bash
asdf plugin add erlang
asdf install erlang 28.0
asdf global erlang 28.0
```

Alternatively, use [kerl](https://github.com/kerl/kerl) or your OS package manager, but
confirm the version is OTP 28 (`erl -version`).

### rebar3

Download the self-contained binary:

```bash
wget https://s3.amazonaws.com/rebar3/rebar3 -O ~/bin/rebar3
chmod +x ~/bin/rebar3
```

Or install via a package manager that provides rebar3 ≥ 3.23.

---

## Compile

```bash
rebar3 compile
```

All seven OTP apps (`beamclaw_obs`, `beamclaw_memory`, `beamclaw_tools`, `beamclaw_mcp`,
`beamclaw_core`, `beamclaw_gateway`, `beamclaw_cli`) must compile with zero warnings.

---

## Build the CLI Escript

```bash
rebar3 escriptize
```

Produces a self-contained binary at:

```
_build/default/bin/beamclaw
```

The binary embeds all seven app `.beam` files plus deps in a single zip-based escript.
It requires a compatible OTP installation on the target machine (same major version).

```bash
# Run directly
export OPENROUTER_API_KEY=sk-or-...
_build/default/bin/beamclaw tui

# Or copy to your PATH
cp _build/default/bin/beamclaw ~/bin/
beamclaw doctor
```

---

## Run Tests

```bash
rebar3 eunit                        # all apps
rebar3 eunit --module=bc_scrubber   # single module
```

Expected: 47+ tests passing, 0 failures.

---

## Type Checking

```bash
rebar3 dialyzer
```

Expected: 0 warnings (PLT is built on first run; takes a few minutes).

---

## Lint

```bash
rebar3 lint
```

Uses Elvis rules defined in `elvis.config`. Expected: clean output.

---

## Building an OTP Release (from source)

```bash
rebar3 release
```

The self-contained release is written to:

```
_build/default/rel/beamclaw/
├── bin/
│   └── beamclaw        ← start/stop/attach script
├── erts-*/             ← bundled Erlang runtime (no system Erlang needed)
├── lib/                ← compiled OTP apps + deps
└── releases/
    └── 0.1.0/
        ├── sys.config  ← copied from config/sys.config
        └── vm.args     ← copied from config/vm.args
```

The release bundles its own ERTS (`{include_erts, true}`), so the target machine needs
only the C runtime libraries (glibc / musl + OpenSSL + ncurses).

To start the release:

```bash
export OPENROUTER_API_KEY=sk-or-...
_build/default/rel/beamclaw/bin/beamclaw foreground
```

---

## Building a Docker Image

Requires: Docker 20.10+

```bash
docker build -t beamclaw:latest .
```

The build uses a multi-stage `Dockerfile`:

| Stage | Base image | Purpose |
|---|---|---|
| `builder` | `erlang:28-alpine` | Compile + `rebar3 as docker release` |
| runtime | `alpine:3.21` | Copy self-contained release; no Erlang package needed |

The `docker` rebar3 profile (`rebar3 as docker release`) uses `config/sys.docker.config`
instead of `config/sys.config`. The only difference is that the TUI channel is disabled
(`{tui, #{enabled => false}}`), since stdin is not available in detached containers.

Build-time secrets are **never** needed; the image contains no API keys. Secrets are
injected at `docker run` time via `-e` flags.

Expected image size: < 100 MB.

---

## Creating a Release Tarball

```bash
rebar3 release tar
ls _build/default/rel/beamclaw/releases/0.1.0/*.tar.gz
```

The tarball can be copied to any compatible Linux host and extracted without installing
Erlang.
