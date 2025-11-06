# Discord Message Forwarding Bot

## Overview

**Discord Multi-Channel Forwarding Bot (v0.2.1.0-beta)**  
A Haskell-based bot that forwards messages between multiple channels or discussion threads.  
This beta release focuses on **stability, configuration clarity, and graceful shutdown**.

---

## Features

- Configurable `source_channels` and `target_id` via `config.toml`
- Automatic environment variable loading for `DISCORD_SECRET`
- Detects messages containing URLs or @Bot mentions
- Forwards messages from multiple source channels to a **fixed discussion thread**
- Prevents self-forward loops (no overlap between source and target)
- Graceful shutdown with signal handling (`Ctrl-C`)
- Rate-limiting system to prevent API bans

---

## Configuration Example

Create `config.toml`:

```bash
cp config.toml.example config.toml
```

Edit the file to match your channel IDs:

``` toml
# ==============================================
# Source Channel Configuration
# ----------------------------------------------
# Controls how many channels the bot handles.
# Input strings must be Discord channel IDs.
# ==============================================
[source]
sources = ["111", "222", "333"]

# ==============================================
# Target Channel Configuration
# ----------------------------------------------
# Controls where the bot forwards messages to.
# Input string must be a Discord channel ID.
# ==============================================
[target]
channel = "444"

# ==============================================
# Rate Limit Configuration
# ----------------------------------------------
# Prevents API bans by limiting request frequency.
# ==============================================
[rate_limit]
# Duration of each sliding time window (milliseconds)
window_time_max = 5000

# Maximum number of requests allowed per window
max_request = 20
```

## Environment Setup

Set your bot token as an environment variable:

```
export DISCORD_SECRET=your_bot_token_here
```

To start the bot:

```
stack run
```

If using Nix (flake-based):

```
nix develop
stack run
```

## Versioning Policy (`v.x.y.z`)

| Field | Meaning                                 | Example Change                   |
| ----- | --------------------------------------- | -------------------------------- |
| **v** | Refactor version — architecture rewrite | Switch from STM to Effect System |
| **x** | Release stage — preview → beta → stable | 0.1 → 0.2 → 1.0                  |
| **y** | Feature additions                       | Add attachment forwarding        |
| **z** | Bug fixes or small behavior changes     | Fix rate-limit boundary          |

Current version: **v0.2.0.0-beta**

## Development Roadmap

| Task    | Description                                                  | Status      |
| ------- | ------------------------------------------------------------ | ----------- |
| Task 9  | Introduce event queue (`TQueue`) to serialize event handling | completed   |
| Task 10 | Implement rate limiter and “time-stop” mechanism             | completed   |
| Task 11 | Configuration structure refactor and validation improvements | completed   |
| Task 12 | Full Nix environment management and configuration support    | completed   |
| Task 13 | Add automated test suite (Hspec) for core modules            | completed   |
| Task 14 | CI/CD integration (Woodpecker / GitHub Actions)              | In progress |

## Beta Release Notes — v0.2.1.0
- Marked as first fully tested beta release
- Implemented and validated all core modules:
  1. Configuration loading and validation (Config)
  2. Event queue and graceful shutdown (Queue)
  3. Rate limiter with window reset (RateLimit)
- Added automated test suite using Hspec
- Verified config correctness, queue lifecycle, and rate-limit behavior
- Improved developer workflow:
    stack test for local testing

## Next Milestone (v1.0.0.0)

- Full test coverage for queue, rate limiter, and configuration modules
- CI/CD pipeline for continuous validation
- Mark as **Stable Release**

**Maintainer:** Lexon

**Language:** Haskell

**Version:** v0.2.0.0-beta

**License:** BSD-3-Clause
