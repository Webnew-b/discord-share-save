# Discord Message Forwarding Bot v0.1.3.0-preview

## Overview

First preview release of the Discord multi-channel forwarding bot, written in Haskell.

This version focuses on **stability and configurable message forwarding** between channels and discussion threads.

## Features

- Configurable `source_channels` and `target_id` via `config.toml`
- Automatic environment variable loading for `DISCORD_SECRET`
- Detects messages containing URLs or @Bot mentions
- Forwards messages from multiple source channels to a **fixed discussion thread**
- Prevents self-forward loops (no overlapping between source and target)

## Configuration Example
Create config.toml
``` sh
cp config.toml.example config.toml
```
Configure in confg.toml

```toml
# ==============================================
# Source channel Configuration
# ----------------------------------------------
# Controls how many channel the bot handle
# Input String must be a channel id which was
# obtained in discord.
# ==============================================
[source]
source = ["111", "222", "333"]

# ==============================================
# Source channel Configuration
# ----------------------------------------------
# Controls a channel the bot forward the message
# to.
# Input String must be a channel id which was
# obtained in discord.
# ==============================================

[target]
channel = "444"

# ==============================================
# Rate Limit Configuration
# ----------------------------------------------
# Controls how many requests the bot can send
# within a short period to prevent Discord API bans.
# ==============================================

[rate_limit]
# Maximum duration of each sliding time window (milliseconds)
# For example, 5000 means the rate-limit window resets every 5 seconds.
window_time_max = 5000

# Maximum number of requests allowed within a single time window
# When this limit is reached, further requests are paused
# until the next window begins.
max_request = 20

```

## Environment

```
export DISCORD_SECRET=your_bot_token_here
```

## Next Milestone

- [x] Task 9: Introduce event queue (`TQueue`) to serialize event handling  
- [x] Task 10: Implement rate limiter and “time-stop” mechanism  
- [x] Task 11: Configuration structure refactor and validation improvements  
- [ ] Task 12: Full Nix environment management and configuration support

## Preview Release

This is a preview version focusing on reliable forwarding.
 Future releases will add event queuing, rate limiting, and error recovery.
