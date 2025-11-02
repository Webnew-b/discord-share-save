# Discord Message Forwarding Bot v0.1.2.0-preview

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
[source]
source = ["111", "222", "333"] # source channels

[target]
channel = "444" # target thread or channel
```

## Environment

```
export DISCORD_SECRET=your_bot_token_here
```

## Next Milestone

- [x] Task 9: Introduce event queue (`TQueue`) to serialize event handling  
- [x] Task 10: Implement rate limiter and “time-stop” mechanism  
- [ ] Task 11: Configuration structure refactor and validation improvements  
- [ ] Task 12: Full Nix environment management and configuration support

## Preview Release

This is a preview version focusing on reliable forwarding.
 Future releases will add event queuing, rate limiting, and error recovery.
