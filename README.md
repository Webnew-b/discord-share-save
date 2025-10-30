# Discord Message Forwarding Bot v0.1.0-preview

## Overview

First preview release of the Discord multi-channel forwarding bot, written in Haskell.

This version focuses on **stability and configurable message forwarding** between channels and discussion threads.

## Features

- Configurable `source_channels` and `target_id` via `config.toml`
- Automatic environment variable loading for `DISCORD_TOKEN`
- Detects messages containing URLs or @Bot mentions
- Forwards messages from multiple source channels to a **fixed discussion thread**
- Prevents self-forward loops (no overlapping between source and target)

## Configuration Example
Configure in confg.toml

```toml
[source]
channels = ["111", "222", "333"] # source channels

[target]
id = "444" # target thread or channel
```

## Environment

```
export DISCORD_TOKEN=your_bot_token_here
```

## Known Limitations

- No rate-limit control yet (429-safe version coming next)
- Events are handled directly (queue model will be added in v0.2)
- No attachment forwarding in this preview

## Next Milestone

- [ ] Task 9: Introduce event queue (`TQueue`) to serialize event handling  
- [ ] Task 10: Implement rate limiter and “time-stop” mechanism  
- [ ] Task 11: Configuration structure refactor and validation improvements  
- [ ] Task 12: Full Nix environment management and configuration support

## Preview Release

This is a preview version focusing on reliable forwarding.
 Future releases will add event queuing, rate limiting, and error recovery.
