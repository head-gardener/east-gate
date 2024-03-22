# East Gate

Simple set of hooks for exporting XMonad usage metrics via Prometheus.

# Usage

You can use this module with the following in your `xmonad.hs`:

```haskell
import XMonad.Hooks.EastGate

main = xmonad $ ... . withMetrics def . ... $ def{...}
```

This will start a Prometheus exporter, that you can access on port 9532.
You can use the following example `prometheus.yml` to view the metrics:

```yaml
global:
  scrape_interval: 10s
scrape_configs:
- job_name: xmonad
  static_configs:
  - targets:
    - localhost:9532
```

The hooks add support for tracking keybind usage, spawned windows and a couple
other arbitrary counters. All exported metrics are prefixed with `xmonad_`.
