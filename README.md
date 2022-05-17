# Declops

Declarative ops

## Current state

Still playing around.
Come back no sooner than when CI passes.

## Goals

* Declarative infrastructure, no commands; only results
* Monadic infrastructure: Infrastructure can depend on previously set up infrastructure
* Self-healing deployment; Only idempotent operations (because they can be retried)
* Built-in infrastructure testing.
