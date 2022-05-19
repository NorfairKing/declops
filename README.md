# Declops

Declarative ops

## Current state

Still playing around.
Come back no sooner than when CI passes.

To see what's already possible, you can dig into the `integration-tests` directory.

## Goals

* Declarative infrastructure, no commands; only results
* Applicative infrastructure: Infrastructure can depend on previously set up infrastructure
* Self-healing deployment; Only idempotent operations (because they can be retried)
* Built-in infrastructure testing.

Would be nice too:

* Monadic infrastructure: Which infrastructure to set up next can depend on previously set up infrastructure

