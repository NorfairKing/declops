# Declops

Declarative ops

## Current state

Still playing around.
Come back no sooner than when CI passes.

To see what's already possible, you can dig into the `integration-tests` directory.

## Goals

* Declarative infrastructure, no commands; only results
* Applicative infrastructure: Infrastructure can depend on previously set up infrastructure
* Self-healing deployment: All successful operations must be idempotent (because they can be retried).
* Success as the exception: Many things can go wrong. Errors must be first-class, explicit, clear, and diagnosable.
* Completely debuggable: Ops is difficult, any attempt at hiding that will end in more confusion rather than less.
* Built-in infrastructure testing.

Would be nice too:

* Monadic infrastructure: Which infrastructure to set up next can depend on previously set up infrastructure.
* Stateless version: A mode in which all providers must be locally stateless.

## Hacking

Declops has been the most challenging testing project I have made so far.

We many different tests with many different requirements:

* Unit tests and integration tests that can run inside a regular `nix-build` of a Haskell package without any extra dependencies:
  * The test suite in `declops`
  * The test suite in `declops-provider-gen`
  * The test suite in `declops-provider-local`
* Integration tests that can be run inside a nix build but require the "Virtualisation" setting in the BIOS of the build machine to be turned on.
  * The test suite in `declops-provider-virtualbox`
* Integration tests that need to be able to run `nix` as a command, and can therefore not be run inside a `nix-build`.
  These tests are run in a NixOS test.
  They cannot be run on GitHub actions CI.
  * The test suite in `declops-nix-test` is run in `nix/nix-test.nix`
* Integration tests of local providers that run entire deployments inside a NixOS test.
  They cannot be run on GitHub actions CI.
  * The `integration-tests/local` directory.
