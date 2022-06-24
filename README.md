# Declops

Declarative ops

## Current state

Able to deploy local resources statelessly with good testing.

Next steps:

1. Get a local virtualbox deployment with a given nixos configuration set up and passing the current tests.
2. Implement a `--build-only` dry-run that already builds the nixos configs.
3. Try to implement remote providers like AWS EC2 instances.

Please come talk to me if you or your company would like to help out with this project.
Without extra support, this could take ages to get into working shape.

## Goals

* Declarative infrastructure, no commands; only results
* Applicative infrastructure: Infrastructure can depend on previously set up infrastructure
* Self-healing deployment: All successful operations must be idempotent (because they can be retried).
* Success as the exception: Many things can go wrong. Errors must be first-class, explicit, clear, and diagnosable.
* Completely debuggable: Ops is difficult, any attempt at hiding that will end in more confusion rather than less.
* Built-in infrastructure testing.
* Stateless: No local state

Would be nice too:

* Monadic infrastructure: Which infrastructure to set up next can depend on previously set up infrastructure.

## Comparison

|                                            | Declops | [Nixops](https://github.com/NixOS/nixops)  | [Terraform](https://www.terraform.io/) | [Nixinate](https://github.com/MatthewCroughan/nixinate)  | Ansible | Puppet |
|--------------------------------------------|---------|---------|-----------|-----------|-------|-----|
| Declarative                                | ✔       | ✔       | ✔         | ✔         | ✖️     | ✖️   |
| Stateless                                  | ✔       | ✖️       | ✖️         | ✔️         | ✔️     | ✔️   |
| Set up new infrastructure                  | ✔       | ✔       | ✔         | ✖️         | ✔️     | ✔️   |
| Nix for configuration                      | ✔       | ✔       | ✖️         | ✔️         | ✖️     | ✖️   |
| Exists                                     | ✖️       | ✔       | ✔️         | ✔️         | ✔️     | ✔️   |

## Hacking

Declops has been the most challenging testing project I have made so far.

We many different tests with many different requirements, so they are tested as automatically as possible and with the least privileges possible.
The following table is an overview of which tests are run, where you can find them, and where they are run.

| Tests                                      | Location                              | Stack test | Package build | NixOS test | GitHub Actions CI | Manual Nix Script |
|--------------------------------------------|---------------------------------------|------------|---------------|------------|-------------------|-------------------|
| Unit tests                                 | `declops-provider-gen/test`           | ✔          | ✔️             |            | ✔️                 |                   |
| Local provider integration tests           | `declops-provider-local/test`         | ✔          | ✔️             |            | ✔️                 |                   |
| Nix tests (require the `nix` command)      | `nix/nix-test.nix`                    | ✔️          | ✖️             | ✔️          | ✖️                 |                   |
| Deployment integration tests               | `integration-tests/local`             | ✖          | ✖️             | ✔️          | ✖️                 |                   |
| VirtualBox tests (require virtiualisation) | `declops-provider-virtualbox-test`    | ✔          | ✖️             | ✖️          | ✖️                 | ✔                 |
| VirtualBox integration tests               | `integration-tests/virtualisation`    | ✖          | ✖️             | ✖️          | ✖️                 | ✔                 |
