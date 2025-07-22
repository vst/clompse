<h1 align="center">
    <div>
        <img alt="clompse" width="320" src="https://github.com/vst/clompse/assets/374793/089a65db-0cc1-49ba-ac8a-62b743910ead" />
    </div>
    <sub>Take a Glimpse at Your Cloud</sub>
    <p></p>
    <div>
        <img alt="GitHub Release" src="https://img.shields.io/github/v/release/vst/clompse?display_name=tag&style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues/vst/clompse?style=for-the-badge">
        <img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues-pr/vst/clompse?style=for-the-badge">
    </div>
</h1>

> [!WARNING]
>
> This is an experimental project that is in its early stages of
> development. Both the functionality and API are subject to change at
> any time.
>
> It is not recommended to use this in production or any other
> critical environment. Use at your own risk.

**clompse** is a command-line tool designed to provide a unified
interface to various cloud providers, helping you manage a registry of
your cloud resources.

It consumes a JSON/YAML formatted configuration file that lists
various profiles where each contains a number of cloud provider API
credentials. It then allows you to query and list cloud resources
across these providers. The output can be displayed in tabular format
on the console or exported to a file in CSV or JSON format.

Currently, it supports listing cloud servers with their firewall
configurations, object storage buckets, DNS zones, and records for the
following cloud service providers:

1. Amazon Web Services
2. DigitalOcean
3. Hetzner

## Motivation

Using cloud services has become common for many individuals and
organizations. As the number of cloud resources grows, it becomes
increasingly difficult to keep track of them. This tool aims to
provide a unified interface to query and list commonly-used cloud
resources across different cloud providers.

While there are many tools and SaaS solutions that offer similar or
more advanced functionality, they are often too complex, too
complicated, or too expensive for individual hackers. This tool is a
more humble alternative to such solutions.

## Non-Motivation

This tool is not intended to be a full-fledged cloud management
tool. It is not a replacement for the cloud providers' own management
consoles or APIs.

It also does not provide functionality to create, update, or delete
cloud resources, nor is such functionality planned: it is a read-only
interface to cloud resources.

## Challenge

Not all cloud providers offer APIs that list resources as conveniently
as others. Additionally, the exposed properties of similar resources
may differ between providers.

To address this, the tool defines common data types for cloud
resources and maps cloud provider-specific data types to these common
types. It is not always possible to map all properties of a cloud
resource to a common data type. In such cases, the tool will assume
some (hopefully) reasonable defaults or omit such properties
altogether.

## Installation

Download and install the statically compiled binary for Linux
(x86_64):

```sh
curl -Lo /tmp/clompse https://github.com/vst/clompse/releases/latest/download/clompse-static-linux-x86_64
sudo install -m 755 /tmp/clompse /usr/local/bin/clompse
```

Or install it into your `nix` profile (replace `<VERSION>` with the
latest version):

```sh
nix profile install --file https://github.com/vst/clompse/archive/v<VERSION>.tar.gz
```

## Configuration

Here is an example configuration file:

```yaml
# yaml-language-server: $schema=https://raw.githubusercontent.com/vst/clompse/main/config_schema.json

name: "Example Configuration"

cloud_profiles:
  - name: "Personal"
    connections:
      - type: "aws"
        value:
          access_key_id: "AKIAIOSFODNN7EXAMPLE"
          secret_access_key: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  - name: "Work - Internal"
    connections:
      - type: "aws"
        value:
          access_key_id: "AKIAIOSFODH7EXAMPLE"
          secret_access_key: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  - name: "Work - Client"
    connections:
      - type: "do"
        value:
          token: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          spaces_access_key_id: "AKIAIOSFODNN7EXAMPLE"
          spaces_secret_access_key: "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
      - type: "hetzner"
        value:
          token: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
      - type: "hetzner"
        value:
          token: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          token_dns: "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
```

## Usage

Command-line parameters should be self-explanatory:

```console
$ clompse --help
clompse - Rainwater Harvesting vX.Y.Z

Usage: clompse (config | server | storage | domains | version) [-v|--version]

  Top Level Commands

Available options:
  -v,--version             Show application version and exit
  -h,--help                Show this help text

Available commands:
  config                   Configuration commands.
  server                   Server commands.
  storage                  Storage commands.
  domains                  DNS commands.
  version                  Show version and build information.

See <https://github.com/vst/clompse> for help and feedback.
```

## Development

Provision Nix shell via `direnv`:

```sh
direnv allow
```

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 clompse -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

## License

Copyright &copy; 2024-2025 Vehbi Sinan Tunalioglu. This work is licensed
under [MIT License].

<!-- REFERENCES -->

[MIT License]: https://opensource.org/license/mit
