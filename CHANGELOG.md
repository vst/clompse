# Changelog

## [0.0.7](https://github.com/vst/clompse/compare/v0.0.6...v0.0.7) (2025-12-29)


### Bug Fixes

* **build:** add missing amazonka deps to stack.yaml for static builds ([23c506a](https://github.com/vst/clompse/commit/23c506af846c2514528faf850402d62858cf0b8f))
* **deps:** upgrade nixpkgs to v25.11, bump flake inputs ([df9ddfc](https://github.com/vst/clompse/commit/df9ddfc61b470f3738eaa7cf03b236756563f31a))

## [0.0.6](https://github.com/vst/clompse/compare/v0.0.5...v0.0.6) (2025-11-30)


### Bug Fixes

* **aws:** exclude inaccessible AWS Lightsail regions ([7be4b7d](https://github.com/vst/clompse/commit/7be4b7ddb029f295567a711184cfb3af2186cd59))

## [0.0.5](https://github.com/vst/clompse/compare/v0.0.4...v0.0.5) (2025-02-13)


### Bug Fixes

* **build:** fix GHC version of static builder ([074ff6f](https://github.com/vst/clompse/commit/074ff6f18a5424490c964c8ceee51a70e25b213b))

## [0.0.4](https://github.com/vst/clompse/compare/v0.0.3...v0.0.4) (2025-02-13)


### Features

* attach firewall information to servers ([e79d153](https://github.com/vst/clompse/commit/e79d1535620a08cf56a057711eb2985356d7723c))
* list AWS Lightsail buckets ([7b801dc](https://github.com/vst/clompse/commit/7b801dc2203deec011396df3ca1024171a6ba807))
* list AWS S3 buckets ([bb0a78a](https://github.com/vst/clompse/commit/bb0a78a202a01769ebdb17d7aed854252bcd0a74))
* list DigitalOcean spaces ([0cf913b](https://github.com/vst/clompse/commit/0cf913b31cfe50ef04c51f02be6ac3b9c2396147))
* list DNS records managed on AWS Lightsail ([8a60545](https://github.com/vst/clompse/commit/8a6054501ee21c2ee0de821586ba6f64c91b7185))
* list DNS records managed on AWS Route53 ([118c00e](https://github.com/vst/clompse/commit/118c00eb911844dcdd1227a8cfc451758a8e1f9d))
* list DNS records managed on DigitalOcean ([6a237a6](https://github.com/vst/clompse/commit/6a237a6131c99b36362ddb8bff1eea4d8671e777))
* list DNS records managed on Hetzner ([9239890](https://github.com/vst/clompse/commit/9239890abd0dd99df8a7ecf05daa8c8b69cb0905))
* list domains managed on AWS Lightsail ([bf399b7](https://github.com/vst/clompse/commit/bf399b724dd9d8b6e55d1684c7b80f3803fc5916))
* list domains managed on AWS Route53 ([2e95c66](https://github.com/vst/clompse/commit/2e95c66694eeee77a8cc968cfcdfc2fb428b2e15))
* list domains managed on DigitalOcean ([d4e11ed](https://github.com/vst/clompse/commit/d4e11ed70671e7f3e949da056c960bb64887e8b5))
* list domains managed on Hetzner ([202f034](https://github.com/vst/clompse/commit/202f0340f78d8831e3aeaa836a473f60fd6385ff))


### Bug Fixes

* add AWS EC2 instance memory and storage size ([1f2a773](https://github.com/vst/clompse/commit/1f2a7733e9b6e6f84dcde390d63531e80916db55))
* **deps:** upgrade hetzner library to v0.7.1.0 ([65b4159](https://github.com/vst/clompse/commit/65b415958581a8808486586ee5645c1e2d338cae))
* parse lightsail instance ID from arn value ([a3455c8](https://github.com/vst/clompse/commit/a3455c82619dd3df9d5eadd76da4a4737130b8dd))

## [0.0.3](https://github.com/vst/clompse/compare/v0.0.2...v0.0.3) (2024-05-02)


### Bug Fixes

* **deps:** upgrade table-layout to v1.0.0.0 ([5d2cb91](https://github.com/vst/clompse/commit/5d2cb912f0c6165a86c6a42c9fc4c7bd19de68e5))

## [0.0.2](https://github.com/vst/clompse/compare/v0.0.1...v0.0.2) (2024-04-30)


### Features

* add CSV format to server list output options ([2bc355c](https://github.com/vst/clompse/commit/2bc355c2af51dc6d6684284f9c8010f5346ddf56))
* make server list output information in JSON and tabular format ([050fd60](https://github.com/vst/clompse/commit/050fd603f8695ec9924f222e40e39b9eae71d8fc))
* report various IPv4/6 information for servers ([590cd72](https://github.com/vst/clompse/commit/590cd72df06417ee3f3979f26e1e135f4c48d136))
* run AWS EC2/Lightsail API calls in parallel using a thread pool ([23735d2](https://github.com/vst/clompse/commit/23735d237f9322281d79e6a889803afdf16dbc46))
* run profiles in parallel using a thread pool ([2557d27](https://github.com/vst/clompse/commit/2557d27f3b0fe6c5f050df5b17d1f89300862888))


### Bug Fixes

* use DC location name instead of DC name for Hetzner ([e8b56b8](https://github.com/vst/clompse/commit/e8b56b8122f54690b963e680ddb873563fc40d97))

## 0.0.1 (2024-04-26)


### Features

* add `config print` subcommand ([839ee49](https://github.com/vst/clompse/commit/839ee4924070ab73a341d93d7c36d2cfaa0a484c))
* add list command to dump servers as per given configuration ([ed4b056](https://github.com/vst/clompse/commit/ed4b056fd3c90eb5336b8d164857b05a902cd51e))
* implement configuration model, add `config schema` subcommand ([7228856](https://github.com/vst/clompse/commit/722885616f741739cfbbc7da6aa2249b76e422f3))
* init codebase ([0404745](https://github.com/vst/clompse/commit/0404745be2eba55a2135fa9b7839b15eb06bf248))


### Bug Fixes

* allow multiple cloud provider connections per profile ([1c8daac](https://github.com/vst/clompse/commit/1c8daac4a25e567d9712116ad846555643f72ca6))

## Changelog
