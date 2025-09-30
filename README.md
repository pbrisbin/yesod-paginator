> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/yesod-paginator
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# yesod-paginator


[![Hackage](https://img.shields.io/hackage/v/yesod-paginator.svg?style=flat)](https://hackage.haskell.org/package/yesod-paginator)
[![Stackage Nightly](http://stackage.org/package/yesod-paginator/badge/nightly)](http://stackage.org/nightly/package/yesod-paginator)
[![Stackage LTS](http://stackage.org/package/yesod-paginator/badge/lts)](http://stackage.org/lts/package/yesod-paginator)
[![CI](https://github.com/pbrisbin/yesod-paginator/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/yesod-paginator/actions/workflows/ci.yml)

Handle a database query and/or array-math to paginate a list and produce a page
of items along with a pagination widget for navigating them.

## Usage

See the top-level [module documentation][docs].

[docs]: http://hackage.haskell.org/package/yesod-paginator/docs/Yesod-Paginator.html

## Examples

See the [example](./example/Main.hs). Run it with

```console
stack build --flag yesod-paginator:examples
stack exec yesod-paginator-example
```

## Development & Tests

```console
stack setup
stack build --pedantic --test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
